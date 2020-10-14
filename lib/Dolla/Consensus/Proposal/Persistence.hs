{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Dolla.Consensus.Proposal.Persistence
  ( ProposalRootFolder
  , Location (..)
  , readRaw
  , write
  , getLocalProposalFolder
  , getLocalProposalTemporaryFile
  , transactLocalProposalCreation
  , moveLocalProposalIntoBroadcast
  , waitTillProposalDownloaded
  , waitTillLocalProposalProduced) where


import           Prelude hiding (log,writeFile,readFile,read)
import           Data.Function ((&))
import           Data.ByteString.Lazy
import           Data.Coerce (coerce)
import           System.Directory

import           Control.Monad.IO.Class
import           Control.Monad.Cont
import           Control.Monad.Catch.Pure ( MonadCatch, catchAll)

import           Streamly.FSNotify

import qualified Streamly.Internal.Prelude as S

import           Dolla.Common.NodeId
import           Dolla.Consensus.Proposal.ProposalId
import           Dolla.Consensus.Log.Aggregation
import           Dolla.Common.Logging.Core
import           Dolla.Common.Offset (Offset)


type  ProposalRootFolder = FilePath

data Location
  = Local ProposalRootFolder
  | Broadcast ProposalRootFolder

readRaw
  :: ( MonadIO m )
  => Location
  -> ProposalId
  -> m ByteString
readRaw location proposalId
  = liftIO $ readFile (getFilePath location proposalId)

write
  :: ( MonadIO m)
  => Location
  -> ProposalId
  -> ByteString
  -> m ()
write location proposalId content
  = do 
    liftIO $ createDirectoryIfMissing True (getFolderPath location proposalId)
    let filePath = getFilePath location proposalId
    fileNotExist <- liftIO $ not <$> doesFileExist filePath
    liftIO $ when fileNotExist (liftIO (writeFile filePath content))
  

moveLocalProposalIntoBroadcast
  :: ( MonadIO m)
  => ProposalRootFolder
  -> ProposalId
  -> m ()
moveLocalProposalIntoBroadcast proposalRootFolder proposalId
  = do
  liftIO $ createDirectoryIfMissing True (getFolderPath (Broadcast proposalRootFolder) proposalId)
  liftIO $ copyFile
    (getFilePath (Local proposalRootFolder) proposalId)
    (getFilePath (Broadcast proposalRootFolder) proposalId)


getFolderPath :: Location -> ProposalId -> String
getFolderPath (Local proposalRootFolder) _ =  getLocalProposalFolder proposalRootFolder
getFolderPath (Broadcast proposalRootFolder) ProposalId {..} =  proposalRootFolder ++ "/broadcast/" ++ show blockOffset ++ "/"


getFilePath :: Location -> ProposalId -> String
getFilePath local@ (Local _) proposalId@ProposalId {..} = getFolderPath local proposalId ++ show localBlockOffset ++ ".proposal"
getFilePath broadcast@(Broadcast _) proposalId@ProposalId {..} =  getFolderPath broadcast proposalId ++ coerce proposerId ++ ".downloaded"

getLocalProposalFolder :: ProposalRootFolder -> FilePath
getLocalProposalFolder proposalRootFolder
  = proposalRootFolder ++ "local/"

getLocalProposalTemporaryFile :: ProposalRootFolder -> Offset -> FilePath
getLocalProposalTemporaryFile proposalRootFolder blockOffset
  = getLocalProposalFolder proposalRootFolder ++ show blockOffset ++ ".tmp"

transactLocalProposalCreation
  :: ( MonadIO m)
  => ProposalRootFolder
  -> Offset
  -> m Offset
transactLocalProposalCreation proposalRootFolder blockOffset = do
  liftIO $ renameFile
    (getLocalProposalTemporaryFile proposalRootFolder blockOffset)
    (getLocalProposalFolder proposalRootFolder ++ show blockOffset ++ ".proposal")
  return blockOffset

waitTillProposalDownloaded
  :: ( MonadIO m, MonadCatch m)
  => Logger
  -> ProposalRootFolder 
  -> ByProposer 
  -> m FilePath
waitTillProposalDownloaded logger proposalRootFolder ByProposer {..}
  = catchAll
    (do
     let currentBlockFolder = proposalRootFolder ++ "broadcast/" ++ show blockOffset ++ "/"
         proposalDownloadedFilePath = currentBlockFolder ++ coerce proposerId ++ ".downloaded"
     proposalDownloadedFilePathExist <- liftIO $ doesFileExist proposalDownloadedFilePath
     unless
        proposalDownloadedFilePathExist
        $ liftIO
          $ do
            log logger INFO  $ "Waiting file to be added : " ++ proposalDownloadedFilePath
            (manager,eventStream) <- watchDirectory
                              currentBlockFolder
                              (EventPredicate (\case
                                 Added filepath _ _ | filepath == proposalDownloadedFilePath -> True
                                 Modified filepath _ _ | filepath == proposalDownloadedFilePath -> True
                                 _ -> False))
            S.drain $ eventStream & S.take 1
            stopWatching manager
     log logger INFO  $ "file Added : " ++ proposalDownloadedFilePath
     return proposalDownloadedFilePath)
    (\exception -> error $ "Issue happening in waitTillProposalDownloaded " ++ show exception)

waitTillLocalProposalProduced
  :: ( MonadIO m, MonadCatch m)
  => Logger
  -> ProposalRootFolder
  -> ByBlockOffset
  -> m FilePath
waitTillLocalProposalProduced logger proposalRootFolder ByBlockOffset {..}
  = catchAll
    (do
     let currentBlockFolder = getLocalProposalFolder proposalRootFolder
         proposalProducedFilePath = currentBlockFolder ++ show blockOffset ++ ".proposal"
     proposalDownloadedFilePathExist <- liftIO $ doesFileExist proposalProducedFilePath
     unless
        proposalDownloadedFilePathExist
        $ liftIO
          $ do
            log logger INFO  $ "Waiting file to be added : " ++ proposalProducedFilePath
            (manager,eventStream) <- watchDirectory
                  currentBlockFolder
                  (EventPredicate (\case
                     Added filepath _ _ | filepath == proposalProducedFilePath -> True
                     Modified filepath _ _ | filepath == proposalProducedFilePath -> True
                     _ -> False))
            S.drain $ eventStream & S.take 1
            stopWatching manager
     log logger INFO  $ "file Added : " ++ proposalProducedFilePath
     return proposalProducedFilePath)
    (\exception -> error $ "Issue happening in waitTillLocalProposalProduced " ++ show exception)