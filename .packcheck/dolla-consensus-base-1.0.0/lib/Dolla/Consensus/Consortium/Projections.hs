{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Dolla.Consensus.Consortium.Projections
  ( Location (..)
  , ActiveNodeIds (..)
  , toActiveNodeIds
  , NodeIdsForBlock (..)
  , mapToNodeIdList
  , ConsortiumState (..)
  , MembershipStatus (..)
  , LocationStatus (..)
  , toConsortiumState'
  , initialConsortiumState
  ) where

import           Prelude hiding (log)
import           Dolla.Consensus.Consortium.Event
import           Dolla.Common.Offset
import           Dolla.Common.NodeId
import qualified Data.IntMap.Strict as IM
import qualified Data.HashMap.Strict as HM
import           Dolla.Common.Network.Core
import           Data.Maybe
import Dolla.Common.Memory.Byte (Byte)
import Dolla.Common.Logging.Core
import Control.Monad.State.Strict

import Control.Monad.Reader
import qualified Streamly as S
import qualified Streamly.Internal.Prelude as SIP
import           Dolla.Common.Verification



-- Active Team : Team not revoked and having a location provided
data Location
  = Location
    { statusServerUrl :: URL
    , downloadServerUrl :: URL
    , votingBroadcastUrl :: URL
    , receptionistUrl :: URL
    } deriving (Eq, Show)

data ActiveNodeIds
  = ActiveNodeIds
      { blockInProgress :: Offset
      , locations :: [(NodeId,Location)]
      } deriving Show

data NodeIdsForBlock = NodeIdsForBlock { blockInProgress :: Offset, nodeIds :: [NodeId]} deriving Show

data MembershipStatus = Granted | Revoked deriving (Eq, Show)
data LocationStatus = NoLocation | Located Location deriving (Eq, Show)

data ConsortiumState
  = ConsortiumState
      { blockCreated :: Offset
      , nodeIndexes :: HM.HashMap NodeId Int -- counting disabled ones
      , proposalSizeLimitMaybe :: Maybe Byte
      , nodeStates :: IM.IntMap (NodeId,MembershipStatus,LocationStatus) } deriving Show

toNodeIdList
  :: (MonadIO m , MonadReader r m)
  => ConsortiumState
  -> (r -> (Logger, NodeId))
  -> S.SerialT m Event
  -> S.SerialT m NodeIdsForBlock
toNodeIdList initialState trans = SIP.evalStateT initialState . toNodeIdList' trans . SIP.liftInner

toNodeIdList'
  :: ( MonadIO m
     , MonadReader r m
     , MonadState ConsortiumState m)
  => (r -> (Logger, NodeId))
  -> S.SerialT m Event
  -> S.SerialT m NodeIdsForBlock
toNodeIdList' trans events = do
  ActiveNodeIds {blockInProgress, locations} <- toActiveNodeIds trans events
  return NodeIdsForBlock 
         { nodeIds = fst <$> locations
         , blockInProgress}


toActiveNodeIds
  :: ( MonadIO m
     , MonadReader r m
     , MonadState ConsortiumState m)
  => (r -> (Logger, NodeId))
  -> S.SerialT m Event
  -> S.SerialT m ActiveNodeIds
toActiveNodeIds trans events = do
  ConsortiumState {blockCreated, nodeStates} <- toConsortiumState' trans events
  let locations =
        IM.elems $
        IM.mapMaybe
          (\case
             (nodeId, Granted, Located url) -> Just (nodeId, url)
             _ -> Nothing)
          nodeStates
  return ActiveNodeIds {locations, blockInProgress = nextOffset blockCreated, ..}

initialConsortiumState :: ConsortiumState
initialConsortiumState = ConsortiumState
                               { blockCreated = firstOffset
                               , nodeIndexes = HM.empty
                               , nodeStates = IM.empty
                               , proposalSizeLimitMaybe = Nothing}

mapToNodeIdList
  :: (S.MonadAsync m, MonadReader r m)
  => (r -> (Logger, NodeId))
  -> S.SerialT m Event
  -> S.SerialT m NodeIdsForBlock
mapToNodeIdList = toNodeIdList initialConsortiumState

toConsortiumState'
  :: ( MonadIO m
     , MonadReader r m
     , MonadState ConsortiumState m)
  => (r -> (Logger, NodeId))
  -> S.SerialT m Event
  -> S.SerialT m ConsortiumState
toConsortiumState' trans events = do
  event <- events
  (logger, _) <- asks trans
  log logger DEBUG $ "[projections] received event " ++ show event
  case event of
    BlockCreated {blockOffset = newBlockCreated} -> do
      ConsortiumState {..} <- get
      log logger DEBUG $ "[projections] new consortium state available from Block " ++  show blockCreated ++ " to " ++ show newBlockCreated
      _ <- verifyStrictlyMonotonicallyIncreasingOffsetPropertyOnM blockCreated newBlockCreated
      let newState = ConsortiumState {blockCreated = newBlockCreated, .. }
      put newState
      return newState
    ProposalSizeLimited {size} ->
      SIP.nilM $
      modify'
        (\ConsortiumState {..} ->
           ConsortiumState {proposalSizeLimitMaybe = Just size, ..})
    ConsortiumMembershipGranted {nodeId} ->
      SIP.nilM $
      modify'
        (\ConsortiumState {..} ->
           ConsortiumState
             { nodeIndexes = HM.insert nodeId (HM.size nodeIndexes) nodeIndexes
             , nodeStates =
                 IM.insert
                   (IM.size nodeStates)
                   (nodeId, Granted, NoLocation)
                   nodeStates
             , ..
             })
    ConsortiumMembershipRevoked {nodeId} -> do
      ConsortiumState {..} <- get
      let nodeIndexAsInt =
            fromMaybe
              (error "toConsortiumState inconsistency : nodeId not present")
              (HM.lookup nodeId nodeIndexes)
          (_, _, previousLocationStatus) =
            fromMaybe
              (error "toConsortiumState inconsistency : nodeIndex not present")
              (IM.lookup nodeIndexAsInt nodeStates)
         -- Changing the status of membership
      SIP.nilM $
        put
          ConsortiumState
            { nodeIndexes
            , nodeStates =
                IM.insert
                  nodeIndexAsInt
                  (nodeId, Revoked, previousLocationStatus)
                  nodeStates
            , ..
            }
    TeamLocated {nodeId,..} -> do
      ConsortiumState {..} <- get
      let nodeIndexAsInt =
            fromMaybe
              (error "toConsortiumState inconsistency : nodeId not present")
              (HM.lookup nodeId nodeIndexes)
          (_, previousMembershipStatus, _) =
            fromMaybe
              (error "toConsortiumState inconsistency : nodeIndex not present")
              (IM.lookup nodeIndexAsInt nodeStates)
         -- updating Location
      SIP.nilM $
        put
          ConsortiumState
            { nodeIndexes
            , nodeStates =
                IM.insert
                  nodeIndexAsInt
                  (nodeId, previousMembershipStatus, Located Location {..})
                  nodeStates
            , ..
            }
    TeamReLocated {nodeId,..} -> do
      ConsortiumState {..} <- get
      let nodeIndexAsInt =
            fromMaybe
              (error "toConsortiumState inconsistency : teamId not present")
              (HM.lookup nodeId nodeIndexes)
          (_, previousMembershipStatus, _) =
            fromMaybe
              (error "toConsortiumState inconsistency : nodeIndex not present")
              (IM.lookup nodeIndexAsInt nodeStates)
         -- updating Location
      SIP.nilM $
        put
          ConsortiumState
            { nodeIndexes
            , nodeStates =
                IM.insert
                  nodeIndexAsInt
                  (nodeId, previousMembershipStatus, Located Location{..})
                  nodeStates
            , ..
            }
