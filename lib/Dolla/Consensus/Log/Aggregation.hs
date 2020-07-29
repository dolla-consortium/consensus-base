{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
module Dolla.Consensus.Log.Aggregation
  ( ByBlockOffset (..)
  , ByProposer    (..)
  , ByProposalBroadcaster (..)
  , ByBroadcaster (..)
  , ByVotingRound (..)
  , fromByVotingRoundToByProposer
  , StreamNameTranslatable (..)
  ) where

import           Prelude hiding (round)
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia
import           Data.Aeson
import           Dolla.Common.Offset
import           Dolla.Consensus.Voting.Round
import           Dolla.Common.NodeId
import           Data.Hashable
import Data.Coerce (coerce)
import qualified Data.Binary as B

newtype ByBlockOffset = ByBlockOffset {blockOffset :: Offset}
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON ByBlockOffset

data ByProposer
  = ByProposer
    { blockOffset :: Offset
    , proposerId :: ProposerId }
  deriving (Eq,Show,Hashable,Generic)
  deriving B.Binary
  deriving (ToJSON,FromJSON) via DefaultJSON ByProposer

instance ToJSONKey ByProposer
instance FromJSONKey ByProposer


data ByProposalBroadcaster
  = ByProposalBroadcaster
    { blockOffset :: Offset
    , proposerId :: ProposerId
    , broadcasterId :: BroadcasterId }
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON ByProposalBroadcaster

data ByBroadcaster
  = ByBroadcaster
    { blockOffset :: Offset
    , broadcasterId :: BroadcasterId }
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON ByBroadcaster

fromByVotingRoundToByProposer :: ByVotingRound -> ByProposer
fromByVotingRoundToByProposer ByVotingRound {..} = ByProposer {..}

data ByVotingRound
  = ByVotingRound
    { blockOffset :: Offset
    , proposerId :: ProposerId
    , round :: Round }
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON ByVotingRound

class StreamNameTranslatable aggregation where
  toStreamName :: aggregation -> String

instance StreamNameTranslatable ByBlockOffset where
  toStreamName ByBlockOffset {..} = "block_" ++ show blockOffset
  
instance StreamNameTranslatable ByProposer where
  toStreamName ByProposer {..}  = "block_" ++ show blockOffset ++ "_proposer_" ++ coerce proposerId

instance StreamNameTranslatable ByBroadcaster where
  toStreamName ByBroadcaster {..}  = "block_" ++ show blockOffset ++ "_broadcaster_" ++ coerce broadcasterId

instance StreamNameTranslatable ByProposalBroadcaster where
  toStreamName ByProposalBroadcaster {..}  
    = "block_" ++ show blockOffset ++ "_proposer_" ++ coerce proposerId ++ "_broadcaster_" ++ coerce broadcasterId


instance StreamNameTranslatable ByVotingRound where
  toStreamName ByVotingRound {..}  
    = "block_" ++ show blockOffset ++ "_proposer_" ++ coerce proposerId ++ "_round_" ++ show round

