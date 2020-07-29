{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# OPTIONS_GHC -fno-warn-partial-fields #-}
{-# LANGUAGE DeriveGeneric #-}

module Dolla.Consensus.Voting.BinaryMessage
  ( BinaryMessage (..)) where

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia

import           Dolla.Consensus.Voting.Coordinator
import           Dolla.Consensus.Voting.Estimate
import           Dolla.Consensus.Voting.Auxiliary
import           Dolla.Libraries.LogEngine.Appendable
import           Dolla.Consensus.Log.Aggregation


data BinaryMessage
  = AuxiliaryMessage              { byVotingRound :: ByVotingRound, auxiliary :: Auxiliary }
  | CoordinatorPropositionMessage { byVotingRound :: ByVotingRound, coordinatorProposition :: CoordinatorProposition }
  | EstimateMessage               { byVotingRound :: ByVotingRound, estimate :: Estimate }
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON BinaryMessage

instance Appendable BinaryMessage where
  getItemName AuxiliaryMessage {} = "AuxiliaryMessage"
  getItemName CoordinatorPropositionMessage {} = "CoordinatorPropositionMessage"
  getItemName EstimateMessage {} = "EstimateMessage"