{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Dolla.Consensus.Voting.TimedOut
  ( TimedOut (..)) where

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia
import           Dolla.Libraries.LogEngine.Appendable

import           Dolla.Consensus.Log.Aggregation (ByVotingRound)

data TimedOut =
        CoordinatorPropositionPropagationWaitTimedOut {byVotingRound :: ByVotingRound}
      | AuxiliaryMessagePropagationWaitTimedOut {byVotingRound :: ByVotingRound}
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON TimedOut

instance Appendable TimedOut where
  getItemName CoordinatorPropositionPropagationWaitTimedOut {} = "CoordinatorPropositionPropagationWaitTimedOut"
  getItemName AuxiliaryMessagePropagationWaitTimedOut {} = "AuxiliaryMessagePropagationWaitTimedOut"
