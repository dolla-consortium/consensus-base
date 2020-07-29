{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Dolla.Team.Consensus.Maestro.Output (Output (..)) where

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia
import           Dolla.Libraries.LogEngine.Appendable
import           Dolla.Consensus.Log.Aggregation
import           Dolla.Consensus.Voting.Vote


data Output
  = ProposalAccepted                 { byProposer    :: ByProposer}
  | ProposalRejectedAndNeverReceived { byProposer    :: ByProposer}
  | ProposalRejected                 { byProposer    :: ByProposer}
  | ProposalReadyForVote             { byProposer    :: ByProposer, vote :: Vote}
  | ConsensusReached                 { byBlockOffset :: ByBlockOffset}
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON Output

instance Appendable Output  where
  getItemName ProposalAccepted {} = "ProposalAccepted"
  getItemName ProposalRejectedAndNeverReceived {} = "ProposalRejectedAndNeverReceived"
  getItemName ProposalRejected {} = "ProposalRejected"
  getItemName ProposalReadyForVote {} = "ProposalReadyForVote"
  getItemName ConsensusReached {} = "ConsensusReached"

