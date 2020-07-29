{-# LANGUAGE DerivingVia, DeriveGeneric, FlexibleContexts,
  UndecidableInstances, StandaloneDeriving #-}
module Dolla.Consensus.Voting.Decision (Decision (..),decisionFromBool) where

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia

data Decision = Accepted | Rejected
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON (Decision)


decisionFromBool :: Bool -> Decision
decisionFromBool True = Accepted
decisionFromBool False = Rejected