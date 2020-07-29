{-# LANGUAGE DerivingVia, DeriveGeneric, FlexibleContexts,
  UndecidableInstances, StandaloneDeriving #-}
module Dolla.Consensus.Voting.Vote (Vote (..)) where

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia

data Vote =  Accept | Reject
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON (Vote)