{-# LANGUAGE DerivingVia, DeriveGeneric, FlexibleContexts,
  UndecidableInstances, StandaloneDeriving #-}
module Dolla.Consensus.Voting.Estimate (Estimate (..)) where

import           Data.Aeson
import           GHC.Generics

newtype Estimate = Estimate Bool deriving (Eq,Show, Generic , ToJSON,FromJSON) via Bool