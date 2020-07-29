{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Dolla.Consensus.Voting.Auxiliary
  ( Auxiliary (..)) where

import           Data.Aeson
import           GHC.Generics

newtype Auxiliary = Auxiliary Bool deriving (Eq,Show, Generic , ToJSON,FromJSON) via Bool



