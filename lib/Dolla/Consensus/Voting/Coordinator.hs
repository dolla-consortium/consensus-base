{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Dolla.Consensus.Voting.Coordinator
  ( CoordinatorProposition (..) ) where

import           Data.Aeson
import           GHC.Generics

newtype CoordinatorProposition = CoordinatorProposition Bool deriving (Eq,Show, Generic , ToJSON,FromJSON) via Bool


