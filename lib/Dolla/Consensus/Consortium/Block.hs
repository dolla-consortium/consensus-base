{-#  LANGUAGE
    DuplicateRecordFields
  , DerivingVia
  , DeriveGeneric
  , FlexibleContexts
  , UndecidableInstances #-}
module Dolla.Consensus.Consortium.Block
  (ConsortiumEventBlock (..))
  where

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia
import           Dolla.Common.Range
import           Data.UUID

data ConsortiumEventBlock
  = ConsortiumEventBlock
    { eventRange :: OffsetRange
    , eventsHash :: UUID}
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON ConsortiumEventBlock