{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingVia, DeriveGeneric #-}
module Dolla.Consensus.Dummy.Client.Block
  (ClientEventBlock (..))
  where

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia
import           Dolla.Common.Range
import           Data.UUID

data ClientEventBlock
  = ClientEventBlock
    { eventRange :: OffsetRange
    , eventsHash :: UUID}
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON ClientEventBlock