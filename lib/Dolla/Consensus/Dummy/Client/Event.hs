{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# OPTIONS_GHC -fno-warn-partial-fields #-}
{-# LANGUAGE DeriveGeneric #-}

module Dolla.Consensus.Dummy.Client.Event (Event (..)) where

import           Data.UUID

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia
import           Dolla.Libraries.LogEngine.Appendable

import Dolla.Common.Offset (Offset)

type Address = UUID

data Event = Input  { source :: Address,destination :: Address,amount :: Integer}
           | Output { source :: Address,destination :: Address,amount :: Integer}
           | BlockCreated  { blockOffset :: Offset }
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON Event

instance Appendable Event where
  getItemName Input {} = "Input"
  getItemName Output {} = "Output"
  getItemName BlockCreated {} = "BlockCreated"

