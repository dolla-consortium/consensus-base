{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
module Dolla.Consensus.Dummy.Client.Request 
  (DollaClientRequest (..)) where

import           Prelude hiding (id)
import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia
import           Data.UUID
import           Dolla.Libraries.LogEngine.Appendable
import           Data.Hashable
import           Dolla.Common.UUID.Provider


type Address = UUID

data DollaClientRequest = SpendMoney {
                          commandId :: UUID,
                          source :: Address,
                          destination :: Address,
                          amount :: Integer }
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON DollaClientRequest

instance  Hashable  DollaClientRequest

instance Appendable DollaClientRequest where
  getItemName SpendMoney {} = "SpendMoney"

instance UUIDProvider DollaClientRequest where
  getUUID = commandId
