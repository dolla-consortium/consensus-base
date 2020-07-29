{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
module Dolla.Consensus.Dummy.Client.Request 
  (ClientRequest (..)) where

import           Prelude hiding (id)
import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia
import           Data.UUID
import           Dolla.Libraries.LogEngine.Appendable
import           Data.Hashable
import           Dolla.Common.UUID.Provider


type Address = UUID

data ClientRequest = SpendMoney {
                          commandId :: UUID,
                          source :: Address,
                          destination :: Address,
                          amount :: Integer }
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON ClientRequest

instance  Hashable  ClientRequest

instance Appendable ClientRequest where
  getItemName SpendMoney {} = "SpendMoney"

instance UUIDProvider ClientRequest where 
  getUUID = commandId
