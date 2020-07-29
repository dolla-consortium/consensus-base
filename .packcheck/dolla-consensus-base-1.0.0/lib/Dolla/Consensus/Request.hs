{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
module Dolla.Consensus.Request
  ( RequestChannel
  , Request (..)
  , RequestRange (..))where

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia
import           Dolla.Consensus.Consortium.Request
import           Dolla.Consensus.Dummy.Client.Request
import           Data.Hashable (Hashable)
import           Dolla.Common.Range (OffsetRange)
import           Dolla.Libraries.LogEngine.Appendable
import           Dolla.Common.UUID.Provider

data RequestChannel 
  = RequestReceived Request
  | FlushRequested  
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON RequestChannel  

data Request
  = ClientReq     ClientRequest
  | ConsortiumReq ConsortiumRequest
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON Request

newtype RequestRange
  = RequestRange { range :: OffsetRange}
  deriving (Eq,Show, Generic) via OffsetRange
  deriving (ToJSON,FromJSON) via OffsetRange

instance Appendable Request where
  getItemName (ClientReq request)  = "ClientRequest." ++ getItemName request
  getItemName (ConsortiumReq request)  = "ConsortiumRequest." ++ getItemName request

instance UUIDProvider Request where
  getUUID (ClientReq request) = getUUID request
  getUUID (ConsortiumReq request) = getUUID request
instance Hashable RequestRange

