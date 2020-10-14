{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
module Dolla.Consensus.Request
  ( Request (..)
  , RequestRange (..))where

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia
import           Data.Hashable (Hashable)
import           Dolla.Common.Range (OffsetRange)
import           Dolla.Libraries.LogEngine.Appendable
import           Dolla.Common.UUID.Provider

data Request clientRequest consortiumRequest
  = ClientReq     clientRequest
  | ConsortiumReq consortiumRequest
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON (Request clientRequest consortiumRequest)

newtype RequestRange
  = RequestRange { range :: OffsetRange}
  deriving (Eq,Show, Generic) via OffsetRange
  deriving (ToJSON,FromJSON) via OffsetRange

instance (Appendable clientRequest , Appendable consortiumRequest) 
       => Appendable (Request clientRequest consortiumRequest) where
  getItemName (ClientReq request)  = "ClientRequest." ++ getItemName request
  getItemName (ConsortiumReq request)  = "ConsortiumRequest." ++ getItemName request

instance (UUIDProvider clientRequest , UUIDProvider consortiumRequest) 
       => UUIDProvider (Request clientRequest consortiumRequest) where
  getUUID (ClientReq request) = getUUID request
  getUUID (ConsortiumReq request) = getUUID request
instance Hashable RequestRange

