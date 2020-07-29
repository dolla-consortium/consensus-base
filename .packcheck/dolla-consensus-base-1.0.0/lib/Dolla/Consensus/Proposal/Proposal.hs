{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
module Dolla.Consensus.Proposal.Proposal
  ( Packet (..)
  , Proposal (..)
  , Envelope (..)
  , Header (..)
  , Footer (..)) where

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia
import           Data.UUID
import           Dolla.Common.NodeId
import           Dolla.Common.Offset
import           Data.Hashable (Hashable)



data Packet request
  = Packet
    { header :: Header
    , body :: [request]
    , footer :: Footer}
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON (Packet request)

data Proposal request
  = Proposal
    { header :: Header
    , body :: [request]}
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON (Proposal request)

data Envelope
  = Envelope
    { header :: Header
    , footer :: Footer}
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON Envelope

newtype Footer = Footer{bodyAndHeaderHash :: UUID}
                   deriving (Eq, Show, Generic)
                   deriving (ToJSON, FromJSON) via DefaultJSON Footer
data Header
  = Header
    {  blockOffset :: Offset
    ,  proposerId :: ProposerId}
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON Header


instance Hashable Envelope  
instance Hashable Header  
instance Hashable Footer






