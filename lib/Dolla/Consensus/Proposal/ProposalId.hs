{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
module Dolla.Consensus.Proposal.ProposalId
  (ProposalId (..)) where

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia
import           Dolla.Common.Offset (Offset)
import           Dolla.Common.NodeId (ProposerId)
import           Data.Hashable (Hashable)

data ProposalId
  =  ProposalId
     { blockOffset :: Offset
     , localBlockOffset :: Offset
     , proposerId :: ProposerId}
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON ProposalId

instance Hashable ProposalId