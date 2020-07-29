{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-partial-fields #-}
module Dolla.Consensus.Consortium.Request
  ( ConsortiumRequest (..)) where

import           Prelude hiding (id)
import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia
import           Dolla.Common.Network.Core
import           Data.UUID
import           Data.Hashable
import           Dolla.Common.UUID.Provider
import           Dolla.Libraries.LogEngine.Appendable



data ConsortiumRequest
  = GrantConsortiumMembership { requestId :: UUID,  teamId :: UUID }
  | RevokeConsortiumMembership { requestId :: UUID, teamId :: UUID }
  | LocateTeam { requestId :: UUID,  teamId :: UUID, url :: URL }
  | ReLocateTeam { requestId :: UUID,  teamId :: UUID, url :: URL }
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON ConsortiumRequest

instance Appendable ConsortiumRequest where
  getItemName GrantConsortiumMembership {} = "GrantConsortiumMembership"
  getItemName RevokeConsortiumMembership {} = "RevokeConsortiumMembership"
  getItemName LocateTeam {} = "LocateTeam"
  getItemName ReLocateTeam {} = "ReLocateTeam"

instance UUIDProvider ConsortiumRequest where
  getUUID GrantConsortiumMembership {requestId} = requestId
  getUUID RevokeConsortiumMembership {requestId} = requestId
  getUUID LocateTeam {requestId} = requestId
  getUUID ReLocateTeam {requestId} = requestId



instance  Hashable ConsortiumRequest