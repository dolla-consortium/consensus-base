{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Dolla.Consensus.Consortium.Event (Event (..)) where

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia
import           Dolla.Libraries.LogEngine.Appendable
import           Dolla.Common.NodeId
import           Dolla.Common.Network.Core
import           Dolla.Common.Memory.Byte
import           Dolla.Common.Offset (Offset)

data Event = ConsortiumMembershipGranted { nodeId :: NodeId }
           | ConsortiumMembershipRevoked { nodeId :: NodeId }
           | TeamLocated                 { nodeId :: NodeId
                                         , statusServerUrl :: URL
                                         , downloadServerUrl :: URL
                                         , votingBroadcastUrl :: URL
                                         , receptionistUrl :: URL }
           | TeamReLocated               { nodeId :: NodeId
                                         , statusServerUrl :: URL
                                         , downloadServerUrl :: URL
                                         , votingBroadcastUrl :: URL
                                         , receptionistUrl :: URL }
           | ProposalSizeLimited         { size :: Byte  }
           | BlockCreated                {blockOffset :: Offset }
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON Event

instance Appendable Event where
  getItemName ConsortiumMembershipGranted {} = "ConsortiumMembershipGranted"
  getItemName ConsortiumMembershipRevoked {} = "ConsortiumMembershipRevoked"
  getItemName TeamLocated {} = "TeamLocated"
  getItemName TeamReLocated {} = "TeamReLocated"
  getItemName ProposalSizeLimited {} = "ProposalSizeLimited"
  getItemName BlockCreated {} = "BlockCreated"
