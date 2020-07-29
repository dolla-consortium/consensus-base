{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Dolla.Consensus.Log.EventStoreLog
  ( LogIndex (..)
  , getEventStoreLog
  ) where


import           Prelude hiding (round)
import qualified Data.Text as Text

import           Database.EventStore
import           Dolla.Libraries.LogEngine.Instances.EventStore.Settings
import           Dolla.Libraries.LogEngine.Instances.EventStore.EventStoreLog


import Dolla.Consensus.Log.LogNameIndex

getEventStoreLog
  :: Dependencies
  -> LogIndex
  -> EventStoreLog item
getEventStoreLog clientDependencies index
  = EventStoreLog
    { clientDependencies
    , streamName = StreamName $ Text.pack $ getStreamNameFromIndex index }
