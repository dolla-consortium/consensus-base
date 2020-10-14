{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
{-# LANGUAGE RecordWildCards #-}

module Dolla.Consensus.Maestro.ESMerger (loadMaestroInputProjection) where

import           Prelude hiding (log)
import           Data.Coerce (coerce)
import qualified Dolla.Libraries.LogEngine.Instances.EventStore.Settings as EventStore
import           Dolla.Libraries.LogEngine.Instances.EventStore.Projection.Client
import           Text.InterpolatedString.Perl6 (qc)
import           Dolla.Common.NodeId
import           Control.Monad.Reader (ReaderT,ask, withReaderT)
import           Dolla.Common.Logging.Core
import           Dolla.Libraries.LogEngine.Instances.EventStore.Projection.Definition
import           Dolla.Consensus.Log.LogNameIndex


-- | User Defined Projection loaded into the event store for dispatching RB Broadcast Merged output and Result of Voting
--   events into the proper input stream for the maestro
--    * for more details about event store projection : https://eventstore.org/docs/projections/api/index.html
loadMaestroInputProjection :: ReaderT (NodeId, EventStore.Dependencies) IO ()
loadMaestroInputProjection = do
  (nodeId , EventStore.Dependencies {..}) <- ask
  let projectionName = coerce nodeId ++ "_maestro_input"
      proposalBroadcastAlgorithmOutputMergedStreamName = getStreamNameFromIndex PBAlgorithmOutputMergedLog
      orchestratorPersistedOutputMergedStreamName = getStreamNameFromIndex OrchestratorPersistedOutputMergedLog
      body = [qc| options(\{
                         reorderEvents: false,
                         processingLag: 0
                     })
                     fromStreams ([ '{proposalBroadcastAlgorithmOutputMergedStreamName}'
                                  , '{orchestratorPersistedOutputMergedStreamName}'])
                     .when(\{
                         $any : function(s,e)\{
                           function getOutputStream(blockOffset) \{
                                return "maestro_input_block_" + blockOffset
                           }
                           if (e.eventType == "ProposalDelivered" ) \{
                            var messageJson = JSON.parse(e.bodyRaw)
                            linkTo ( getOutputStream(messageJson.byProposer.blockOffset)
                                   , e
                                   , \{})
                          }
                          if (e.eventType == "ProposalAcceptanceDecided" ) \{
                            var proposalAcceptanceDecided = JSON.parse(e.bodyRaw)
                            emit ( getOutputStream (proposalAcceptanceDecided.byProposer.blockOffset)
                                 , "DecisionDelivered"
                                 , \{ "tag": "DecisionDelivered",
                                      "byProposer": proposalAcceptanceDecided.byProposer,
                                      "decision": proposalAcceptanceDecided.decision}
                                 , \{});
                          } // if
                         } // $any
                     })
                     .outputState()|]

  log logger DEBUG "Loading Projection for Maestro Input  "
  -- TODO : Verified if the right projection version is loaded 
  result <- withReaderT snd $ createContinuousProjection
                              (ProjectionName projectionName)
                              (ProjectionBody body)
                              (ProjectionEnabled True)
                              (ProjectionEmitting True)
                              (ProjectionTrackEmittedStreams True)
  log logger DEBUG $ "Projection loaded : " ++ show result
  return ()

