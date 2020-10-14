{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Dolla.Consensus.Log.LogNameIndex
  ( LogIndex (..)
  , getStreamNameFromIndex) where

import           Prelude hiding (round)
import           Text.InterpolatedString.Perl6 (qc)
import           Dolla.Consensus.Log.Aggregation



data LogIndex
    -- Proposing
  = ProposingPackagingOutputLog
  -- Proposal Broadcast
  | PBSyncedProposalMessageLog          ByProposalBroadcaster
  | PBSyncedProposalMessageMergedLog    -- Todo : Make the aggregation byProposer instead of current at large
  | PBAlgorithmInput                    ByProposer
  | PBAlgorithmOutput                   ByProposer
  | PBAlgorithmOutputMergedLog
  -- Maestro
  | MaestroInputLog                      ByBlockOffset
  | MaestroOutputLog                     ByBlockOffset
  | MaestroOutputMergedLog
  -- Orchestrator
  | OrchestratorLifeCycleSPInputLog      ByBlockOffset
  | OrchestratorPersistedInputLog        ByProposer
  | OrchestratorPersistedOutputLog       ByProposer
  | OrchestratorPersistedOutputMergedLog
  | OrchestratorTimedOutOutputLog        ByProposer
  | OrchestratorTimedOutOutputMergedLog
  -- Binary Broadcast
  | BBOutGoingMessageLog                 ByBlockOffset
  | BBSyncedMessageLog                   ByBroadcaster
  | BBSyncedMessageMergedLog
  | BBAlgorithmLifeCycleSPInputLog       ByBlockOffset
  | BBAlgorithmInputLog                  ByVotingRound
  | BBAlgorithmOutputLog                 ByVotingRound
  | BBAlgorithmOutputMergedLog
  -- Transaction
  | TransactionOrderingInputLog          ByBlockOffset
  | TransactionMergingInputLog           ByBlockOffset

  -- Requests
  | LocalRequestLog

  -- Ledgers
  | ClientMergedRequestLog
  | ClientEventLog

  | ConsortiumMergedRequestLog
  | ConsortiumEventLog


getStreamNameFromIndex :: LogIndex -> String
getStreamNameFromIndex
 =
  \case
    -- Proposing
    ProposingPackagingOutputLog                  -> [qc|proposing_packaging_output|]
    -- Proposal Broadcast                        
    PBSyncedProposalMessageLog aggregation       -> [qc|proposal_broadcast_synced_message|] ++ applyByCategoryProjection  ++ toStreamName aggregation
    PBSyncedProposalMessageMergedLog             -> [qc|$ce-proposal_broadcast_synced_message|]
    PBAlgorithmInput aggregation                 -> [qc|proposal_broadcast_algorithm_input_|] ++ toStreamName aggregation
    PBAlgorithmOutput aggregation                -> [qc|proposal_broadcast_algorithm_output|] ++ applyByCategoryProjection  ++ toStreamName aggregation
    PBAlgorithmOutputMergedLog                   -> [qc|$ce-proposal_broadcast_algorithm_output|]
    -- Maestro                                   
    MaestroInputLog  aggregation                 -> [qc|maestro_input_|] ++ toStreamName aggregation
    MaestroOutputLog aggregation                 -> [qc|maestro_output|] ++ applyByCategoryProjection  ++ toStreamName aggregation
    MaestroOutputMergedLog                       -> [qc|$ce-maestro_output|]
    -- Orchestrator                              
    OrchestratorLifeCycleSPInputLog aggregation  -> [qc|voting_orchestrator_lifecycle_|] ++ toStreamName aggregation
    OrchestratorPersistedInputLog aggregation    -> [qc|voting_orchestrator_input_|] ++ toStreamName aggregation
    OrchestratorPersistedOutputLog aggregation   -> [qc|voting_orchestrator_output|] ++ applyByCategoryProjection  ++ toStreamName aggregation
    OrchestratorPersistedOutputMergedLog         -> [qc|$ce-voting_orchestrator_output|]
    OrchestratorTimedOutOutputLog aggregation    -> [qc|voting_orchestrator_timedout|] ++ applyByCategoryProjection  ++ toStreamName aggregation
    OrchestratorTimedOutOutputMergedLog          -> [qc|$ce-voting_orchestrator_timedout|]
    -- Binary Broadcast                          
    BBOutGoingMessageLog aggregation             -> [qc|binary_broadcast_outgoing_message_|] ++ toStreamName aggregation
    BBSyncedMessageLog aggregation               -> [qc|binary_broadcast_synced_message|] ++ applyByCategoryProjection  ++ toStreamName aggregation
    BBSyncedMessageMergedLog                     -> [qc|$ce-binary_broadcast_synced_message|]
    BBAlgorithmLifeCycleSPInputLog aggregation   -> [qc|binary_broadcast_lifecycle_|] ++ toStreamName aggregation
    BBAlgorithmInputLog aggregation              -> [qc|binary_broadcast_input_|] ++ toStreamName aggregation
    BBAlgorithmOutputLog aggregation             -> [qc|binary_broadcast_output|] ++ applyByCategoryProjection ++ toStreamName aggregation
    BBAlgorithmOutputMergedLog                   -> [qc|$ce-binary_broadcast_output|]
    -- Transaction
    TransactionOrderingInputLog  aggregation     -> [qc|transaction_ordering_input_|] ++ toStreamName aggregation
    TransactionMergingInputLog   aggregation     -> [qc|transaction_merging_input_|] ++ toStreamName aggregation
    -- Requests
    LocalRequestLog                              -> [qc|request|]
    -- Ledgers
    ClientMergedRequestLog                       -> [qc|merged_client_request|]
    ClientEventLog                               -> [qc|client_event|]

    ConsortiumMergedRequestLog                   -> [qc|merged_consortium_request|]
    ConsortiumEventLog                           -> [qc|consortium_event|]

applyByCategoryProjection :: String
applyByCategoryProjection = "-"


