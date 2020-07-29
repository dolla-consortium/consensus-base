{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Dolla.Consensus.Consortium.ProjectionsSpec
  ( spec
  ) where

import Control.Monad
import Control.Monad.Morph
import Control.Monad.State.Strict
import Control.Monad.Reader
import qualified Data.IntMap.Strict as IM
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Instances ()
import Test.Hspec
import Test.Hspec.QuickCheck

import Dolla.Consensus.Consortium.Event as Event
import Dolla.Consensus.Consortium.Projections as Projections

import Dolla.Common.Memory.Byte
import Dolla.Common.Offset
import Dolla.Common.Logging.Core
import Dolla.Common.Network.Core
import Dolla.Common.NodeId
import Dolla.Common.Streamly
import qualified Streamly as S
import qualified Streamly.Prelude as SP


-- |Generate an event based on the current 'ConsortiumState'. We can
-- generate 'Event's completely arbitrarily but that wouldn't be
-- terribly useful as most of those generated events would be invalid
-- and will result in error states.
generateEvents :: ConsortiumState -> Gen Event
generateEvents cs@ConsortiumState {blockCreated = oldBlockOffset} =
  oneof $
  [ return $ BlockCreated (nextOffset oldBlockOffset)
  , ProposalSizeLimited . Byte <$> arbitrary
  , ConsortiumMembershipGranted . NodeId <$> arbitrary
  ]
  ++ genConsortiumMembershipRevoked
  ++ genTeamLocated
  ++ genTeamReLocated
  where
    genTis =
      let tis = HM.keys (nodeIndexes cs)
       in if (Prelude.null tis)
            then []
            else [elements tis]
    arbitraryUrlPort = URLPort <$> arbitrary      
    arbitraryUrl = URL <$> arbitrary <*> arbitraryUrlPort <*> arbitrary
    genTeamLocationEvent p = do
      genTi <- genTis
      return $ p <$> genTi <*> arbitraryUrl <*> arbitraryUrl <*> arbitraryUrl <*> arbitraryUrl
    genConsortiumMembershipRevoked =
      (ConsortiumMembershipRevoked `fmap`) <$> genTis
    genTeamLocated = genTeamLocationEvent TeamLocated
    genTeamReLocated = genTeamLocationEvent TeamReLocated

generateEventsStream ::
     MonadIO m
  => S.SerialT (StateT Projections.ConsortiumState m) Event.Event
generateEventsStream =
  SP.take 100 $
  repeatMSerial $ do
    cs <- get
    liftIO $ generate $ generateEvents cs

-- |Helper function that asserts that a 'Maybe a' is 'Just a'
assertIsJust :: Monad m => Maybe a -> PropertyM m ()
assertIsJust x = assert (isJust x)

-- |Assert whether a new block has been created with the given blockOffset.
assertNewBlockCreated
  :: Monad m => Offset -> StateT Projections.ConsortiumState (PropertyM m) ()
assertNewBlockCreated blockCreatedOffset = do
  ConsortiumState {blockCreated = newBlockCreatedOffset} <- get
  lift $ assert (blockCreatedOffset == newBlockCreatedOffset)

-- |Assert whether the proposal size limit has been updated in the state.
assertProposalSizeLimited
  :: Monad m => Byte -> StateT Projections.ConsortiumState (PropertyM m) ()
assertProposalSizeLimited receivedProposalSize = do
  ConsortiumState {proposalSizeLimitMaybe = newProposalSize} <- get
  lift $ do
    -- We have just received a ProposalSizeLimited event, this can not
    -- be 'Nothing'.
    assertIsJust newProposalSize
    -- The use of fromJust should be safe here as we are checking
    -- whether it is a 'Just' value before usage.
    assert (receivedProposalSize == fromJust newProposalSize)

-- |Assert if membership has been granted for the given nodeId.
assertMembershipGranted
  :: Monad m => NodeId -> StateT Projections.ConsortiumState (PropertyM m) ()
assertMembershipGranted nodeId = do
  ConsortiumState {nodeIndexes, nodeStates} <- get
  lift $ do
    assert (HM.member nodeId nodeIndexes)
    let v = IM.lookup (IM.size nodeStates - 1) nodeStates
    assert (isJust v)
    assert (fromJust v == (nodeId, Granted, NoLocation))

-- |Membership can be revoked for nodes that are already present in the
-- ConsortiumState's nodeIndexes HashMap. It is an error to receive a
-- 'ConsortiumMembershipRevoked' event for a nodeId that is not in the
-- nodeIndexes HashMap.
assertMembershipRevoked
  :: Monad m => NodeId -> StateT Projections.ConsortiumState (PropertyM m) ()
assertMembershipRevoked nodeId = do
  ConsortiumState {nodeIndexes, nodeStates} <- get
  lift $ do
    let nodeIndexAsInt = HM.lookup nodeId nodeIndexes
    assertIsJust nodeIndexAsInt
    let v = IM.lookup (fromJust nodeIndexAsInt) nodeStates
    assertIsJust v
    let (_, memberstatus, _) = fromJust v
    assert (memberstatus == Revoked)

-- |Similar to ConsortiumMembershipRevoked, it is an error to receive
-- a 'TeamLocated' event for a nodeId whose membership hasn't been
-- granted. Checks whether the given team location has been updated in
-- the nodeStates 'IntMap'.
assertTeamLocatedTo
  :: Monad m
  => NodeId
  -> Location
  -> StateT Projections.ConsortiumState (PropertyM m) ()
assertTeamLocatedTo nodeId Location {..} = do
  ConsortiumState {nodeIndexes, nodeStates} <- get
  lift $ do
    let nodeIndexAsInt = HM.lookup nodeId nodeIndexes
    assertIsJust nodeIndexAsInt
    let v = IM.lookup (fromJust nodeIndexAsInt) nodeStates
    assert (isJust v)
    let (_, _, l) = fromJust v
    assert (l == Located Location {..})

-- |This is handled identical to the 'TeamLocated' event, do we need
-- two of them? Are these interpreted different somewhere else.
assertTeamReLocatedTo
  :: Monad m
  => NodeId
  -> Location
  -> StateT Projections.ConsortiumState (PropertyM m) ()
assertTeamReLocatedTo nodeId Location {..} = do
  ConsortiumState {nodeIndexes, nodeStates} <- get
  lift $ do
    let nodeIndexAsInt = HM.lookup nodeId nodeIndexes
    assert (isJust nodeIndexAsInt)
    let v = IM.lookup (fromJust nodeIndexAsInt) nodeStates
    assert (isJust v)
    let (_, _, l) = fromJust v
    assert (l == Located Location {..})

-- |This function intersperse's our state test after every event is
-- consumed by the consumer, in this case the 'toConsortiumState''
-- function, i.e. This is in alternative to checking the outputs of
-- toConsortiumState' as it is not a one to one function and would
-- require modelling of the complete function and a mock
-- implementation, it might be better to use unit tests for such
-- complete testing.
--
-- It would be cleaner to implement separate hspec properties for each event, however this would mean the use of partial functions that match on only one event which we want to avoid.
intersperseConsortiumStateCheck
  :: Monad m
  => S.SerialT (StateT Projections.ConsortiumState (ReaderT r (PropertyM m))) Event.Event
  -> S.SerialT (StateT Projections.ConsortiumState (ReaderT r (PropertyM m))) Event.Event
intersperseConsortiumStateCheck =
  intersperseAssertsM_ $ \event ->
    hoist lift $
    case event of
      BlockCreated {blockOffset} -> assertNewBlockCreated blockOffset
      ProposalSizeLimited {size} -> assertProposalSizeLimited size
      ConsortiumMembershipGranted {nodeId} -> assertMembershipGranted nodeId
      ConsortiumMembershipRevoked {nodeId} -> assertMembershipRevoked nodeId
      TeamLocated {..} -> assertTeamLocatedTo nodeId Location {..}
      TeamReLocated {..} -> assertTeamReLocatedTo nodeId Location {..}

initialConsortiumState'Spec :: Property
initialConsortiumState'Spec =
  monadicIO $ do
    let logger = Logger (LoggerId "Test Logger")
        tid =
          NodeId "0431b548-8916-439a-ad39-02bcd116d828"
    void $
      runReaderT
        (runStateT
           (SP.drain $
            Projections.toConsortiumState'
              id
              (intersperseConsortiumStateCheck generateEventsStream))
           Projections.initialConsortiumState)
        (logger, tid)

spec :: Spec
spec = prop "initialConsortiumState'" initialConsortiumState'Spec
