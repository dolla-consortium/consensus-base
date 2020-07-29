{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Dolla.Consensus.Voting.Round (Round(..)) where

import           Data.Aeson

newtype Round = Round Int
  deriving (Show, Num, Enum, Real, Integral, Eq, Ord, ToJSON,FromJSON) via Int