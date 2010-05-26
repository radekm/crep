-- |
-- Module    : Core.Partition
-- Copyright : (c) Radek Micek 2010
-- License   : BSD3
-- Stability : experimental
--
-- This module exports type @'Pa' e@ for partitioning values of type @e@.
-- Partitions form commutative monoid with intersection.
--
module Core.Partition
  ( 
    Pa
  ) where

import Core.Partition.Internal
