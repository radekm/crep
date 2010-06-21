-- |
-- Module    : Core.PartialOrder
-- Copyright : (c) Radek Micek 2010
-- License   : BSD3
-- Stability : experimental
--
-- Partial order on rules.
--
module Core.PartialOrder where

import Data.Array
import Core.Rule
import Data.List (groupBy, sortBy)
import Data.Bits ((.|.), setBit, testBit)

type Bitmask = Integer

-- | Function @'idx' bitmask n@ returns whether @n@-th bit is set.
idx :: Bitmask -> RuNum -> Bool
idx bm (RuN n) = bm `testBit` n

-- | Partial order @po@ of rules is represented as array of bitmasks.
--   @a@-th bit of @po!b@ is set iff rule @a@ is greater than rule @b@.
type POrder = Array RuNum Bitmask

-- | Function @'isGreater' a b po@ returns whether @a > b@.
isGreater :: RuNum -> RuNum -> POrder -> Bool
isGreater a b po = po!b `idx` a

-- | Creates partial order from given rules.
fromRules :: [Rule p a] -> POrder
fromRules rules = toPOrder groupedRules
  where
    -- Rules are grouped by priority. First group has the lowest priority.
    groupedRules :: [[RuNum]]
    groupedRules = map (map fst) $ groupBy (\a b -> snd a == snd b)
                                 $ sortBy (\a b -> snd a `compare` snd b)
                                 $ zip [RuN 0..]
                                 $ map toPrio rules

    toPrio (Rule _ p _ _ _) = p

    toPOrder :: [[RuNum]] -> POrder
    toPOrder []     = array (RuN 0, RuN $ -1) []
    toPOrder (r:rs) = fst $ foldl (\(po, prev) cur ->
                                    (mkGreaterLists cur prev po, cur))
                                  (emptyPOrder, r) rs
      where
        emptyPOrder = listArray (RuN 0, RuN $ pred $ length rules) (repeat 0)

-- | Function @'mkGreater' a b po@ returns new order where @a > b@.
mkGreater :: RuNum -> RuNum -> POrder -> POrder
mkGreater a@(RuN a') b po
  | isGreater b a po = error "Core.PartialOrder.mkGreater: is lower"
  | otherwise
  = po // ((b, po!b .|. greaterThanB):
           [(i, po!i .|. greaterThanB) | i <- [lo..hi], isGreater b i po])
  where
    (lo, hi)    = bounds po
    greaterThanA = po!a
    greaterThanB = greaterThanA `setBit` a'

-- | Function @'mkGreaterLists' his los po@ returns partial order where
--   every rule from @his@ is greater than all rules in @los@.
mkGreaterLists :: [RuNum] -> [RuNum] -> POrder -> POrder
mkGreaterLists his los o = foldl (\po hi -> mkGreaterList hi los po) o his
  where
    -- Makes rule @h@ greater than all rules in @ls@.
    mkGreaterList h ls o' = foldl (\po l -> mkGreater h l po) o' ls
