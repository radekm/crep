-- |
-- Module    : Core.Regex
-- Copyright : (c) Radek Micek 2009
-- License   : BSD3
-- Stability : experimental
--
-- Extended regular expressions.
--
module Core.Regex where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Monad (guard)
import Core.SymbSet

-- |Represents extended regular expression.
data Regex = REpsilon
           | RCharSet CharSet
           | ROr Regex Regex
           | RAnd Regex Regex
           | RConcat Regex Regex
           | RCounter !Laziness !Int !(Maybe Int) Regex
           | RNot Regex
           | RGroup !Int Regex

data Laziness = Greedy | Lazy
              deriving Eq

-- |Converts list of regular expressions to one regular expression.
wrap :: Regex -> (Regex -> Regex -> Regex) -> [Regex] -> Regex
wrap emptyRegex _    []      = emptyRegex
wrap _          _    [x]     = x
wrap _          cons [x, x'] = cons x x'
wrap emptyRegex cons (x:xs)  = cons x (wrap emptyRegex cons xs)

type Captured = [(Int, String)]

-- |Returns matching string for each group.
capture :: Regex -> String -> Captured
capture re = (\(cs, _, _) -> cs) . head .
             filter (\(_, _, rest) -> null rest) . cap re
  where
    -- Returns: (captured content, processed prefix of xs, rest suffix of xs).
    cap :: Regex -> String -> [(Captured, String, String)]
    cap REpsilon xs = [([], "", xs)]
    cap (RCharSet cs) (x:xs)
      | member x cs = [([], [x], xs)]
    cap (RCharSet _) _ = []
    cap (ROr a b) xs = cap a xs ++ cap b xs
    cap (RAnd a b) xs
      = concatMap
          (\(cs, pref, suf) ->
            case filter (\(_, _, rest) -> null rest) (cap b pref) of
              ((cs', _, _):_) -> [(cs ++ cs', pref, suf)]
              _               -> []) $
          cap a xs
    cap (RConcat a b) xs = do (cs, pref, suf) <- cap a xs
                              (cs', pref', suf') <- cap b suf
                              return (cs ++ cs', pref ++ pref', suf')
    cap (RCounter lazy l h r) xs = rep [] "" xs l h
      where
        rep cs pref suf lo hi
          | lo == 0 && (hi == Nothing || hi >= Just 1)
          = if lazy == Greedy then more lo ++ [now]
                              else now:more lo
          | lo == 0  = [now]
          | otherwise = more (pred lo)
          where
            more newLo = do (cs', pref', suf') <- cap r suf
                            -- Empty matches are not allowed.
                            guard (not $ null pref')
                            rep (cs ++ cs') (pref ++ pref')
                                suf' newLo (pred <$> hi)
            now  = (cs, pref, suf)
    cap (RNot r) xs
      = map (\(pref, suf) -> ([], pref, suf)) $
        filter (\(pref, _) ->
                 null $  -- Cannot be matched by r.
                 filter (\(_, _, rest) -> null rest) $
                 cap r pref) $
        reverse $ partitions xs
      where
        partitions []     = [([], [])]
        partitions (a:as) = ([], a:as) : map (first (a:)) (partitions as)
    cap (RGroup i r) xs = map (\(cs, pref, suf) ->
                                ((i, pref):cs, pref, suf)) $
                          cap r xs

data Descriptor
  = Epsilon | Atom | ONot | OCounter | OConcat | OAnd | OOr
  deriving (Eq, Ord)

instance Show Regex where
  showsPrec _ = fst . shows'
    where
      -- Returns a pair @(repr, d)@ where @repr@ is a representation
      -- of given regular expression and @d@ describes shape
      -- of the regular expression.
      shows' :: Regex -> (String -> String, Descriptor)
      shows' REpsilon       = (id, Epsilon)
      shows' (RCharSet set) = (shows set, Atom)
      shows' (ROr a b)      = (wrap' OOr a . ('|':) . wrap' OOr b, OOr)
      shows' (RAnd a b)     = (wrap' OAnd a . ('&':) . wrap' OAnd b, OAnd)
      shows' (RConcat a b)  = (wrap' OConcat a . wrap' OConcat b, OConcat)
      shows' (RNot r)       = (('^':) . wrap' ONot r, ONot)
      shows' (RGroup _ r)   = (('(':) . shows r . (')':), Atom)
      shows' (RCounter lazy minRep maxRep r)
        = (wrap' OCounter r . (counterStr ++), OCounter)
        where
          counterStr = '{':minRepStr ++ maxRepStr ++ '}':lazyStr
          lazyStr    = if lazy == Lazy then "?" else ""
          minRepStr  = show minRep
          maxRepStr  = case maxRep of Nothing -> ","
                                      Just maxRep'
                                        | minRep == maxRep' -> ""
                                        | otherwise -> ',':show maxRep'
      -- Returns representation of @r@ which can be given as an argument
      -- to operator @parent@. That means represantation of @r@ is taken
      -- and wrapped in parentheses if necessary.
      wrap' parent r
        | (parent >= OConcat && parent >= child)
            || (child == ONot && parent == OCounter)
            || child == Atom
          = repr
        | otherwise = ("(?" ++) . repr . (')':)
        where
          (repr, child) = shows' r

