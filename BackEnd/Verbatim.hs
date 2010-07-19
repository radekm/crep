-- |
-- Module    : BackEnd.Verbatim
-- Copyright : (c) Radek Micek 2010
-- License   : BSD3
-- Stability : experimental
--
-- Contains quasi-quoter.
--
module BackEnd.Verbatim where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

verbatim :: QuasiQuoter
verbatim = QuasiQuoter stringE (litP . stringL)
