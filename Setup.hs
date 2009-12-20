#!/usr/bin/env runhaskell

import Distribution.Simple
import Distribution.PackageDescription (PackageDescription)
import Distribution.Simple.LocalBuildInfo
import System.Cmd (system)

main :: IO ()
main = defaultMainWithHooks (simpleUserHooks { runTests = tests })

tests :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
tests _ _ _ _ = do system ("runhaskell ./tests/run.hs")
                   return ()

