import System.Cmd (system)

main :: IO ()
main = runTests tests

runTests :: [(String, String)] -> IO ()
runTests ((desc, t):ts)
  = do putStrLn ("\nTesting " ++ desc ++ ":")
       system ("runhaskell ./tests/" ++ t ++ ".hs")
       runTests ts
runTests []     = return ()

tests :: [(String, String)]
tests = [("Partition", "Partition")
        ,("UTF8", "UTF8")
        ,("Matcher", "Matcher")]
