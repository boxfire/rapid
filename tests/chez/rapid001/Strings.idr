module Main

Show Ordering where
  show EQ = "EQ"
  show LT = "LT"
  show GT = "GT"

strings : List String
strings = [
    "abc"
  , "abcdefg"
  , "abd"
  , "zzz"
  ]
compareResults : List (String, String, Ordering)
compareResults = [
  (s1, s2, compare s1 s2) | s1 <- strings, s2 <- strings
  ]

strFun : (String -> String -> Bool) -> List (String, String, Bool)
strFun f = [
  (s1, s2, s1 `f` s2) | s1 <- strings, s2 <- strings
  ]

printRow : (String, String, Ordering) -> IO ()
printRow (s1, s2, c) = putStrLn $ "\"" ++ s1 ++ "\" `compare` \"" ++ s2 ++ "\" = " ++ show c

strOps : List (String, (String -> String -> Bool))
strOps = [
    ("< ", (<))
  , ("<=", (<=))
  , ("==", (==))
  , (">=", (>=))
  , ("> ", (>))
  ]

main : IO ()
main = do
  if "hello" == "hello world" then putStrLn "equal" else putStrLn "not equal"
  traverse_ printRow compareResults
  traverse_ putStrLn [
    "\"" ++ s1 ++ "\" " ++ fst sOp ++ " \"" ++ s2 ++ "\" = " ++ show ((snd sOp) s1 s2) | s1 <- strings, s2 <- strings, sOp <- strOps
    ]
