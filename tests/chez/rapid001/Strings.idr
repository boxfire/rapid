module Main

import Data.String

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

alphabet : String
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

partial
main : IO ()
main = do
  if "hello" == "hello world" then putStrLn "equal" else putStrLn "not equal"
  traverse_ printRow compareResults
  traverse_ putStrLn [
    "\"" ++ s1 ++ "\" " ++ fst sOp ++ " \"" ++ s2 ++ "\" = " ++ show ((snd sOp) s1 s2) | s1 <- strings, s2 <- strings, sOp <- strOps
    ]
  putStrLn "substrings:"
  putStrLn (prim__strTail alphabet)
  putStrLn (substr 0 0 alphabet)
  putStrLn (substr 0 25 alphabet)
  putStrLn (substr 0 26 alphabet)
  putStrLn (substr 0 27 alphabet)
  putStrLn (substr 9 3 alphabet)
  putStrLn (substr 9 9999 alphabet)
  putStrLn (substr 9999 4 alphabet)
  putStrLn $ "neg1: " ++ (prim__strSubstr (-12) 8 alphabet)
  putStrLn $ "neg2: " ++ (prim__strSubstr 4 (-17) alphabet)
  putStrLn $ "neg3: " ++ (prim__strSubstr (-13) (-5) alphabet)
  putStrLn $ "neg4: " ++ (prim__strSubstr 28 (-4) alphabet)
  putStrLn $ "rev1: " ++ (prim__strReverse "")
  putStrLn $ "rev2: " ++ (prim__strReverse "x")
  putStrLn $ "rev3: " ++ (prim__strReverse alphabet)
  printLn $ fastUnpack "abc456@//$"
