module Main

import Data.Sexp
import SexpLexer
import SexpParser
import Compiler.VMCode

{-getVMDefs : List Sexp -> List (Either String (String, VMDef))-}
getVMDefs : List Sexp -> List (String, VMDef)
getVMDefs s = rights $ map fromSexp s

main : IO ()
main = do
  (Right input) <- readFile "second.sexp"
  | Left _ => putStrLn "read file error"
  let lexed = lexSexp input
  {-putStrLn $ show lexed-}
  let result = (lexed >>= parseSexp)
  case result of
       Right parsed => do
         putStrLn $ show $ parsed
         putStrLn $ show $ getVMDefs parsed
       Left e => putStrLn $ "error" ++ e
