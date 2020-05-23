module Main

import Data.Strings
import System
import System.File

import Data.Sexp
import SexpLexer
import SexpParser
import Compiler.VMCode
import Compiler.VMCodeSexp
import Compiler.SteamCG

{-getVMDefs : List Sexp -> List (Either String (String, VMDef))-}
getVMDefs : List Sexp -> List (String, VMDef)
getVMDefs s = either (\error=>[(error, MkVMError [])]) id $ traverse fromSexp s

main : IO ()
main = do
  (_::filename::_) <- getArgs
  | _ => putStrLn "missing argument"
  putStrLn $ "reading input from: " ++ filename
  (Right input) <- readFile filename
  | Left _ => putStrLn "read file error"
  let lexed = lexSexp input
  {-putStrLn $ show lexed-}
  let result = (lexed >>= parseSexp)
  case result of
       Right parsed => do
         putStrLn $ show $ parsed
         let vmcode = getVMDefs parsed
         putStrLn $ show $ vmcode
         (Right support) <- readFile "support.ll"
         | Left _ => pure ()
         let support = ""
         let ir = support ++ (unlines $ map getVMIR $ enumerate vmcode)
         {-putStrLn $ ir-}
         _ <- writeFile (filename ++ ".output.ll") ir
         pure ()
       Left e => putStrLn $ "error" ++ e
