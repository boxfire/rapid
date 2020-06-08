module Main

import Data.List
import Data.SortedMap
import Data.Strings
import System
import System.File

import Core.Name
import Compiler.VMCode

import Data.Sexp
import Data.Sexp.Lexer
import Data.Sexp.Parser
import Compiler.VMCodeSexp
import Compiler.PrepareCode
import Compiler.SteamCG

debug : Bool
debug = True

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
         --putStrLn $ show $ parsed
         let vmcode = getVMDefs (filter isVmdef parsed)
         let foreigns = (filter isForeignDecl parsed)
         --putStrLn $ show $ vmcode
         (Right support) <- readFile "support.ll"
         | Left _ => pure ()
         let support = ""
         let nameMap = getNameMap $ map snd vmcode
         let ir = fastAppend $ [support, closureHelper] ++ (map (getVMIR debug nameMap) $ enumerate vmcode)
         _ <- writeFile (filename ++ ".output.ll") ir
         pure ()
       Left e => putStrLn $ "error" ++ e
