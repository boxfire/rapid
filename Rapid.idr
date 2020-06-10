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

isBlacklisted : (Name, a) -> Bool
isBlacklisted ((NS ["PrimIO"] (UN "schemeCall")), _) = True
isBlacklisted ((NS ["PrimIO"] (UN "prim__schemeCall")), _) = True
isBlacklisted ((NS ["Prelude"] (UN "fastPack")), _) = True
isBlacklisted ((NS ["Prelude"] (MN "fastPack" _)), _) = True
isBlacklisted ((NS ["Strings", "Data"] (UN "fastAppend")), _) = True
isBlacklisted ((NS ["Strings", "Data"] (MN "fastAppend" _)), _) = True
isBlacklisted _ = False

main : IO ()
main = do
  let verbose = False
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
         let vmcodeAll = getVMDefs (filter isVmdef parsed)
         let vmcode = filter (not . isBlacklisted) vmcodeAll
         let foreigns = (filter isForeignDecl parsed)
         --putStrLn $ show $ vmcode
         (Right support) <- readFile "support.ll"
         | Left _ => pure ()
         let support = ""
         let nameMap = getNameMap $ map snd vmcode
         let indexedCode = enumerate vmcode
         let funCode = map (getVMIR debug nameMap) indexedCode
         --funCode <- for indexedCode (\c => do when verbose $ putStrLn $ "compile fun: " ++ safeName (fst (snd c))
                                              --pure $ getVMIR debug nameMap c
                                          --)
         putStrLn $ "codegen complete: " ++ show (length funCode)
         putStrLn $ "result size: " ++ show (sum' (map (prim__strLength) funCode))
         (Right outFile) <- openFile (filename ++ ".output.ll") WriteTruncate
         | Left err => putStrLn $ "error: " ++ show err
         fPutStr outFile support
         fPutStr outFile closureHelper
         traverse_ (fPutStr outFile) funCode
         closeFile outFile
         --let ir = fastAppend $ [support, closureHelper] ++ funCode
         --putStrLn "fastAppend complete"
         --_ <- writeFile (filename ++ ".output.ll") ir
         pure ()
       Left e => putStrLn $ "error" ++ e
