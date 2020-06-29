module Rapid

import Data.List
import Data.SortedMap
import Data.Strings
import System
import System.File

import Compiler.VMCode
import Core.CompileExpr
import Core.Name

import Data.Sexp
import Data.Sexp.Lexer
import Data.Sexp.Parser
import Compiler.VMCodeSexp
import Compiler.PrepareCode
import Compiler.SteamCG

isBlacklisted : (Name, a) -> Bool
isBlacklisted ((NS ["PrimIO"] (UN "schemeCall")), _) = True
isBlacklisted ((NS ["PrimIO"] (UN "prim__schemeCall")), _) = True
isBlacklisted ((NS ["Prelude"] (UN "fastPack")), _) = True
isBlacklisted ((NS ["Prelude"] (MN "fastPack" _)), _) = True
isBlacklisted ((NS ["Strings", "Data"] (UN "fastAppend")), _) = True
isBlacklisted ((NS ["Strings", "Data"] (MN "fastAppend" _)), _) = True
isBlacklisted _ = False

record CliOptions where
  constructor MkOptions
  debugEnabled : Bool
  inputFilename : String

emptyOpts : CliOptions
emptyOpts = MkOptions False ""

parseCliArgs : List String -> Either String CliOptions
parseCliArgs [] = Left "missing argument"
parseCliArgs (_::args) = go args emptyOpts where
  go : List String -> CliOptions -> Either String CliOptions
  go [] opts = if inputFilename opts /= "" then Right opts else Left "missing input filename"
  go ("--debug"::rest) opts = go rest $ record { debugEnabled = True } opts
  go (fname::rest) opts = go rest $ record { inputFilename = fname } opts

partial
main : IO ()
main = do
  args <- getArgs
  (Right opts) <- pure $ parseCliArgs args
  | Left e => putStrLn e
  let debug = debugEnabled opts
  let filename = inputFilename opts
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
         --putStrLn $ show $ vmcode
         (Right support) <- readFile "support/rapid/support.ll"
         | Left _ => putStrLn "support.ll not found"
         let support = ""
         let nameMap = getNameMap $ map snd vmcode

         let foreignSexps = (filter isForeignDecl parsed)
         (Right foreigns) <- pure $ getForeignDefs foreignSexps
         | Left e => putStrLn $ "error parsing foreign decls: " ++ e
         putStrLn $ "number of foreign decls: " ++ show (length foreigns)
         let foreignCode = map (compileForeign debug) (enumerate foreigns)

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
         traverse_ (fPutStr outFile) foreignCode
         traverse_ (fPutStr outFile) funCode
         closeFile outFile
         --let ir = fastAppend $ [support, closureHelper] ++ funCode
         --putStrLn "fastAppend complete"
         --_ <- writeFile (filename ++ ".output.ll") ir
         pure ()
       Left e => putStrLn $ "error:\n" ++ e
