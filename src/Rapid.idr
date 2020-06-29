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
import Rapid.Driver

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

main : IO ()
main = do
  args <- getArgs
  (Right opts) <- pure $ parseCliArgs args
  | Left e => putStrLn e

  (Right support) <- readFile "support/rapid/support.ll"
  | Left _ => putStrLn "support.ll not found"

  let debug = debugEnabled opts
  let filename = inputFilename opts
  putStrLn $ "reading input from: " ++ filename

  (Right input) <- readFile filename
  | Left _ => putStrLn "read file error"
  (Right parsed) <- pure (lexSexp input >>= parseSexp)
  | Left e => putStrLn $ "error:\n" ++ e
  (Right allFunctions) <- pure $ getVMDefs (filter isVmdef parsed)
  | Left e => putStrLn $ "error parsing VMCode s-exp: " ++ e
  let foreignSexps = (filter isForeignDecl parsed)
  (Right foreigns) <- pure $ getForeignDefs foreignSexps
  | Left e => putStrLn $ "error parsing foreign decls: " ++ e

  writeIR allFunctions foreigns support (filename ++ ".output.ll")
