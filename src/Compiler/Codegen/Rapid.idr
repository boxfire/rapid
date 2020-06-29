module Compiler.Codegen.Rapid

import Data.List
import Data.Strings
import System
import System.File

import Core.Directory
import Core.CompileExpr
import Core.Context
import Compiler.Common
import Compiler.VMCode
import Utils.Path

import Compiler.PrepareCode
import Compiler.SteamCG

isBlocked : (Name, a) -> Bool
isBlocked ((NS ["PrimIO"] (UN "schemeCall")), _) = True
isBlocked ((NS ["PrimIO"] (UN "prim__schemeCall")), _) = True
isBlocked ((NS ["Prelude"] (UN "fastPack")), _) = True
isBlocked ((NS ["Prelude"] (MN "fastPack" _)), _) = True
isBlocked ((NS ["Strings", "Data"] (UN "fastAppend")), _) = True
isBlocked ((NS ["Strings", "Data"] (MN "fastAppend" _)), _) = True
isBlocked _ = False

isFgn : (Name, FC, NamedDef) -> Bool
isFgn (_, _, (MkNmForeign _ _ _)) = True
isFgn _ = False

debug : Bool
debug = False

shell : List String -> String
shell args = showSep " " $ map shellQuote args
  where
    shellQuote : String -> String
    shellQuote s = s

globalizeStackmap : String -> IO Bool
globalizeStackmap fname = do
  (Right outFile) <- openFile fname Append
  | Left err => do putStrLn $ "error opening asm file: " ++ show err
                   pure False
  fPutStr outFile "\n.globl __LLVM_StackMaps\n"
  closeFile outFile
  pure True

compile : Ref Ctxt Defs -> (tmpDir : String) -> (outputDir : String) ->
        ClosedTerm -> (outfile : String) -> Core (Maybe String)
compile defs tmpDir outputDir term outfile = do
  let appDirRel = outfile ++ "_rapid" -- relative to build dir
  let appDirGen = outputDir </> appDirRel -- relative to here
  let outputFileName = appDirGen </> (outfile ++ ".ll") -- LLVM IR (text)
  let bcFileName = appDirGen </> (outfile ++ ".bc") -- optimized LLVM bitcode
  let asmFileName = appDirGen </> (outfile ++ ".s") -- compiled assembler
  let objectFileName = appDirGen </> (outfile ++ ".o") -- object file
  let binaryFileName = outputDir </> outfile
  coreLift $ mkdirAll appDirGen

  -- load supporting files first, so we can fail early
  support <- readDataFile $ "rapid" </> "support.ll"
  runtime <- findDataFile $ "rapid" </> "runtime.bc"

  cd <- getCompileData VMCode term
  coreLift $ putStrLn $ "got compiledata"
  let foreigns = map (\(n,_,d) => (n,d)) $ filter isFgn $ namedDefs cd
  let foreignCode = map (compileForeign debug) (enumerate foreigns)
  let allFunctions = vmcode cd
  let functions = filter (not . isBlocked) allFunctions
  let nameMap = getNameMap $ map snd functions
  let indexedFuncs = enumerate functions
  let fcount = length indexedFuncs
  let optFlags = [
    "-mem2reg", "-constprop", "-constmerge", "-sccp", "-dce", "-globaldce",
    "-rewrite-statepoints-for-gc"]

  coreLift $ putStrLn $ "functions to compile: " ++ show (length indexedFuncs)
  --let funCode = map (getVMIR debug nameMap) indexedFuncs
  coreLift $ do
    (Right outFile) <- openFile outputFileName WriteTruncate
    | Left err => putStrLn $ "error opening output file: " ++ show err
    fPutStr outFile support
    fPutStr outFile closureHelper
    fPutStr outFile $ fastAppend foreignCode

    --putStrLn $ "number of foreign decls: " ++ show (length foreigns)

    for indexedFuncs (\c => do
      putStrLn $ "compile fun " ++ show (1 + fst c) ++ "/" ++ (show fcount) ++ ": " ++ safeName (fst (snd c))
      let funcIr = getVMIR debug nameMap c
      fPutStr outFile funcIr
      )
    closeFile outFile

  coreLift $ do
    system $ shell $ ["opt", outputFileName] ++ optFlags ++ ["-o=" ++ bcFileName]
    system $ shell ["llc", "-tailcallopt", "-o=" ++ asmFileName, bcFileName]
    True <- globalizeStackmap asmFileName
    | False => putStrLn "error"
    system $ shell ["clang", "-c", "-o", objectFileName, asmFileName]
    system $ shell ["clang", "-o", binaryFileName, objectFileName, runtime]

    pure ()

  pure $ Nothing

execute : Ref Ctxt Defs -> (tmpDir : String) -> ClosedTerm -> Core ()
execute defs tmpDir term = do coreLift $ putStrLn "Maybe in an hour."

export
rapidCodegen : Codegen
rapidCodegen = MkCG compile execute
