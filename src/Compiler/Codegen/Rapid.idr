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

import Rapid.Driver

isFgn : (Name, a, NamedDef) -> Bool
isFgn (_, _, (MkNmForeign _ _ _)) = True
isFgn _ = False

shell : List String -> String
shell args = showSep " " $ map shellQuote args
  where
    shellQuote : String -> String
    shellQuote s = s

runShell : List String -> IO ()
runShell args = do
  let cmd = shell args
  putStrLn $ "+" ++ cmd
  rc <- system cmd
  case rc of
       0 => pure ()
       err_rc => do putStrLn $ "command failed with exit code " ++ show err_rc
                    exitFailure

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
  platformLib <- findDataFile $ "rapid" </> "platform.a"
  rapidLLVMPlugin <- findDataFile $ "rapid" </> "librapid.so"

  cd <- getCompileData VMCode term
  coreLift $ putStrLn $ "got compiledata"
  let foreigns = map (\(n,_,d) => (n,d)) $ filter isFgn $ namedDefs cd
  let allFunctions = vmcode cd
  let optFlags = [
    "-mem2reg", "-constprop", "-constmerge", "-sccp", "-dce", "-globaldce",
    "-rewrite-statepoints-for-gc"]

  coreLift $ writeIR allFunctions foreigns support outputFileName

  let lateTransformFlags = ["-rapid-lower"]
  coreLift $ do
    runShell $ ["opt", outputFileName, "-load=" ++ rapidLLVMPlugin] ++ optFlags ++ lateTransformFlags ++ ["-o=" ++ bcFileName]
    runShell ["llc", "--frame-pointer=all", "-tailcallopt", "-o=" ++ asmFileName, bcFileName]
    True <- globalizeStackmap asmFileName
    | False => putStrLn "error"
    runShell ["clang", "-c", "-o", objectFileName, asmFileName]
    runShell ["clang", "-o", binaryFileName, objectFileName, runtime, platformLib, "-lm"]

    pure ()

  pure $ Nothing

execute : Ref Ctxt Defs -> (tmpDir : String) -> ClosedTerm -> Core ()
execute defs tmpDir term = do coreLift $ putStrLn "Maybe in an hour."

export
rapidCodegen : Codegen
rapidCodegen = MkCG compile execute
