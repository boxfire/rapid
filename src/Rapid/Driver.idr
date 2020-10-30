module Rapid.Driver

import Data.List
import Data.Strings
import System
import System.File

import Compiler.Common
import Compiler.VMCode
import Core.Directory
import Core.CompileExpr
import Core.Name
import Utils.Path

import Compiler.PrepareCode
import Compiler.SteamCG

isBlocked : (Name, a) -> Bool
isBlocked ((NS ns n), _) with (unsafeUnfoldNamespace ns)
  isBlocked ((NS ns (UN "schemeCall")), _) | ["PrimIO"] = True
  isBlocked ((NS ns (UN "prim__schemeCall")), _) | ["PrimIO"] = True
  isBlocked ((NS ns (UN "fastPack")), _) | ["Types", "Prelude"] = True
  isBlocked ((NS ns (MN "fastPack" _)), _) | ["Types", "Prelude"] = True
  isBlocked ((NS ns (UN "fastAppend")), _) | ["Strings", "Data"] = True
  isBlocked ((NS ns (MN "fastAppend" _)), _) | ["Strings", "Data"] = True
  isBlocked ((NS _ _), _) | _ = False
isBlocked _ = False

export
writeIR : (allFunctions : List (Name, VMDef)) -> (foreigns : List (Name, NamedDef)) ->
          (support : String) -> (outfile : String) -> IO ()
writeIR allFunctions foreigns support outfile = do
  let debug = False
  let foreignCode = map (compileForeign debug) (enumerate foreigns)
  let functions = filter (not . isBlocked) allFunctions
  let nameMap = getNameMap $ map snd functions
  let indexedFuncs = enumerate functions
  let fcount = length indexedFuncs
  putStrLn $ "functions to compile: " ++ show (length indexedFuncs)
  (Right outFile) <- openFile outfile WriteTruncate
  | Left err => putStrLn $ "error opening output file: " ++ show err
  fPutStr outFile support
  fPutStr outFile closureHelper
  fPutStr outFile $ fastAppend foreignCode

  for indexedFuncs (\c => do
    let i = 1 + fst c
    when (i `mod` 100 == 0) $ putStrLn $ "compile fun " ++ show i ++ "/" ++ (show fcount) ++ ": " ++ safeName (fst (snd c))
    let funcIr = getVMIR debug nameMap c
    fPutStr outFile funcIr
    )
  closeFile outFile
