module Main

import Data.Strings
import System
import System.File

import Core.Core
import Core.Name
import Compiler.VMCode

import Compiler.VMCodeSexp
import Data.Sexp
import Frontend.Compile

compile : String -> IO()
compile filename =
  do vmcode <- runCore (compileMain filename)
     case vmcode of
          (Right defs) => writeFile (filename ++ ".sexp") (fastAppend $ map dumpDef defs) >>= \_ => pure ()
          (Left e) => (putStrLn $ "error: " ++ show e)
  where
    dumpDef : (Name, VMDef) -> String
    dumpDef d = (show $ toSexp d) ++ "\n\n"

main : IO ()
main = do
  args <- getArgs
  case args of
       [_, f] => compile f
       _ => putStrLn "Usage:\n\n    rapid2-fe Main.Idr"
