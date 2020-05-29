module Main

import Data.Strings

import Core.Core
import Core.Name
import Compiler.VMCode

import Compiler.VMCodeSexp
import Data.Sexp
import Frontend.Compile

main : IO ()
main =
  do vmcode <- runCore (compileMain "Hello.idr")
     case vmcode of
          (Right defs) => (putStrLn $ fastAppend $ map dumpDef defs)
          (Left e) => (putStrLn $ "error: " ++ show e)
  where
    dumpDef : (Name, VMDef) -> String
    dumpDef d = (show $ toSexp d) ++ "\n\n"
