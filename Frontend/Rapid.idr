module Main

import Data.Strings
import System
import System.File

import Core.Core
import Core.Name
import Compiler.VMCode

import Compiler.VMCodeSexp
import Data.Sexp
import Data.Sexp.Parser
import Data.Sexp.Lexer
import Frontend.Compile

debug : Bool
debug = True

partial
verify : (Name, VMDef) -> String -> Bool
verify d input =
  let lexed = lexSexp input in
  let result = (lexed >>= parseSexp) in
  case result of
       Right parsed =>
         let [(_, parsedDef)] = getVMDefs parsed
             origTxt = show $ snd d
             parsedTxt = show parsedDef in
             if origTxt == parsedTxt then True else
             idris_crash ("mismatch while verifying:\n"++origTxt++"\n"++parsedTxt++"\nsource was:\+"++(show parsed))
       Left e =>
             idris_crash ("error parsing generated sexp: " ++ show e)

compile : String -> IO()
compile filename =
  do vmcode <- runCore (compileMain filename)
     case vmcode of
          (Right defs) => writeFile (filename ++ ".sexp") (fastAppend $ map (if debug then dumpDefWithCheck else dumpDef) defs) >>= \_ => pure ()
          (Left e) => (putStrLn ("error: " ++ show e) >>= \_ => exitFailure)
  where
    dumpDef : (Name, VMDef) -> String
    dumpDef d = (show $ toSexp d) ++ "\n\n"

    dumpDefWithCheck : (Name, VMDef) -> String
    dumpDefWithCheck d = let sexp = (show $ toSexp d) in
                             if verify d sexp then
                               sexp ++ "\n\n"
                             else
                               idris_crash "error while verifying generated Sexp"

main : IO ()
main = do
  args <- getArgs
  case args of
       [_, f] => compile f
       _ => putStrLn "Usage:\n\n    rapid2-fe Main.Idr"
