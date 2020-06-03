module Main

import Data.Strings
import System
import System.File

import Compiler.Common
import Compiler.VMCode
import Core.Core
import Core.CompileExpr
import Core.FC
import Core.Name

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
  do compiled <- runCore (compileMain filename)
     case compiled of
          (Right cd) => do
            let foreignDecls = map dumpFgn (namedDefs cd)
            let compiledFunctions = map (if debug then dumpDefWithCheck else dumpDef) (vmcode cd)
            writeFile ("build/rapid/" ++ filename ++ ".sexp") (fastAppend (foreignDecls ++ compiledFunctions))
            pure ()
          (Left e) => (putStrLn ("error: " ++ show e) >>= \_ => exitFailure)
  where
    dumpFgn : (Name, FC, NamedDef) -> String
    dumpFgn (n, _, def@(MkNmForeign cs args ret)) = show (toSexp (n, def)) ++ "\n" --";FOREIGN: " ++ show n ++ " = " ++ show cs ++ " (" ++ show args ++ ") -> " ++ show ret ++ "\n"
    dumpFgn _ = "" -- not a foreign function

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
