module Main

import Data.List
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

%default partial

debug : Bool
debug = True

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

handleError : Error -> IO ()
handleError e = printLn e
--(\e => (putStrLn ("error: " ++ show e) >>= \_ => exitFailure))

output : String -> CompileData -> IO ()
output filename cd = do let foreignDecls = map dumpFgn (namedDefs cd)
                        let compiledFunctions = map (if debug then dumpDefWithCheck else dumpDef) (vmcode cd)
                        writeFile ("build/rapid/" ++ filename ++ ".sexp") (fastAppend (foreignDecls ++ compiledFunctions))
                        pure ()
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


removeExtension : String -> String
removeExtension s = pack $ reverse $ removeExtension' $ reverse $ unpack s where
  removeExtension' : List Char -> List Char
  removeExtension' ('r'::'d'::'i'::'.'::base) = base
  removeExtension' s = s

compile : String -> IO ()
compile filename = do coreRun (compileMain filename) handleError (output $ removeExtension filename)
                      pure ()

main : IO ()
main = do
  args <- getArgs
  case args of
       [_, f] => compile f
       _ => putStrLn "Usage:\n\n    rapid2-fe Main.Idr"
