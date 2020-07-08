module Rapid.Main

import System

import Compiler.Common
import Idris.CommandLine
import Idris.Driver

import Compiler.Codegen.VmcodeSexp
import Compiler.Codegen.Rapid

main : IO ()
main = do
  (_::args) <- getArgs
    | _ => do putStrLn "invalid command line"
              exitFailure

  Right opts <- pure $ getOpts args
    | Left err => do putStrLn err
                     putStrLn usage
                     exitFailure

  mainWithCodegensAndOpts [
  ("llvm", rapidCodegen),
  ("vmcode-sexp", vmcodeSexp)
  ] opts
