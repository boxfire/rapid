module Rapid.Main

import System

import Compiler.Common
import Idris.Driver

import Compiler.Codegen.VmcodeSexp
import Compiler.Codegen.Rapid

main : IO ()
main = do
  mainWithCodegens [
  ("llvm", rapidCodegen),
  ("vmcode-sexp", vmcodeSexp)
  ]
