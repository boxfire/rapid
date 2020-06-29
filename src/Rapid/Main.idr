module Rapid.Main

import Compiler.Common
import Idris.Driver

import Compiler.Codegen.VmcodeSexp
import Compiler.Codegen.Rapid

main : IO ()
main = mainWithCodegens [
  ("llvm", rapidCodegen),
  ("vmcode-sexp", vmcodeSexp)
  ]
