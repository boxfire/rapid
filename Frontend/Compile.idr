module Frontend.Compile


import Compiler.Common
import Compiler.VMCode

import Core.AutoSearch
import Core.CaseTree
import Core.CompileExpr
import Core.Context
import Core.Context
import Core.Core
import Core.Env
import Core.InitPrimitives
import Core.LinearCheck
import Core.Metadata
import Core.Name
import Core.Normalise
import Core.Options
import Core.Termination
import Core.TT
import Core.Unify
import Core.UnifyState

import Idris.Desugar
import Idris.ProcessIdr
import Idris.REPL
import Idris.REPLOpts
import Idris.SetOptions
import Idris.Syntax
import IdrisPaths

import TTImp.Elab
import TTImp.Elab.Check
import TTImp.TTImp

export
compileMain : String -> Core CompileData
compileMain fname = do
  defs <- initDefs
  c <- newRef Ctxt defs
  s <- newRef Syn initSyntax
  m <- newRef MD initMetadata
  o <- newRef ROpts (REPLOpts.defaultOpts Nothing (REPL False))
  addPrimitives
  setWorkingDir "."

  setPrefix yprefix
  --defs <- get Ctxt
  addPkgDir "prelude"
  addPkgDir "base"
  addPkgDir "idris2"
  addPkgDir "contrib"

  addLibDir "."

  u <- newRef UST initUState
  readPrelude True
  --process "Hello.idr" "2Hello.idr"
  loadMainFile fname

  let ctm = the PTerm (PRef (MkFC "(script)" (0, 0) (0, 0)) (UN "main"))
  inidx <- resolveName (UN "[input]")
  ttimp <- desugar AnyExpr [] (PApp replFC (PRef replFC (UN "unsafePerformIO")) ctm)
  (tm, gty) <- elabTerm inidx InExpr [] (MkNested []) [] ttimp Nothing
  tm_erased <- linearCheck replFC linear True [] tm
  getCompileData VMCode tm_erased
