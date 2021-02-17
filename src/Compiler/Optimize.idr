module Compiler.Optimize

import Control.Monad.State
import Data.List
import Data.String
import System
import System.File

import Compiler.Common
import Compiler.VMCode
import Core.Directory
import Core.CompileExpr
import Core.Name
import Libraries.Data.SortedMap
import Libraries.Utils.Path

record OptInfo where
  constructor MkOptInfo
  funcUsageCounter : Bool
  allFuncs : SortedMap Name VMDef

record TransformSt where
  constructor MkTransformSt
  nextLoc : Int
  instructions : List VMInst

Transform : Type -> Type
Transform = Control.Monad.State.State TransformSt

addInstructon : VMInst -> Transform ()
addInstructon i = modify (record { instructions $= (i ::)})

gatherInfo : List (Name, VMDef) -> OptInfo
gatherInfo defs = MkOptInfo True (fromList defs)

nextFreeLoc : Int -> List VMInst -> Int
nextFreeLoc i [] = i
nextFreeLoc i (DECLARE (Loc x)::rest) = nextFreeLoc (max i (x+1)) rest
nextFreeLoc i (START::_) = i
nextFreeLoc i (_::rest) = nextFreeLoc i rest

renumber : (off : Int) -> Reg -> Reg
renumber _ Discard = Discard
renumber off RVal = Loc off
renumber off (Loc n) = Loc (off + n + 1)

renumberInst : Int -> VMInst -> VMInst
renumberInst off (APPLY r f a) = APPLY (renumber off r) (renumber off f) (renumber off a)
renumberInst off (ASSIGN r src) = ASSIGN (renumber off r) (renumber off src)

renumberInst off START = START
renumberInst off x = ERROR $ "renumber not impl:" ++ show x

parameters (info : OptInfo)
  inlinable : Name -> Bool
  inlinable (NS _ (CaseBlock _ _)) = True
  inlinable n = False

  inlineFunc : Reg -> Name -> List Reg -> Transform ()
  inlineFunc r n args = do
    case lookup n info.allFuncs of
         (Just (MkVMFun iargs iis)) => do
           argShift <- (.nextLoc) <$> get
           let newVarCount = nextFreeLoc (cast $ length iargs) iis
           modify (record { nextLoc $= (newVarCount +)})
           let renumberedRval = (Loc argShift)
           let renumberedArgs = map (renumber argShift . Loc) iargs

           -- RVal from inlined function:
           addInstructon (DECLARE renumberedRval)
           -- function args:
           for renumberedArgs (\arg => addInstructon (DECLARE arg))

           for (zip renumberedArgs args) (\(t, f) => addInstructon (ASSIGN (t) (f)))
           for iis (\ins => addInstructon $ renumberInst argShift ins)
           --addInstructon (CALL r False n args)
           addInstructon (ASSIGN r renumberedRval)
           pure ()
         _ => addInstructon (CALL r False n args)

  doTransform : Name -> List Int -> VMInst -> Transform ()
  doTransform n args i = do
    case i of
         (CALL r tailpos fn args) => if inlinable n then inlineFunc r fn args else addInstructon i
         i => addInstructon i

  transformFun : Name -> List Int -> List VMInst -> VMDef
  transformFun n args is = MkVMFun args is

  transformDef : (Name, VMDef) -> (Name, VMDef)
  transformDef (n, (MkVMFun args is)) =
    let finSt = fst $ runState (MkTransformSt (nextFreeLoc (cast $ length args) is) []) (traverse_ (doTransform n args) is) in
    (n, MkVMFun args (reverse $ finSt.instructions))
  transformDef x = x

export
optimize : List (Name, VMDef) -> List (Name, VMDef)
optimize defs = let info = gatherInfo defs in
                    map (transformDef info) defs
