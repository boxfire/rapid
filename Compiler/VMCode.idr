module Compiler.VMCode

import Core.TT

import Data.List
import Data.Maybe
import Data.Strings

%default covering

showSep : String -> List String -> String
showSep s xs = go xs where
  go' : List String -> String
  go' xs = concat $ map ((++) s) xs

  go : List String -> String
  go [] = ""
  go [x] = x
  go (x::rest) = x ++ go' rest

public export
data Name = MkName String

export
Show Name where
  show (MkName s) = s

public export
data Reg : Type where
     RVal : Reg
     Loc : Int -> Reg
     Discard : Reg

-- VM instructions - first Reg is where the result goes, unless stated
-- otherwise.

-- As long as you have a representation of closures, and an 'apply' function
-- which adds an argument and evaluates if it's fully applied, then you can
-- translate this directly to a target language program.
public export
data VMInst : Type where
     DECLARE : Reg -> VMInst
     START : VMInst -- start of the main body of the function
     ASSIGN : Reg -> Reg -> VMInst
     MKCON : Reg -> (tag : Int) -> (args : List Reg) -> VMInst
     MKCLOSURE : Reg -> Name -> (missing : Nat) -> (args : List Reg) -> VMInst
     MKCONSTANT : Reg -> Constant -> VMInst
     APPLY : Reg -> (f : Reg) -> (a : Reg) -> VMInst
     CALL : Reg -> (tailpos : Bool) -> Name -> (args : List Reg) -> VMInst
     OP : Reg -> String -> List Reg -> VMInst
     EXTPRIM : Reg -> String -> List Reg -> VMInst

     CASE : Reg -> -- scrutinee 
            (alts : List (Int, List VMInst)) -> -- based on constructor tag
            (def : Maybe (List VMInst)) ->
            VMInst
     CONSTCASE : Reg -> -- scrutinee 
                 (alts : List (Constant, List VMInst)) ->
                 (def : Maybe (List VMInst)) ->
                 VMInst
     PROJECT : Reg -> (value : Reg) -> (pos : Int) -> VMInst
     NULL : Reg -> VMInst

     ERROR : String -> VMInst

public export
data VMDef : Type where
     MkVMFun : (args : List Reg) -> List VMInst -> VMDef
     MkVMError : List VMInst -> VMDef

export
Show Reg where
  show RVal = "RVAL"
  show (Loc i) = "v" ++ show i
  show Discard = "DISCARD"

export
Eq Reg where
  RVal == RVal = True
  Discard == Discard = True
  (Loc a) == (Loc b) = (a == b)
  _ == _ = False

export
Show VMInst where
  show (DECLARE r) = "DECLARE " ++ show r
  show START = "START"
  show (ASSIGN r v) = show r ++ " := " ++ show v
  show (MKCON r t args)
      = show r ++ " := MKCON " ++ show t ++ " (" ++
                  showSep ", " (map show args) ++ ")"
  show (MKCLOSURE r n m args)
      = show r ++ " := MKCLOSURE " ++ show n ++ " " ++ show m ++ " (" ++
                  showSep ", " (map show args) ++ ")"
  show (MKCONSTANT r c) = show r ++ " := MKCONSTANT " ++ show c
  show (APPLY r f a) = show r ++ " := " ++ show f ++ " @ " ++ show a
  show (CALL r t n args)
      = show r ++ " := " ++ (if t then "TAILCALL " else "CALL ") ++
        show n ++ "(" ++ showSep ", " (map show args) ++ ")"
  show (OP r op args)
      = show r ++ " := " ++ "OP " ++
        show op ++ "(" ++ showSep ", " (map show (toList args)) ++ ")"
  show (EXTPRIM r n args)
      = show r ++ " := " ++ "EXTPRIM " ++
        show n ++ "(" ++ showSep ", " (map show args) ++ ")"

  show (CASE scr alts def)
      = "CASE " ++ show scr ++ " " ++ show alts ++ " {default: " ++ show def ++ "}"
  show (CONSTCASE scr alts def)
      = "CASE " ++ show scr ++ " " ++ show alts ++ " {default: " ++ show def ++ "}"

  show (PROJECT r val pos)
      = show r ++ " := PROJECT(" ++ show val ++ ", " ++ show pos ++ ")"
  show (NULL r) = show r ++ " := NULL"
  show (ERROR str) = "ERROR " ++ show str

export
Show VMDef where
  show (MkVMFun args body) = show args ++ ": \n  " ++ (showSep "\n  " (map show body)) ++ "\n"
  show (MkVMError err) = "Error: " ++ show err

