module Compiler.VMCode

import Core.TT

import Codegen
import Data.Sexp
import Utils.Hex

%default covering

HEADER_SIZE : String
HEADER_SIZE = "8"

showSep : String -> List String -> String
showSep s xs = go xs where
  go' : List String -> String
  go' xs = concat $ map ((++) s) xs

  go : List String -> String
  go [] = ""
  go [x] = x
  go (x::rest) = x ++ go' rest

safeName : String -> String
safeName s = concatMap okchar (unpack s)
  where
    okchar : Char -> String
    okchar c = if isAlphaNum c
                  then cast c
                  else "_" ++ asHex (cast {to=Int} c) ++ "_"

public export
data Name = MkName String

public export
ToSexp String where
  toSexp n = SAtom ("\"" ++ (show n) ++ "\"")

export
ToSexp Name where
  toSexp (MkName n) = SAtom ("\"" ++ (show n) ++ "\"")

export
Show Name where
  show (MkName s) = s

export
FromSexp Name where
  fromSexp (SAtom s) = Right $ MkName s
  fromSexp _ = Left $ "not a name"

export
FromSexp Bool where
  fromSexp (SAtom "True") = Right $ True
  fromSexp (SAtom "False") = Right $ False
  fromSexp _ = Left $ "invalid Bool"

public export
data Reg : Type where
     RVal : Reg
     Loc : Int -> Reg
     Discard : Reg

ToSexp Reg where
  toSexp RVal = SAtom "RVAL"
  toSexp (Loc i) = SAtom ("v" ++ show i)
  toSexp Discard = SAtom "DISCARD"

FromSexp Reg where
  fromSexp (SAtom "RVAL") = Right RVal
  fromSexp (SAtom "DISCARD") = Right Discard
  fromSexp (SAtom "") = Left "invalid reg: \"\""
  fromSexp (SAtom s) = Right (Loc $ cast $ assert_total $ strTail s)
  fromSexp s = Left ("invalid reg: " ++ show s)

export
FromSexp Constant where
  fromSexp (SList [SAtom "I", SAtom i]) = Right $ I $ cast i
  fromSexp (SList [SAtom "BI", SAtom i]) = Right $ BI $ cast i
  fromSexp (SList [SAtom "Str", SAtom s]) = Right $ Str s
  fromSexp (SList [SAtom "Ch", SAtom c]) = Right $ Ch $ assert_total $ strHead c
  fromSexp (SList [SAtom "Db", SAtom d]) = Right $ Db $ cast d
  fromSexp (SList [SAtom "%World"]) = Right $ WorldVal
  fromSexp s = Left $ "invalid constant: " ++ show s

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

shelper : ToSexp a => String -> List a -> Sexp
shelper s xs = SList ([SAtom s] ++ map toSexp xs)

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

public export
ToSexp Constant where
  toSexp c = SAtom $ show c

public export
ToSexp VMInst where
  toSexp (DECLARE r) = SList [SAtom "DECLARE", toSexp r]
  toSexp START = SList [SAtom "START"]
  toSexp (ASSIGN d s) = shelper "ASSIGN" [d, s]
  toSexp (MKCON reg tag args) = SList $ [SAtom "MKCON", toSexp reg, SAtom $ show tag, SList $ map toSexp args]
  toSexp (MKCLOSURE reg n missing args) = SList $ [SAtom "MKCLOSURE", toSexp reg, toSexp n, SAtom $ show missing, SList (map toSexp args)]
  toSexp (MKCONSTANT reg const) = SList $ [SAtom "MKCONSTANT", toSexp reg, toSexp const]
  toSexp (CALL reg isTail n args) = SList $ [SAtom "CALL", toSexp reg, SAtom $ show isTail, toSexp n, SList $ map toSexp args]
  toSexp (OP reg op args) = SList $ [SAtom "OP", toSexp reg, SAtom $ show op] ++ map toSexp (toList args)
  toSexp (APPLY reg f a) = shelper "APPLY" [reg, f, a]
  toSexp (CASE reg alts def) = SList ([SAtom "CASE", toSexp reg, defaultCase def] ++ (map altToSexp alts)) where
    defaultCase : Maybe (List VMInst) -> Sexp
    defaultCase def = case def of
                           Nothing => SList [SAtom "nodefault"]
                           Just insts => SList [SAtom "default", SList $ assert_total $ map toSexp insts]

    altToSexp : (Int, List VMInst) -> Sexp
    altToSexp (i, insts) = SList [SAtom $ show i, SList $ assert_total $ map toSexp insts]
  toSexp (CONSTCASE reg alts def) = SList ([SAtom "CONSTCASE", toSexp reg, defaultCase def] ++ (map altToSexp alts)) where
    defaultCase : Maybe (List VMInst) -> Sexp
    defaultCase def = case def of
                           Nothing => SList [SAtom "nodefault"]
                           Just insts => SList [SAtom "default", SList $ assert_total $ map toSexp insts]

    altToSexp : (Constant, List VMInst) -> Sexp
    altToSexp (c, insts) = SList [toSexp c, SList $ assert_total $ map toSexp insts]
  toSexp (PROJECT reg val pos) = SList [SAtom "PROJECT", toSexp reg, toSexp val, SAtom $ show pos]
  toSexp u = SList [SAtom "not-implemented", SAtom ("\"" ++ (assert_total $ show u) ++ "\"")]
  {-toSexp ASSIGN = SList -}

collectFromSexp : FromSexp a => List Sexp -> Either String (List a)
{-collectFromSexp s = Right $ rights $ map fromSexp s-}
{-collectFromSexp s = sequence $ map fromSexp s-}
collectFromSexp s = traverse fromSexp s

export
FromSexp VMInst where
  fromSexp (SList [SAtom "DECLARE", r]) = fromSexp r >>= pure . DECLARE
  fromSexp (SList [SAtom "START"]) = Right START
  fromSexp (SList [SAtom "ASSIGN", d, s]) = do
    pd <- fromSexp d
    ps <- fromSexp s
    pure $ ASSIGN pd ps
  fromSexp (SList [SAtom "MKCON", regS, SAtom tagS, SList argsS]) = do
    reg <- fromSexp regS
    let tag = cast tagS
    args <- collectFromSexp argsS
    pure $ MKCON reg tag args
  fromSexp (SList [SAtom "MKCLOSURE", regS, nameS, SAtom missingStr, SList argsS]) = do
    reg <- fromSexp regS
    name <- fromSexp nameS
    args <- collectFromSexp argsS
    pure $ MKCLOSURE reg name (cast missingStr) args
  fromSexp (SList [SAtom "MKCONSTANT", regS, constS]) = do
    reg <- fromSexp regS
    const <- fromSexp constS
    pure $ MKCONSTANT reg const
  fromSexp (SList [SAtom "CALL", regS, tailS, nameS, SList argsS]) = do
    reg <- fromSexp regS
    name <- fromSexp nameS
    tail <- fromSexp tailS
    args <- collectFromSexp argsS
    pure $ CALL reg tail name args
  fromSexp (SList ((SAtom "OP")::regS::(SAtom name)::argsS)) = do
    reg <- fromSexp regS
    args <- collectFromSexp argsS
    pure $ OP reg name args
  fromSexp (SList [SAtom "APPLY", regS, fS, argS]) = do
    reg <- fromSexp regS
    f <- fromSexp fS
    arg <- fromSexp argS
    pure $ APPLY reg f arg
  fromSexp (SList ((SAtom "CONSTCASE")::regS::defaultS::altsS)) =
    (do
      reg <- fromSexp regS
      default <- assert_total $ readDefault defaultS
      pure $ CONSTCASE reg (assert_total $ rights $ map readAlt altsS) default
      )
      where
        readDefault : Sexp -> Either String (Maybe (List VMInst))
        readDefault (SList [SAtom "nodefault"]) = Right Nothing
        readDefault (SList [SAtom "default", SList is]) = do
          insts <- collectFromSexp is
          pure $ Just insts
        readDefault _ = Right Nothing --Left "invalid default"
        readAlt : Sexp -> Either String (Constant, (List VMInst))
        readAlt (SList [SAtom c, SList is]) = do
          insts <- collectFromSexp is
          pure $ (I $ cast c, insts)
        readAlt _ = Left $ "error in alt"
  fromSexp (SList ((SAtom "CASE")::regS::defaultS::altsS)) =
    (do
      reg <- fromSexp regS
      default <- assert_total $ readDefault defaultS
      pure $ CASE reg [] default
      )
      where
        readDefault : Sexp -> Either String (Maybe (List VMInst))
        readDefault (SList [SAtom "nodefault"]) = Right Nothing
        readDefault (SList [SAtom "default", SList is]) = do
          insts <- collectFromSexp is
          pure $ Just insts
        readDefault _ = Right Nothing --Left "invalid default"
  fromSexp (SList ((SAtom "EXTPRIM")::regS::(SAtom name)::(SList argsS)::[])) = do
    reg <- fromSexp regS
    args <- collectFromSexp argsS
    pure $ EXTPRIM reg name args
  fromSexp sexp = Left $ "vminst not impl" ++ show sexp

public export
ToSexp VMDef where
  toSexp (MkVMFun args insts) = SList $ [SAtom "fun", SList $ map (\i => SAtom $ "v" ++ show i) args, SList $ map toSexp insts]
  toSexp (MkVMError insts) = SList $ [SAtom "error", SList $ map toSexp insts]

public export
ToSexp (String, VMDef) where
  toSexp (n, (MkVMFun args insts)) = SList $ [SAtom "defun", toSexp n, SList $ map (\i => SAtom $ "v" ++ show i) args, SList $ map toSexp insts]
  toSexp (n, (MkVMError insts)) = SList $ [SAtom "deferr", toSexp n, SList $ map toSexp insts]

{-listFromSexp : {FromSexp a} => FromSexp (List a)-}
{-listFromSexp {fa} =-}
  {-let mapped = map fromSexp s-}

{-export-}
{-FromSexp a => FromSexp (List a) where-}
  {-fromSexp (SAtom _) = Left "not a list"-}
  {-fromSexp s = let mapped = map fromSexp s in-}
                   {-?fmmm-}

export
FromSexp (String, VMDef) where
  fromSexp (SList [SAtom "defun", SAtom n, SList args, SList insts]) = do
    fArgs <- collectFromSexp args
    fInsts <- collectFromSexp insts
    pure (n, MkVMFun fArgs fInsts)
  fromSexp _ = Left "invalid vmdef"

export
Show VMDef where
  show (MkVMFun args body) = show args ++ ": \n  " ++ (showSep "\n  " (map show body)) ++ "\n"
  show (MkVMError err) = "Error: " ++ show err

interface ToIR a where
  toIR : a -> String

ToIR Reg where
  toIR (Loc i) = "%v" ++ show i
  toIR RVal = "%rval"
  toIR Discard = "undef"

ToIR String where
  toIR = id


argIR : Reg -> Codegen String
argIR (Loc i) = pure $ "%ObjPtr %v" ++ show i
argIR _ = pure $ "undef"

mkVarName : String -> Codegen String
mkVarName pfx = do
  i <- getUnique
  pure $ (pfx ++ show i)

data Value = MkConst Int
           | MkExtractValue Value Int

assignSSA : ToIR a => a -> Codegen String
assignSSA value = do
  i <- getUnique
  let varname = "%t" ++ show i
  appendCode ("  " ++ varname ++ " = " ++ (toIR value))
  pure varname


heapAllocate : Int -> Codegen String
heapAllocate size = do
  su <- show <$> getUnique
  let totalSize = (cast HEADER_SIZE) + size
  allocated <- assignSSA $ "call hhvmcc %Return1 @rapid_allocate(%RuntimePtr %HpArg, i64 "++(show totalSize)++", %RuntimePtr %BaseArg, i64 undef, %RuntimePtr %HpLimArg) alwaysinline optsize nounwind"
  appendCode $ "  %new.Hp."++su++" = extractvalue %Return1 " ++ allocated ++ ", 0
  store %RuntimePtr %new.Hp."++su++", %RuntimePtr* %HpVar
  %new.Base."++su++" = extractvalue %Return1 " ++ allocated ++ ", 1
  store %RuntimePtr %new.Base."++su++", %RuntimePtr* %BaseVar
  %new.HpLim."++su++" = extractvalue %Return1 " ++ allocated ++ ", 2
  store %RuntimePtr %new.HpLim."++su++", %RuntimePtr* %HpLimVar
  %newobj"++su++" = extractvalue %Return1 " ++ allocated ++ ", 3"
  pure $ "%newobj" ++ su

cgMkInt : String -> Codegen String
cgMkInt var = do
  newObj <- heapAllocate 8
  su <- mkVarName "mkint"
  appendCode $ "  %"++su++" = bitcast %ObjPtr " ++ newObj ++ " to i64*
  store i64 4294967296, i64* %"++su++"
  %"++su++".payloadPtr = getelementptr i64, i64* %" ++ su ++ ", i64 1
  store i64 "++ var ++", i64* %"++su++".payloadPtr"
  pure newObj

enumerate : List a -> List (Int, a)
enumerate l = enumerate' 0 l where
  enumerate' : Int -> List a -> List (Int, a)
  enumerate' _ [] = []
  enumerate' i (x::xs) = (i, x)::(enumerate' (i+1) xs)

mkCon : Int -> List Reg -> Codegen String
mkCon tag [] = do
  newObj <- heapAllocate 0
  i64Ptr <- assignSSA $ "bitcast %ObjPtr " ++ newObj ++ " to i64*"
  appendCode $ "  store i64 " ++ (show tag) ++ ", i64* " ++ i64Ptr
  pure newObj
mkCon tag args = do
  newObj <- heapAllocate $ cast (8 * (length args))
  i64Ptr <- assignSSA $ "bitcast %ObjPtr " ++ newObj ++ " to i64*"
  appendCode $ "  store i64 " ++ (show tag) ++ ", i64* " ++ i64Ptr
  payloadPtr <- assignSSA $ "bitcast %ObjPtr " ++ newObj ++ " to %ObjPtr*"
  let enumArgs = enumerate args
  for enumArgs (\x => let i = fst x
                          arg = snd x in do
                            argptr <- assignSSA $ "getelementptr %ObjPtr, %ObjPtr* " ++ payloadPtr ++ ", i64 " ++ (show (8+i*8))
                            tmp <- assignSSA $ "load %ObjPtr, %ObjPtr* " ++ (toIR arg) ++ "Var"
                            appendCode $ "store %ObjPtr " ++ tmp ++ ", %ObjPtr* " ++ argptr
                          )
  pure newObj

unboxInt : String -> Codegen String
unboxInt src = do
  su <- show <$> getUnique
  appendCode $ "
  %val" ++ su ++ " = load %ObjPtr, %ObjPtr* " ++ src ++ "Var
  %val.payload" ++ su ++ " = getelementptr i8, i8* %val" ++ su ++ ", i64 8
  %val.payload.cast" ++ su ++ " = bitcast i8* %val.payload" ++ su ++ " to i64*"
  assignSSA $ "load i64, i64* %val.payload.cast" ++ su ++ "\n"

makeCaseLabel : String -> (Constant, a) -> String
makeCaseLabel caseId (I i,_) = "i64 " ++ show i ++ ", label %" ++ caseId ++ "_is_" ++ show i
makeCaseLabel caseId (c,_) = "case error: " ++ show c

instrAsComment : VMInst -> String
instrAsComment i = ";" ++ (unwords $ lines $ show i)

getInstIR : VMInst -> Codegen ()
getInstIR (DECLARE (Loc i)) = appendCode $ "  %v" ++ show i ++ "Var = alloca %ObjPtr"
getInstIR (OP r "+Int" [r1, r2]) = do
  i1 <- unboxInt (toIR r1)
  i2 <- unboxInt (toIR r2)
  vsum <- assignSSA $ "add i64 " ++ i1 ++ ", " ++ i2
  obj <- cgMkInt vsum
  appendCode $ "  store %ObjPtr " ++ obj ++ ", %ObjPtr* " ++ toIR r ++ "Var"

getInstIR (OP r "==Int" [r1, r2]) = do
  i1 <- unboxInt (toIR r1)
  i2 <- unboxInt (toIR r2)
  vsum_i1 <- assignSSA $ "icmp eq i64 " ++ i1 ++ ", " ++ i2
  vsum_i64 <- assignSSA $ "zext i1 " ++ vsum_i1 ++ " to i64"
  obj <- cgMkInt vsum_i64
  appendCode $ "  store %ObjPtr " ++ obj ++ ", %ObjPtr* " ++ toIR r ++ "Var"
getInstIR (MKCON r tag args) = do
  obj <- mkCon tag args
  appendCode $ "  store %ObjPtr " ++ obj ++ ", %ObjPtr* " ++ toIR r ++ "Var"
getInstIR (MKCONSTANT r (I c)) = do
  obj <- cgMkInt $ show c
  appendCode $ "  store %ObjPtr " ++ obj ++ ", %ObjPtr* " ++ toIR r ++ "Var"
getInstIR (MKCONSTANT r (Str s)) = do
  let len = length s
  cn <- addConstant $ "private unnamed_addr constant [" ++ show len ++ " x i8] c" ++ (show s) ++ ""
  pure ()
getInstIR (CONSTCASE r alts def) =
  do let def' = fromMaybe [] def
     caseId <- mkVarName "case_"
     let labelEnd = caseId ++ "_end"
     scrutinee <- unboxInt (toIR r)
     appendCode $ "  switch i64 " ++ scrutinee ++ ", label %" ++ caseId ++ "_default [ " ++ (showSep "\n      " (map (makeCaseLabel caseId) alts)) ++ " ]"
     appendCode $ caseId ++ "_default:"
     traverse getInstIR def'
     appendCode $ "br label %" ++ labelEnd
     traverse (makeCaseAlt caseId) alts
     appendCode $ labelEnd ++ ":"
     pure ()
  where
    makeCaseAlt : String -> (Constant, List VMInst) -> Codegen ()
    makeCaseAlt caseId (I c, is) = do
      appendCode $ caseId ++ "_is_" ++ (show c) ++ ":"
      traverse_ getInstIR is
      appendCode $ "br label %" ++ caseId ++ "_end"
    makeCaseAlt _ (c, _) = appendCode $ "ERROR: constcase must be Int, got: " ++ show c

getInstIR START = pure ()
getInstIR _ = appendCode "; NOT IMPLEMENTED"

prepareArgCallConv' : List String -> List String
prepareArgCallConv' (a1::a2::rest) = ["%RuntimePtr %HpArg", a1, "%RuntimePtr %BaseArg", a2, "%RuntimePtr %HpLimArg"] ++ rest
prepareArgCallConv' _ = idris_crash "impossible"

prepareArgCallConv : List String -> List String
prepareArgCallConv [] = prepareArgCallConv' (["i64 %unused1", "i64 %unused2"])
prepareArgCallConv [x] = prepareArgCallConv' ([x, "i64 %unused1"])
prepareArgCallConv l = prepareArgCallConv' l

funcEntry : String
funcEntry = "
  %HpVar = alloca %RuntimePtr
  store %RuntimePtr %HpArg, %RuntimePtr* %HpVar
  %HpLimVar = alloca %RuntimePtr
  store %RuntimePtr %HpLimArg, %RuntimePtr* %HpLimVar
  %BaseVar = alloca %RuntimePtr
  store %RuntimePtr %BaseArg, %RuntimePtr* %BaseVar
  %rvalVar = alloca %ObjPtr
"

funcReturn : String
funcReturn = "
  %FinHp = load %RuntimePtr, %RuntimePtr* %HpVar
  %FinHpLim = load %RuntimePtr, %RuntimePtr* %HpLimVar
  %FinBase = load %RuntimePtr, %RuntimePtr* %BaseVar
  %FinRVal = load %ObjPtr, %ObjPtr* %rvalVar

  %ret0 = insertvalue %Return1 undef, %RuntimePtr %FinHp, 0
  %ret1 = insertvalue %Return1 %ret0, %RuntimePtr %FinBase, 1
  %ret2 = insertvalue %Return1 %ret1, %RuntimePtr %FinHpLim, 2
  %ret3 = insertvalue %Return1 %ret2, %ObjPtr %FinRVal, 3
  ret %Return1 %ret3
"

getFunIR : String -> List Reg -> List VMInst -> Codegen ()
getFunIR n args body = do
    fargs <- traverse argIR args
    appendCode ("\n\ndefine hhvmcc %Return1 @" ++ (safeName n) ++ "(" ++ (showSep ", " $ prepareArgCallConv fargs) ++ ") {")
    appendCode "entry:"
    appendCode funcEntry
    traverse_ appendCode (map copyArg args)
    for body (\i => appendCode (instrAsComment i) *> getInstIR i)
    appendCode funcReturn
    appendCode "}\n"
  where
    copyArg : Reg -> String
    copyArg (Loc i) = let r = show i in "
  %v" ++ r ++ "Var = alloca %ObjPtr
  store %ObjPtr %v" ++ r ++ ", %ObjPtr* %v" ++ r ++ "Var
"
    copyArg _ = idris_crash "not an argument"

export
getVMIR : (String, VMDef) -> String
getVMIR (n, MkVMFun args body) = runCodegen $ getFunIR n args body
getVMIR _ = ""
