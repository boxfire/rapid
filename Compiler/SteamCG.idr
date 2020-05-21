module Compiler.SteamCG

import Data.Either
import Data.List
import Data.Maybe
import Data.Strings

import Codegen
import Compiler.VMCode
import Core.TT
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
  appendCode ("  %"++su++" = bitcast %ObjPtr " ++ newObj ++ " to i64*\n  store i64 4294967296, i64* %"++su++"\n  %"++su++".payloadPtr = getelementptr i64, i64* %" ++ su ++ ", i64 1\n  store i64 "++ var ++", i64* %"++su++".payloadPtr")
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
  appendCode $ unlines [
    "  %val" ++ su ++ " = load %ObjPtr, %ObjPtr* " ++ src ++ "Var",
    "  %val.payload" ++ su ++ " = getelementptr i8, i8* %val" ++ su ++ ", i64 8",
    "  %val.payload.cast" ++ su ++ " = bitcast i8* %val.payload" ++ su ++ " to i64*"
    ]
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
    copyArg (Loc i) = let r = show i in "\n  %v" ++ r ++ "Var = alloca %ObjPtr
  store %ObjPtr %v" ++ r ++ ", %ObjPtr* %v" ++ r ++ "Var
"
    copyArg _ = idris_crash "not an argument"

export
getVMIR : (String, VMDef) -> String
getVMIR (n, MkVMFun args body) = runCodegen $ getFunIR n args body
getVMIR _ = ""
