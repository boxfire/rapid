module Compiler.SteamCG

import Data.Either
import Data.List
import Data.Maybe
import Data.Strings
import Data.Vect

import Compiler.VMCode
import Core.TT

import Codegen
import Data.Sexp
import Utils.Hex

%default partial

HEADER_SIZE : String
HEADER_SIZE = "8"

OBJECT_TYPE_ID_CON_NO_ARGS : Int
OBJECT_TYPE_ID_CON_NO_ARGS = 0

OBJECT_TYPE_ID_INT : Int
OBJECT_TYPE_ID_INT = 1

OBJECT_TYPE_ID_STR : Int
OBJECT_TYPE_ID_STR = 2

OBJECT_TYPE_ID_CLOSURE : Int
OBJECT_TYPE_ID_CLOSURE = 3

OBJECT_TYPE_ID_CHAR : Int
OBJECT_TYPE_ID_CHAR = 4

--showSep : String -> List String -> String
--showSep s xs = go xs where
  --go' : List String -> String
  --go' xs = concat $ map ((++) s) xs

  --go : List String -> String
  --go [] = ""
  --go [x] = x
  --go (x::rest) = x ++ go' rest

repeatStr : String -> Nat -> String
repeatStr s 0 = ""
repeatStr s (S x) = s ++ repeatStr s x

fullShow : Name -> String
fullShow (DN _ n) = fullShow n
fullShow (NS ns n) = showSep "." (reverse ns) ++ "." ++ fullShow n
fullShow n = show n

safeName : Name -> String
safeName s = concatMap okchar (unpack $ fullShow s)
  where
    okchar : Char -> String
    okchar c = if isAlphaNum c
                  then cast c
                  else "_" ++ asHex (cast {to=Int} c) ++ "_"

safeName' : String -> String
safeName' s = concatMap okchar (unpack s)
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

data IRType = I1 | I8 | I64 | FuncPtr | IRObjPtr | Pointer IRType

Show IRType where
  show I1 = "i1"
  show I8 = "i8"
  show I64 = "i64"
  show FuncPtr = "%FuncPtr"
  show IRObjPtr = "%ObjPtr"
  show (Pointer t) = (show t) ++ "*"

data IRValue : IRType -> Type where
  ConstI64 : Int -> IRValue I64
  SSA : (t : IRType) ->  String -> IRValue t

showWithoutType : IRValue a -> String
showWithoutType (SSA _ n) = n
showWithoutType (ConstI64 i) = show i

ToIR (IRValue t) where
  toIR {t} (SSA t s) = (show t) ++ " " ++ s
  toIR (ConstI64 i) = "i64 " ++ (show i)


data MemValue : IRType -> Type where
  MkMemValue : (t : IRType) -> String -> MemValue t

ToIR (MemValue t) where
  toIR (MkMemValue t n) = (show t) ++ " " ++ n

reg2mem : Reg -> MemValue (Pointer IRObjPtr)
reg2mem (Loc i) = MkMemValue (Pointer IRObjPtr) ("%v" ++ show i ++ "Var")
reg2mem RVal = MkMemValue (Pointer IRObjPtr) ("%rvalVar")
reg2mem _ = MkMemValue (Pointer IRObjPtr) "undef"

reg2val : Reg -> IRValue (Pointer IRObjPtr)
reg2val (Loc i) = SSA (Pointer IRObjPtr) ("%v" ++ show i ++ "Var")
reg2val RVal = SSA (Pointer IRObjPtr) ("%rvalVar")
reg2val _ = SSA (Pointer IRObjPtr) "undef"

load : {auto t : IRType} -> IRValue (Pointer t) -> Codegen (IRValue t)
load {t} mv = do
  loaded <- assignSSA $ "load " ++ (show t) ++ ", " ++ (toIR mv)
  pure $ SSA t loaded

icmp : {t : IRType} -> String -> IRValue t -> IRValue t -> Codegen (IRValue I1)
icmp {t} cond a b = do
  compare <- assignSSA $ "icmp " ++ cond ++ " " ++ (show t) ++ " " ++ showWithoutType a ++ ", " ++ showWithoutType b
  pure $ SSA I1 compare

--and : Value -> Value -> CodeGen Value
--and v1 v1 = toIR v1 ++ toIR v2

getObjectSlot : IRValue IRObjPtr -> Int -> Codegen (IRValue I64)
getObjectSlot obj n = do
  i64ptr <- assignSSA $ "bitcast " ++ toIR obj ++ " to i64*"
  slotPtr <- assignSSA $ "getelementptr i64, i64* " ++ i64ptr ++ ", i64 " ++ show n
  load {t=I64} (SSA (Pointer I64) slotPtr)

getObjectSlotT : {t : IRType} -> IRValue IRObjPtr -> Int -> Codegen (IRValue t)
getObjectSlotT obj n = do
  i64ptr <- assignSSA $ "bitcast " ++ toIR obj ++ " to i64*"
  slotPtr <- assignSSA $ "getelementptr i64, i64* " ++ i64ptr ++ ", i64 " ++ show n
  slotPtrT <- assignSSA $ "bitcast i64* " ++ slotPtr ++ " to " ++ show t ++ "*"
  load {t=t} (SSA (Pointer t) slotPtrT)

putObjectSlot : {t : IRType} -> IRValue IRObjPtr -> Int -> IRValue t -> Codegen ()
putObjectSlot {t} obj n val = do
  i64ptr <- assignSSA $ "bitcast " ++ toIR obj ++ " to i64*"
  slotPtr <- assignSSA $ "getelementptr i64, i64* " ++ i64ptr ++ ", i64 " ++ show n
  slotPtrObj <- assignSSA $ "bitcast i64* " ++ slotPtr ++ " to " ++ show t ++ "*"
  appendCode $ "  store " ++ toIR val ++ ", " ++ show t ++ " * " ++ slotPtrObj

heapAllocate : Int -> Codegen String
heapAllocate size = do
  su <- show <$> getUnique
  let totalSize = (cast HEADER_SIZE) + size

  hp <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpVar"
  hpLim <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpLimVar"
  let base = "%RuntimePtr %BaseArg"

  allocated <- assignSSA $ "call hhvmcc %Return1 @rapid_allocate(" ++ showSep ", " [hp, base, hpLim] ++ ", i64 "++(show totalSize)++") alwaysinline optsize nounwind"
  newHp <- assignSSA $ "extractvalue %Return1 " ++ allocated ++ ", 0"
  appendCode $ "store %RuntimePtr " ++ newHp ++ ", %RuntimePtr* %HpVar"
  newHpLim <- assignSSA $ "extractvalue %Return1 " ++ allocated ++ ", 1"
  appendCode $ "store %RuntimePtr " ++ newHpLim ++ ", %RuntimePtr* %HpLimVar"
  newObj <- assignSSA $ "extractvalue %Return1 " ++ allocated ++ ", 2"
  pure $ newObj

applyClosureHelperFunc : Codegen ()
applyClosureHelperFunc = do
  let maxArgs = 7

  --closureObj <- load {t=IRObjPtr} (MkMemValue (Pointer IRObjPtr) "%closureObjArg")
  let closureObj = SSA IRObjPtr "%closureObjArg"
  let argValue = SSA IRObjPtr "%argumentObjArg"
  header <- getObjectSlot closureObj 0
  argCount <- assignSSA $ "and i64 65535, " ++ showWithoutType header
  missingArgCountShifted <- assignSSA $ "and i64 4294901760, " ++ showWithoutType header
  missingArgCount <- assignSSA $ "lshr i64 " ++ missingArgCountShifted ++ ", 16"
  isSaturated <- assignSSA $ "icmp eq i64 1, " ++ missingArgCount
  labelName <- mkVarName "closure_saturated"
  appendCode $ "br i1 " ++ isSaturated ++ ", label %" ++ labelName ++ "_yes, label %" ++ labelName ++ "_no"
  appendCode $ labelName ++ "_yes:"
  funcPtrI64 <- getObjectSlot closureObj 1
  func <- assignSSA $ "inttoptr " ++ (toIR funcPtrI64) ++ " to %FuncPtrArgs" ++ show (maxArgs+1)

  --hp <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpVar"
  --hpLim <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpLimVar"
  let hp = "%RuntimePtr %HpArg"
  let base = "%RuntimePtr %BaseArg"
  let hpLim = "%RuntimePtr %HpLimArg"

  --argValue <- load {t=IRObjPtr} (MkMemValue (Pointer IRObjPtr) "%argumentObjArg")

  applyClosure <- mkVarName "apply_closure_"
  appendCode $ "  switch i64 " ++ argCount ++ ", label %" ++ applyClosure ++ "_error [\n  " ++
  (showSep "\n  " $ (flip map) (rangeFromTo 0 maxArgs) (\i => "i64 " ++ show i ++ ", label %" ++ applyClosure ++ "_" ++ show i)) ++
  "]"

  for_ (rangeFromTo 0 maxArgs) (\i => do
    let labelName = applyClosure ++ "_" ++ show i
    appendCode $ labelName ++ ":"
    storedArgs <- for (rangeFromThenTo 0 1 (i-1)) (\i => do
                      argItem <- getObjectSlotT {t=IRObjPtr} closureObj (2+i)
                      pure $ (toIR argItem)
                      )
    let argList = [hp, base, hpLim] ++ storedArgs ++ [toIR argValue]
    --appendCode $ labelName ++ "_do_call:"
    let undefs = repeatStr ", %ObjPtr undef" (minus (integerToNat $ cast maxArgs) (integerToNat $ cast i))
    callRes <- assignSSA $ "musttail call hhvmcc %Return1 " ++ func ++ "(" ++ (showSep ", " argList) ++ undefs ++ ")"
    appendCode $ "ret %Return1 " ++ callRes
    )
  appendCode $ labelName ++ "_no:"
  appendCode $ "br label %" ++ applyClosure ++ "_error"
  appendCode $ applyClosure ++ "_error:"
  appendCode $ "call ccc void @idris_rts_crash(i64 42)"
  appendCode $ "ret %Return1 undef"

header : Int -> Integer
header i = (cast i) `prim__shl_Integer` 32

cgMkInt : String -> Codegen String
cgMkInt var = do
  newObj <- heapAllocate 8
  su <- mkVarName "mkint"
  appendCode ("  %"++su++" = bitcast %ObjPtr " ++ newObj ++ " to i64*\n  store i64 " ++ (show $ header OBJECT_TYPE_ID_INT) ++ ", i64* %"++su++"\n  %"++su++".payloadPtr = getelementptr i64, i64* %" ++ su ++ ", i64 1\n  store i64 "++ var ++", i64* %"++su++".payloadPtr")
  pure newObj

export
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

prepareArgCallConv' : List String -> List String
prepareArgCallConv' rest = ["%RuntimePtr %HpArg", "%RuntimePtr %BaseArg", "%RuntimePtr %HpLimArg"] ++ rest

prepareArgCallConv : List String -> List String
--prepareArgCallConv [] = prepareArgCallConv' (["%ObjPtr %unused1", "%ObjPtr %unused2"])
--prepareArgCallConv [x] = prepareArgCallConv' ([x, "%ObjPtr %unused1"])
prepareArgCallConv l = prepareArgCallConv' l

prepareArg : Reg -> Codegen String
prepareArg Discard = pure $ "%ObjPtr undef"
prepareArg (Loc i) = do
  tmp <- assignSSA $ "load %ObjPtr, %ObjPtr* %v" ++ (show i) ++ "Var"
  pure $ "%ObjPtr " ++ tmp
prepareArg RVal = idris_crash "cannot use rval as call arg"

asHex2 : Int -> String
asHex2 c = let s = asHex c in
               if length s == 1 then "0" ++ s else s

getStringIR : String -> String
getStringIR s = concatMap okchar (unpack s)
  where
    okchar : Char -> String
    okchar c = if isAlphaNum c
                  then cast c
                  else "\\" ++ asHex2 (cast {to=Int} c)


getInstIR : Int -> VMInst -> Codegen ()
getInstIR i (DECLARE (Loc r)) = appendCode $ "  %v" ++ show r ++ "Var = alloca %ObjPtr"
getInstIR i (ASSIGN r src) = do
  value <- assignSSA $ "load %ObjPtr, %ObjPtr* " ++ toIR src ++ "Var"
  appendCode $ "  store %ObjPtr " ++ value ++ ", %ObjPtr* " ++ toIR r ++ "Var"
getInstIR i (OP r (Add IntType) [r1, r2]) = do
  i1 <- unboxInt (toIR r1)
  i2 <- unboxInt (toIR r2)
  vsum <- assignSSA $ "add i64 " ++ i1 ++ ", " ++ i2
  obj <- cgMkInt vsum
  appendCode $ "  store %ObjPtr " ++ obj ++ ", %ObjPtr* " ++ toIR r ++ "Var"
getInstIR i (OP r (Sub IntType) [r1, r2]) = do
  i1 <- unboxInt (toIR r1)
  i2 <- unboxInt (toIR r2)
  vsum <- assignSSA $ "sub i64 " ++ i1 ++ ", " ++ i2
  obj <- cgMkInt vsum
  appendCode $ "  store %ObjPtr " ++ obj ++ ", %ObjPtr* " ++ toIR r ++ "Var"

getInstIR i (OP r (Add IntegerType) [r1, r2]) = do
  -- FIXME: we treat Integers as bounded Ints -> should use GMP
  i1 <- unboxInt (toIR r1)
  i2 <- unboxInt (toIR r2)
  vsum <- assignSSA $ "add i64 " ++ i1 ++ ", " ++ i2
  obj <- cgMkInt vsum
  appendCode $ "  store %ObjPtr " ++ obj ++ ", %ObjPtr* " ++ toIR r ++ "Var"
getInstIR i (OP r (Sub IntegerType) [r1, r2]) = do
  -- FIXME: we treat Integers as bounded Ints -> should use GMP
  i1 <- unboxInt (toIR r1)
  i2 <- unboxInt (toIR r2)
  vsum <- assignSSA $ "sub i64 " ++ i1 ++ ", " ++ i2
  obj <- cgMkInt vsum
  appendCode $ "  store %ObjPtr " ++ obj ++ ", %ObjPtr* " ++ toIR r ++ "Var"
getInstIR i (OP r (Mul IntegerType) [r1, r2]) = do
  -- FIXME: we treat Integers as bounded Ints -> should use GMP
  i1 <- unboxInt (toIR r1)
  i2 <- unboxInt (toIR r2)
  vsum <- assignSSA $ "mul i64 " ++ i1 ++ ", " ++ i2
  obj <- cgMkInt vsum
  appendCode $ "  store %ObjPtr " ++ obj ++ ", %ObjPtr* " ++ toIR r ++ "Var"

getInstIR i (OP r (EQ CharType) [r1, r2]) = do
  -- Two Chars are equal, when their headers are equal
  o1 <- load (reg2val r1)
  o2 <- load (reg2val r2)
  i1 <- getObjectSlotT {t=I64} o1 0
  i2 <- getObjectSlotT {t=I64} o2 0
  cmp_i1 <- icmp "eq" i1 i2
  cmp_i64 <- assignSSA $ "zext " ++ toIR cmp_i1 ++ " to i64"
  obj <- cgMkInt cmp_i64
  appendCode $ "  store %ObjPtr " ++ obj ++ ", %ObjPtr* " ++ toIR r ++ "Var"
getInstIR i (OP r (EQ IntType) [r1, r2]) = do
  i1 <- unboxInt (toIR r1)
  i2 <- unboxInt (toIR r2)
  vsum_i1 <- assignSSA $ "icmp eq i64 " ++ i1 ++ ", " ++ i2
  vsum_i64 <- assignSSA $ "zext i1 " ++ vsum_i1 ++ " to i64"
  obj <- cgMkInt vsum_i64
  appendCode $ "  store %ObjPtr " ++ obj ++ ", %ObjPtr* " ++ toIR r ++ "Var"
getInstIR i (OP r (EQ IntegerType) [r1, r2]) = do
  -- FIXME: we treat Integers as bounded Ints -> should use GMP
  i1 <- unboxInt (toIR r1)
  i2 <- unboxInt (toIR r2)
  vsum_i1 <- assignSSA $ "icmp eq i64 " ++ i1 ++ ", " ++ i2
  vsum_i64 <- assignSSA $ "zext i1 " ++ vsum_i1 ++ " to i64"
  obj <- cgMkInt vsum_i64
  appendCode $ "  store %ObjPtr " ++ obj ++ ", %ObjPtr* " ++ toIR r ++ "Var"
getInstIR i (OP r (GT IntegerType) [r1, r2]) = do
  -- FIXME: we treat Integers as bounded Ints -> should use GMP
  i1 <- unboxInt (toIR r1)
  i2 <- unboxInt (toIR r2)
  vsum_i1 <- assignSSA $ "icmp sgt i64 " ++ i1 ++ ", " ++ i2
  vsum_i64 <- assignSSA $ "zext i1 " ++ vsum_i1 ++ " to i64"
  obj <- cgMkInt vsum_i64
  appendCode $ "  store %ObjPtr " ++ obj ++ ", %ObjPtr* " ++ toIR r ++ "Var"
getInstIR i (OP r (GTE IntegerType) [r1, r2]) = do
  -- FIXME: we treat Integers as bounded Ints -> should use GMP
  i1 <- unboxInt (toIR r1)
  i2 <- unboxInt (toIR r2)
  vsum_i1 <- assignSSA $ "icmp sge i64 " ++ i1 ++ ", " ++ i2
  vsum_i64 <- assignSSA $ "zext i1 " ++ vsum_i1 ++ " to i64"
  obj <- cgMkInt vsum_i64
  appendCode $ "  store %ObjPtr " ++ obj ++ ", %ObjPtr* " ++ toIR r ++ "Var"
getInstIR i (OP r (LT IntegerType) [r1, r2]) = do
  -- FIXME: we treat Integers as bounded Ints -> should use GMP
  i1 <- unboxInt (toIR r1)
  i2 <- unboxInt (toIR r2)
  vsum_i1 <- assignSSA $ "icmp slt i64 " ++ i1 ++ ", " ++ i2
  vsum_i64 <- assignSSA $ "zext i1 " ++ vsum_i1 ++ " to i64"
  obj <- cgMkInt vsum_i64
  appendCode $ "  store %ObjPtr " ++ obj ++ ", %ObjPtr* " ++ toIR r ++ "Var"
getInstIR i (OP r (LTE IntegerType) [r1, r2]) = do
  -- FIXME: we treat Integers as bounded Ints -> should use GMP
  i1 <- unboxInt (toIR r1)
  i2 <- unboxInt (toIR r2)
  vsum_i1 <- assignSSA $ "icmp sle i64 " ++ i1 ++ ", " ++ i2
  vsum_i64 <- assignSSA $ "zext i1 " ++ vsum_i1 ++ " to i64"
  obj <- cgMkInt vsum_i64
  appendCode $ "  store %ObjPtr " ++ obj ++ ", %ObjPtr* " ++ toIR r ++ "Var"

getInstIR i (MKCON r (Just tag) args) = do
  obj <- mkCon tag args
  appendCode $ "  store %ObjPtr " ++ obj ++ ", %ObjPtr* " ++ toIR r ++ "Var"

getInstIR i (MKCLOSURE r n missing args) = do
  let len = length args
  let totalArgsExpected = missing + len
  let header = 0x300000000 + (missing * 0x10000) + len
  newObjName <- heapAllocate (8 + 8 * (cast len))
  let newObj = SSA IRObjPtr newObjName
  putObjectSlot newObj 0 (ConstI64 $ cast header)
  funcPtr <- assignSSA $ "bitcast %Return1 (%RuntimePtr,%RuntimePtr,%RuntimePtr" ++ (repeatStr ", %ObjPtr" totalArgsExpected) ++ ")* @" ++ (safeName n) ++ " to %FuncPtr"
  putObjectSlot newObj 1 (SSA FuncPtr funcPtr)
  for_ (enumerate args) (\iv => do
      let (i, arg) = iv
      argObj <- load {t=IRObjPtr} (reg2val arg)
      putObjectSlot newObj (i+2) argObj
      pure ()
                              )
  appendCode $ "  store " ++ toIR newObj ++ ", %ObjPtr* " ++ toIR r ++ "Var"

getInstIR i (APPLY r fun arg) = do
  hp <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpVar"
  hpLim <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpLimVar"
  let base = "%RuntimePtr %BaseArg"

  funV <- ((++) "%FuncPtr ") <$> (assignSSA $ "load %FuncPtr, %FuncPtr* " ++ toIR fun ++ "Var")
  argV <- ((++) "%FuncPtr ") <$> (assignSSA $ "load %FuncPtr, %FuncPtr* " ++ toIR arg ++ "Var")

  result <- assignSSA $ "call hhvmcc %Return1 @idris_apply_closure(" ++ showSep ", " [hp, base, hpLim, funV, argV] ++ repeatStr ", %ObjPtr undef" 6  ++ ")"

  newHp <- assignSSA $ "extractvalue %Return1 " ++ result ++ ", 0"
  appendCode $ "store %RuntimePtr " ++ newHp ++ ", %RuntimePtr* %HpVar"
  newHpLim <- assignSSA $ "extractvalue %Return1 " ++ result ++ ", 1"
  appendCode $ "store %RuntimePtr " ++ newHpLim ++ ", %RuntimePtr* %HpLimVar"
  returnValue <- assignSSA $ "extractvalue %Return1 " ++ result ++ ", 2"
  appendCode $ "store %ObjPtr " ++ returnValue ++ ", %ObjPtr* " ++ toIR r ++ "Var"

  pure ()

getInstIR i (MKCONSTANT r (I c)) = do
  obj <- cgMkInt $ show c
  appendCode $ "  store %ObjPtr " ++ obj ++ ", %ObjPtr* " ++ toIR r ++ "Var"
getInstIR i (MKCONSTANT r (BI c)) = do
  -- FIXME: we treat Integers as bounded Ints -> should use GMP
  obj <- cgMkInt $ show c
  appendCode $ "  store %ObjPtr " ++ obj ++ ", %ObjPtr* " ++ toIR r ++ "Var"
getInstIR i (MKCONSTANT r WorldVal) = do
  obj <- mkCon 1337 []
  appendCode $ "  store %ObjPtr " ++ obj ++ ", %ObjPtr* " ++ toIR r ++ "Var"
getInstIR i (MKCONSTANT r (Str s)) = do
  let len = length s
  cn <- addConstant i $ "private unnamed_addr constant [" ++ show len ++ " x i8] c\"" ++ (getStringIR s) ++ "\""
  cn <- assignSSA $ "bitcast [" ++ show len ++ " x i8]* "++cn++" to i8*"
  su <- mkVarName "mkStr"
  let header = 0x200000000 + len
  newObj <- heapAllocate (cast len)
  appendCode $ unlines [
    "  %"++su++" = bitcast %ObjPtr " ++ newObj ++ " to i64*",
    -- TODO: add string length in bytes to header
    "  store i64 " ++ show header ++", i64* %"++su,
    "  %"++su++".payloadPtr = getelementptr i64, i64* %" ++ su ++ ", i64 1",
    "  %"++su++".strPtr = bitcast i64* %" ++ su ++ ".payloadPtr to i8*",
    "  call void @llvm.memcpy.p0i8.p0i8.i32(i8* %" ++ su ++ ".strPtr, i8* "++cn++", i32 " ++show len ++", i1 false)"
    ]
  appendCode $ "  store %ObjPtr " ++ newObj ++ ", %ObjPtr* " ++ toIR r ++ "Var"
  pure ()
getInstIR i (CONSTCASE r alts def) =
  do let def' = fromMaybe [] def
     caseId <- mkVarName "case_"
     let labelEnd = caseId ++ "_end"
     scrutinee <- unboxInt (toIR r)
     appendCode $ "  switch i64 " ++ scrutinee ++ ", label %" ++ caseId ++ "_default [ " ++ (showSep "\n      " (map (makeCaseLabel caseId) alts)) ++ " ]"
     appendCode $ caseId ++ "_default:"
     uniq <- getUnique
     let nextI = uniq + (i * 100)
     traverse (getInstIR nextI) def'
     appendCode $ "br label %" ++ labelEnd
     traverse (makeCaseAlt caseId) alts
     appendCode $ labelEnd ++ ":"
     pure ()
  where
    makeCaseAlt : String -> (Constant, List VMInst) -> Codegen ()
    makeCaseAlt caseId (I c, is) = do
      appendCode $ caseId ++ "_is_" ++ (show c) ++ ":"
      uniq <- getUnique
      let nextI = uniq + (i * 100)
      traverse_ (getInstIR nextI) is
      appendCode $ "br label %" ++ caseId ++ "_end"
    makeCaseAlt _ (c, _) = appendCode $ "ERROR: constcase must be Int, got: " ++ show c

getInstIR i (CALL r tailpos n args) =
  do argsV <- traverse prepareArg args
     hp <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpVar"
     hpLim <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpLimVar"
     let base = "%RuntimePtr %BaseArg"
     result <- assignSSA $ "call hhvmcc %Return1 @" ++ (safeName n) ++ "(" ++ showSep ", " (hp::base::hpLim::argsV) ++ ")"

     newHp <- assignSSA $ "extractvalue %Return1 " ++ result ++ ", 0"
     appendCode $ "store %RuntimePtr " ++ newHp ++ ", %RuntimePtr* %HpVar"
     newHpLim <- assignSSA $ "extractvalue %Return1 " ++ result ++ ", 1"
     appendCode $ "store %RuntimePtr " ++ newHpLim ++ ", %RuntimePtr* %HpLimVar"
     returnValue <- assignSSA $ "extractvalue %Return1 " ++ result ++ ", 2"
     appendCode $ "store %ObjPtr " ++ returnValue ++ ", %ObjPtr* " ++ toIR r ++ "Var"
     pure ()

getInstIR i (EXTPRIM r n args) =
  do argsV <- traverse prepareArg args
     hp <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpVar"
     hpLim <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpLimVar"
     let base = "%RuntimePtr %BaseArg"
     result <- assignSSA $ "call hhvmcc %Return1 @_extprim_" ++ (safeName n) ++ "(" ++ showSep ", " (hp::base::hpLim::argsV) ++ ")"
     pure ()

getInstIR i START = pure ()
getInstIR i inst = appendCode $ ";=============\n; NOT IMPLEMENTED: " ++ show inst ++ "\n;=============\n"

funcEntry : String
funcEntry = "
  %HpVar = alloca %RuntimePtr
  store %RuntimePtr %HpArg, %RuntimePtr* %HpVar
  %HpLimVar = alloca %RuntimePtr
  store %RuntimePtr %HpLimArg, %RuntimePtr* %HpLimVar
  %rvalVar = alloca %ObjPtr
"

funcReturn : String
funcReturn = "
  %FinHp = load %RuntimePtr, %RuntimePtr* %HpVar
  %FinHpLim = load %RuntimePtr, %RuntimePtr* %HpLimVar
  %FinRVal = load %ObjPtr, %ObjPtr* %rvalVar

  %ret1 = insertvalue %Return1 undef, %RuntimePtr %FinHp, 0
  %ret2 = insertvalue %Return1 %ret1, %RuntimePtr %FinHpLim, 1
  %ret3 = insertvalue %Return1 %ret2, %ObjPtr %FinRVal, 2
  ret %Return1 %ret3
"

getFunIR : Int -> String -> List Reg -> List VMInst -> Codegen ()
getFunIR i n args body = do
    fargs <- traverse argIR args
    appendCode ("\n\ndefine hhvmcc %Return1 @" ++ (safeName' n) ++ "(" ++ (showSep ", " $ prepareArgCallConv fargs) ++ ") {")
    appendCode "entry:"
    appendCode funcEntry
    traverse_ appendCode (map copyArg args)
    for body (\instr => appendCode (instrAsComment instr) *> getInstIR i instr)
    appendCode funcReturn
    appendCode "}\n"
  where
    copyArg : Reg -> String
    copyArg (Loc i) = let r = show i in "\n  %v" ++ r ++ "Var = alloca %ObjPtr
  store %ObjPtr %v" ++ r ++ ", %ObjPtr* %v" ++ r ++ "Var
"
    copyArg _ = idris_crash "not an argument"

export
getVMIR : (Int, (String, VMDef)) -> String
getVMIR (i, n, MkVMFun args body) = runCodegen $ getFunIR i n (map Loc args) body
getVMIR _ = ""

export
closureHelper : String
closureHelper = "define hhvmcc %Return1 @idris_apply_closure(%RuntimePtr %HpArg, %RuntimePtr %BaseArg, %RuntimePtr %HpLimArg, %ObjPtr %closureObjArg, %ObjPtr %argumentObjArg, i8* %unused0, i8* %unused1, i8* %unused2, i8* %unused3, i8* %unused4, i8* %unused5) {\n" ++ runCodegen applyClosureHelperFunc ++ "\n}"
