module Compiler.SteamCG

import Data.Buffer
import Data.Either
import Data.List
import Data.Maybe
import Data.SortedMap
import Data.Strings
import Data.Vect

import Compiler.VMCode
import Core.TT

import Codegen
import Utils.Hex

%default partial

HEADER_SIZE : String
HEADER_SIZE = "8"

OBJECT_TYPE_ID_CON_NO_ARGS : Int
OBJECT_TYPE_ID_CON_NO_ARGS = 0

OBJECT_TYPE_ID_INT : Int
OBJECT_TYPE_ID_INT = 1

-- for now treat all numbers the same
OBJECT_TYPE_ID_DOUBLE : Int
OBJECT_TYPE_ID_DOUBLE = 1

OBJECT_TYPE_ID_STR : Int
OBJECT_TYPE_ID_STR = 2

OBJECT_TYPE_ID_CLOSURE : Int
OBJECT_TYPE_ID_CLOSURE = 3

OBJECT_TYPE_ID_CHAR : Int
OBJECT_TYPE_ID_CHAR = 4

OBJECT_TYPE_ID_IOREF : Int
OBJECT_TYPE_ID_IOREF = 5

OBJECT_TYPE_ID_BUFFER : Int
OBJECT_TYPE_ID_BUFFER = 6

CLOSURE_MAX_ARGS : Int
CLOSURE_MAX_ARGS = 1024

-- A "fat" closure is always invoked via its "closure entry" function
FAT_CLOSURE_LIMIT : Int
FAT_CLOSURE_LIMIT = 8

repeatStr : String -> Nat -> String
repeatStr s 0 = ""
repeatStr s (S x) = s ++ repeatStr s x

fullShow : Name -> String
fullShow (DN _ n) = fullShow n
fullShow (NS ns n) = showSep "." (reverse ns) ++ "." ++ fullShow n
fullShow n = show n

isSafeChar : Char -> Bool
isSafeChar '.' = True
isSafeChar '_' = True
isSafeChar c = isAlphaNum c

export
safeName : Name -> String
safeName s = concatMap okchar (unpack $ fullShow s)
  where
    okchar : Char -> String
    okchar c = if isSafeChar c
                  then cast c
                  else "$" ++ asHex (cast {to=Int} c)

interface ToIR a where
  toIR : a -> String
  showWithoutType : a -> String

ToIR Reg where
  toIR (Loc i) = "%v" ++ show i
  toIR RVal = "%rval"
  toIR Discard = "undef"

  showWithoutType (Loc i) = "%v" ++ show i
  showWithoutType RVal = "%rval"
  showWithoutType Discard = "undef"

ToIR String where
  toIR = id
  showWithoutType = id

argIR : Reg -> Codegen String
argIR (Loc i) = pure $ "%ObjPtr %v" ++ show i
argIR _ = pure $ "undef"

asHex2 : Int -> String
asHex2 0 = "00"
asHex2 c = let s = asHex c in
               if length s == 1 then "0" ++ s else s

doubleToHex : Double -> String
doubleToHex d = let bytes = unsafePerformIO (do
                                buf <- (fromMaybe $ idris_crash "no buf") <$> newBuffer 8
                                setDouble buf 0 d
                                bufferData buf
                                ) in
                                concatMap asHex2 $ reverse bytes

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

data IRType = I1 | I8 | I32 | I64 | F64 | FuncPtr | IRObjPtr | Pointer IRType

Show IRType where
  show I1 = "i1"
  show I8 = "i8"
  show I32 = "i32"
  show I64 = "i64"
  show F64 = "double"
  show FuncPtr = "%FuncPtr"
  show IRObjPtr = "%ObjPtr"
  show (Pointer t) = (show t) ++ "*"

data IRLabel = MkLabel String

ToIR IRLabel where
  toIR (MkLabel l) = "label %" ++ l
  showWithoutType (MkLabel l) = "%" ++ l

beginLabel : IRLabel -> Codegen ()
beginLabel (MkLabel l) = appendCode (l ++ ":")

genLabel : String -> Codegen IRLabel
genLabel s = MkLabel <$> mkVarName ("glbl_" ++ s)

data IRValue : IRType -> Type where
  Const : (t : IRType) -> Integer -> IRValue t
  ConstI64 : Integer -> IRValue I64
  ConstF64 : Double -> IRValue F64
  SSA : (t : IRType) ->  String -> IRValue t
  IRDiscard : IRValue (Pointer IRObjPtr)


total
ToIR (IRValue t) where
  toIR {t} (SSA t s) = (show t) ++ " " ++ s
  toIR (Const t i) = (show t) ++ " " ++ (show i)
  toIR (ConstI64 i) = "i64 " ++ (show i)
  toIR (ConstF64 f) = "double 0x" ++ (assert_total $ doubleToHex f)
  toIR (IRDiscard) = "ERROR: trying to use DISCARD with type"

  showWithoutType (SSA _ n) = n
  showWithoutType (Const _ i) = show i
  showWithoutType (ConstI64 i) = show i
  showWithoutType (ConstF64 f) = "0x" ++ (assert_total $ doubleToHex f)
  showWithoutType (IRDiscard) = "ERROR: trying to use DISCARD without type"

reg2val : Reg -> IRValue (Pointer IRObjPtr)
reg2val (Loc i) = SSA (Pointer IRObjPtr) ("%v" ++ show i ++ "Var")
reg2val RVal = SSA (Pointer IRObjPtr) ("%rvalVar")
reg2val Discard = IRDiscard -- idris_crash "trying to use DISCARD pseudo-register" --SSA (Pointer IRObjPtr) "undef"

load : {t : IRType} -> IRValue (Pointer t) -> Codegen (IRValue t)
-- can be changed to undef if we're not in SAFE mode
--load IRDiscard = pure $ SSA IRObjPtr "undef"
load IRDiscard = pure $ SSA IRObjPtr "null"
load {t} mv = do
  loaded <- assignSSA $ "load " ++ (show t) ++ ", " ++ (toIR mv)
  pure $ SSA t loaded

store : {t : IRType} -> IRValue t -> IRValue (Pointer t) -> Codegen ()
store _ IRDiscard = pure ()
store {t} v dst = do
  appendCode $ "  store " ++ (toIR v) ++ ", " ++ (toIR dst)

icmp : {t : IRType} -> String -> IRValue t -> IRValue t -> Codegen (IRValue I1)
icmp {t} cond a b = do
  compare <- assignSSA $ "icmp " ++ cond ++ " " ++ (show t) ++ " " ++ showWithoutType a ++ ", " ++ showWithoutType b
  pure $ SSA I1 compare

mkZext : {to : IRType} -> IRValue from -> Codegen (IRValue to)
mkZext {to} val = (SSA to) <$> assignSSA ("zext " ++ toIR val ++ " to " ++ show to)

mkSext : {to : IRType} -> IRValue from -> Codegen (IRValue to)
mkSext {to} val = (SSA to) <$> assignSSA ("sext " ++ toIR val ++ " to " ++ show to)

fptosi : {to : IRType} -> IRValue from -> Codegen (IRValue to)
fptosi {to} val = (SSA to) <$> assignSSA ("fptosi " ++ toIR val ++ " to " ++ show to)

phi : {t : IRType} -> List (IRValue t, IRLabel) -> Codegen (IRValue t)
phi xs = (SSA t) <$> assignSSA ("phi " ++ show t ++ " " ++ showSep ", " (map getEdge xs)) where
  getEdge : (IRValue t, IRLabel) -> String
  getEdge (val, lbl) = "[ " ++ showWithoutType val ++ ", " ++ showWithoutType lbl ++ " ]"

getElementPtr : {t : IRType} -> IRValue (Pointer t) -> IRValue ot -> Codegen (IRValue (Pointer t))
getElementPtr {t} ptr offset =
  SSA (Pointer t) <$> assignSSA ("getelementptr inbounds " ++ show t ++ ", " ++ toIR ptr ++ ", " ++ toIR offset)

bitcast : {from : IRType} -> {to : IRType} -> IRValue from -> Codegen (IRValue (Pointer to))
bitcast {from} {to} val = (SSA (Pointer to)) <$> assignSSA ("bitcast " ++ toIR val ++ " to " ++ show (Pointer to))

mkBinOp : {t : IRType} -> String -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkBinOp {t} s a b = do
  result <- assignSSA $ s ++ " " ++ (show t) ++ " " ++ showWithoutType a ++ ", " ++ showWithoutType b
  pure $ SSA t result

mkOr : {t : IRType} -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkOr = mkBinOp "or"

mkAnd : {t : IRType} -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkAnd = mkBinOp "and"

mkAdd : {t : IRType} -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkAdd = mkBinOp "add"

mkAddNoWrap : {t : IRType} -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkAddNoWrap = mkBinOp "add nuw nsw"

mkMul : {t : IRType} -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkMul = mkBinOp "mul"

mkSub : {t : IRType} -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkSub = mkBinOp "sub"

mkSDiv : {t : IRType} -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkSDiv = mkBinOp "sdiv"

mkSRem : {t : IRType} -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkSRem = mkBinOp "srem"

mkShiftL : {t : IRType} -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkShiftL = mkBinOp "shl"

unlikely : IRValue I1 -> Codegen (IRValue I1)
unlikely cond = (SSA I1) <$> assignSSA (" call ccc i1 @llvm.expect.i1(" ++ toIR cond ++ ", i1 0)")

likely : IRValue I1 -> Codegen (IRValue I1)
likely cond = (SSA I1) <$> assignSSA (" call ccc i1 @llvm.expect.i1(" ++ toIR cond ++ ", i1 1)")

branch : IRValue I1 -> (true : IRLabel) -> (false : IRLabel) -> Codegen ()
branch cond whenTrue whenFalse =
  appendCode $ "br " ++ toIR cond ++ ", " ++ toIR whenTrue ++ ", " ++ toIR whenFalse

jump : IRLabel -> Codegen ()
jump to =
  appendCode $ "br " ++ toIR to

mkMin : {t : IRType} -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkMin {t} a b = do
  aSmaller <- icmp "slt" a b
  (SSA t) <$> assignSSA ("select " ++ toIR aSmaller ++ ", " ++ toIR a ++ ", " ++ toIR b)

mkMax : {t : IRType} -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkMax {t} a b = do
  aLarger <- icmp "sgt" a b
  (SSA t) <$> assignSSA ("select " ++ toIR aLarger ++ ", " ++ toIR a ++ ", " ++ toIR b)

voidCall : String -> String -> Vect n String -> Codegen ()
voidCall cconv name args =
  appendCode $ "  call " ++ cconv ++ " void " ++ name ++ "(" ++ (showSep ", " (toList args)) ++ ")"

call : {t : IRType} -> String -> String -> Vect n String -> Codegen (IRValue t)
call {t} cconv name args = do
  SSA t <$> (assignSSA $ "  call " ++ cconv ++ " " ++ show t ++ " " ++ name ++ "(" ++ (showSep ", " (toList args)) ++ ")")

-- Call a "runtime-aware" foreign function, i.e. one, that can allocate memory
foreignCall : {t : IRType} -> String -> Vect n String -> Codegen (IRValue t)
foreignCall {t} name args = do
  SSA t <$> (assignSSA $ "  call ccc " ++ show t ++ " " ++ name ++ "(%RuntimePtr %BaseArg, " ++ (showSep ", " (toList args)) ++ ")")

getObjectSlot : IRValue IRObjPtr -> Int -> Codegen (IRValue I64)
getObjectSlot obj n = do
  i64ptr <- assignSSA $ "bitcast " ++ toIR obj ++ " to i64*"
  slotPtr <- assignSSA $ "getelementptr inbounds i64, i64* " ++ i64ptr ++ ", i64 " ++ show n
  load {t=I64} (SSA (Pointer I64) slotPtr)

getObjectSlotAddr : {t : IRType} -> IRValue IRObjPtr -> Int -> Codegen (IRValue (Pointer t))
getObjectSlotAddr obj n = do
  i64ptr <- assignSSA $ "bitcast " ++ toIR obj ++ " to i64*"
  slotPtr <- assignSSA $ "getelementptr inbounds i64, i64* " ++ i64ptr ++ ", i64 " ++ show n
  slotPtrT <- assignSSA $ "bitcast i64* " ++ slotPtr ++ " to " ++ show t ++ "*"
  pure (SSA (Pointer t) slotPtrT)

getObjectSlotT : {t : IRType} -> IRValue IRObjPtr -> Int -> Codegen (IRValue t)
getObjectSlotT obj n = do
  i64ptr <- assignSSA $ "bitcast " ++ toIR obj ++ " to i64*"
  slotPtr <- assignSSA $ "getelementptr inbounds i64, i64* " ++ i64ptr ++ ", i64 " ++ show n
  slotPtrT <- assignSSA $ "bitcast i64* " ++ slotPtr ++ " to " ++ show t ++ "*"
  load {t=t} (SSA (Pointer t) slotPtrT)

getObjectHeader : IRValue IRObjPtr -> Codegen (IRValue I64)
getObjectHeader o = getObjectSlotT o 0

putObjectSlotG : {t : IRType} -> IRValue IRObjPtr -> IRValue I64 -> IRValue t -> Codegen ()
putObjectSlotG {t} obj pos val = do
  i64ptr <- assignSSA $ "bitcast " ++ toIR obj ++ " to i64*"
  slotPtr <- assignSSA $ "getelementptr inbounds i64, i64* " ++ i64ptr ++ ", " ++ toIR pos
  slotPtrObj <- assignSSA $ "bitcast i64* " ++ slotPtr ++ " to " ++ show t ++ "*"
  appendCode $ "  store " ++ toIR val ++ ", " ++ show t ++ " * " ++ slotPtrObj

putObjectSlot : {t : IRType} -> IRValue IRObjPtr -> Integer -> IRValue t -> Codegen ()
putObjectSlot {t} obj n val = putObjectSlotG {t=t} obj (ConstI64 n) val

putObjectHeader : IRValue IRObjPtr -> IRValue I64 -> Codegen ()
putObjectHeader o h = putObjectSlotG o (ConstI64 0) h

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

dynamicAllocate : IRValue I64 -> Codegen (IRValue IRObjPtr)
dynamicAllocate payloadSize = do
  totalSize <- SSA I64 <$> assignSSA ("add nsw nuw " ++ (toIR payloadSize) ++ ", " ++ HEADER_SIZE)

  hp <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpVar"
  hpLim <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpLimVar"
  let base = "%RuntimePtr %BaseArg"

  allocated <- assignSSA $ "call fastcc %Return1 @rapid_allocate(" ++ showSep ", " [hp, base, hpLim] ++ ", "++(toIR totalSize)++") alwaysinline optsize nounwind"
  newHp <- assignSSA $ "extractvalue %Return1 " ++ allocated ++ ", 0"
  appendCode $ "store %RuntimePtr " ++ newHp ++ ", %RuntimePtr* %HpVar"
  newHpLim <- assignSSA $ "extractvalue %Return1 " ++ allocated ++ ", 1"
  appendCode $ "store %RuntimePtr " ++ newHpLim ++ ", %RuntimePtr* %HpLimVar"
  SSA IRObjPtr <$> assignSSA ("extractvalue %Return1 " ++ allocated ++ ", 2")

mkTrunc : {to : IRType} -> IRValue from -> Codegen (IRValue to)
mkTrunc {to} val = (SSA to) <$> assignSSA ("trunc " ++ toIR val ++ " to " ++ show to)

header : Int -> Integer
header i = (cast i) `prim__shl_Integer` 32

applyClosureHelperFunc : Codegen ()
applyClosureHelperFunc = do
  appendCode $ funcEntry

  let maxArgs = FAT_CLOSURE_LIMIT

  let closureObj = SSA IRObjPtr "%closureObjArg"
  let argValue = SSA IRObjPtr "%argumentObjArg"
  closureHeader <- getObjectHeader closureObj
  argCount <- assignSSA $ "and i64 65535, " ++ showWithoutType closureHeader
  missingArgCountShifted <- assignSSA $ "and i64 4294901760, " ++ showWithoutType closureHeader
  missingArgCount <- assignSSA $ "lshr i64 " ++ missingArgCountShifted ++ ", 16"
  isSaturated <- assignSSA $ "icmp eq i64 1, " ++ missingArgCount
  labelName <- mkVarName "closure_saturated"
  lblUnsaturated <- genLabel "closure_unsaturated"
  appendCode $ "br i1 " ++ isSaturated ++ ", label %" ++ labelName ++ "_yes, " ++ toIR lblUnsaturated
  appendCode $ labelName ++ "_yes:"
  funcPtr <- getObjectSlotT {t=FuncPtr} closureObj 1

  let hp = "%RuntimePtr %HpArg"
  let base = "%RuntimePtr %BaseArg"
  let hpLim = "%RuntimePtr %HpLimArg"

  lblApplyViaClosureEntry <- genLabel "apply_via_closure_entry"

  applyClosure <- mkVarName "apply_closure_"
  -- if the closure requires a total number of arguments <= FAT_CLOSURE_LIMIT
  -- (i.e. storedArgs <= (FAT_CLOSURE_LIMIT - 1)), it is invoked directly
  -- otherwise it is called via its "$$closureEntry" function
  appendCode $ "  switch i64 " ++ argCount ++ ", " ++ toIR lblApplyViaClosureEntry ++ " [\n  " ++
  (showSep "\n  " $ (flip map) (rangeFromTo 0 (maxArgs - 1)) (\i => "i64 " ++ show i ++ ", label %" ++ applyClosure ++ "_" ++ show i)) ++
  "]"

  for_ (rangeFromTo 0 maxArgs) (\i => do
    let labelName = applyClosure ++ "_" ++ show i
    appendCode $ labelName ++ ":"
    storedArgs <- for (rangeFromThenTo 0 1 (i-1)) (\i => do
                      argItem <- getObjectSlotT {t=IRObjPtr} closureObj (2+i)
                      pure $ (toIR argItem)
                      )
    let argList = [hp, base, hpLim] ++ storedArgs ++ [toIR argValue]
    func <- assignSSA $ "bitcast " ++ (toIR funcPtr) ++ " to %FuncPtrArgs" ++ show (i+1)
    callRes <- assignSSA $ "tail call fastcc %Return1 " ++ func ++ "(" ++ (showSep ", " argList) ++ ")"
    appendCode $ "ret %Return1 " ++ callRes
    )

  beginLabel lblUnsaturated

  appliedArgCount <- mkAddNoWrap (SSA I64 argCount) (ConstI64 1)
  newArgsSize <- mkMul appliedArgCount (ConstI64 8)
  -- add 8 bytes for entry func ptr
  newPayloadSize <- mkAddNoWrap newArgsSize (ConstI64 8)
  newClosureTotalSize <- mkAddNoWrap newPayloadSize (ConstI64 $ cast HEADER_SIZE)
  newClosure <- dynamicAllocate newPayloadSize

  let newHeader = ConstI64 $ header OBJECT_TYPE_ID_CLOSURE
  newMissingArgs <- mkSub (SSA I64 missingArgCount) (ConstI64 1)
  newMissingArgsShifted <- mkBinOp "shl" newMissingArgs (ConstI64 16)

  newHeader' <- mkOr newHeader newMissingArgsShifted
  newHeader'' <- mkOr newHeader' appliedArgCount

  -- FIXME: "to copy" size is probably 8 bytes too big
  appendCode $ "  call void @llvm.memcpy.p0i8.p0i8.i64(" ++ toIR newClosure ++ ", " ++ toIR closureObj ++ ", " ++ toIR newClosureTotalSize ++ ", i1 false)"
  putObjectHeader newClosure newHeader''

  newArgSlotNumber <- mkAddNoWrap appliedArgCount (ConstI64 1)
  putObjectSlotG newClosure newArgSlotNumber argValue

  store newClosure (reg2val RVal)

  appendCode $ funcReturn

  beginLabel lblApplyViaClosureEntry
  closureEntryPtr <- assignSSA $ "bitcast " ++ (toIR funcPtr) ++ " to %FuncPtrClosureEntry"
  let argList = [hp, base, hpLim, toIR closureObj, toIR argValue]
  callRes <- assignSSA $ "tail call fastcc %Return1 " ++ closureEntryPtr ++ "(" ++ (showSep ", " argList) ++ ")"
  appendCode $ "ret %Return1 " ++ callRes

  appendCode $ "call ccc void @idris_rts_crash(i64 13)"
  appendCode "unreachable"

cgMkInt : IRValue I64 -> Codegen (IRValue IRObjPtr)
cgMkInt val = do
  newObj <- dynamicAllocate (ConstI64 8)
  putObjectHeader newObj (ConstI64 $ header OBJECT_TYPE_ID_INT)
  putObjectSlotG newObj (ConstI64 1) val
  pure newObj

cgMkDouble : IRValue F64 -> Codegen (IRValue IRObjPtr)
cgMkDouble val = do
  newObj <- dynamicAllocate (ConstI64 8)
  putObjectHeader newObj (ConstI64 $ header OBJECT_TYPE_ID_DOUBLE)
  putObjectSlotG newObj (ConstI64 1) val
  pure newObj

getStringIR : String -> String
getStringIR s = concatMap okchar (unpack s)
  where
    okchar : Char -> String
    okchar c = if isAlphaNum c
                  then cast c
                  else "\\" ++ asHex2 (cast {to=Int} c)

data CompareOp = LT | LTE | EQ | GTE | GT

stringCompare : CompareOp -> Reg -> Reg -> Codegen (IRValue IRObjPtr)
stringCompare op r1 r2 = do
  o1 <- load (reg2val r1)
  o2 <- load (reg2val r2)
  h1 <- getObjectHeader o1
  h2 <- getObjectHeader o2
  l1 <- mkBinOp "and" (ConstI64 0xffffffff) h1
  l2 <- mkBinOp "and" (ConstI64 0xffffffff) h2

  minLength <- mkMin l1 l2

  lblSizeCompare <- genLabel "strcompare_size"
  lblCmpStart <- genLabel "strcompare_start"
  lblPrefixEq <- genLabel "strcompare_prefix_eq"
  lblPrefixNotEq <- genLabel "strcompare_prefix_neq"
  lblEnd <- genLabel "strcompare_end"

  jump lblSizeCompare
  beginLabel lblSizeCompare
  lengthsEqual <- icmp "eq" l1 l2

  let startCommand = case op of
                          EQ => (branch lengthsEqual lblCmpStart lblEnd)
                          _ => (branch (Const I1 1) lblCmpStart lblEnd)

  startCommand
  beginLabel lblCmpStart

  str1 <- getObjectSlotAddr {t=I8} o1 1
  str2 <- getObjectSlotAddr {t=I8} o2 1
  cmpResult32 <- call {t=I32} "fastcc" "@rapid.memcmp" [toIR str1, toIR str2, toIR minLength]
  cmpResult <- mkSext cmpResult32
  cmpResultIsEq <- icmp "eq" cmpResult (ConstI64 0)
  branch cmpResultIsEq lblPrefixEq lblPrefixNotEq

  beginLabel lblPrefixEq
  string1Shorter <- icmp "slt" l1 l2
  string1ShorterOrEqual <- icmp "sle" l1 l2
  string2Shorter <- icmp "slt" l2 l1
  string2ShorterOrEqual <- icmp "sle" l2 l1
  let result : IRValue I1
      result =  case op of
                     LT  => string1Shorter
                     LTE => string1ShorterOrEqual
                     EQ  => lengthsEqual
                     GTE => string2ShorterOrEqual
                     GT  => string2Shorter
  jump lblEnd

  beginLabel lblPrefixNotEq
  cmpResultIsLt <- icmp "slt" cmpResult (Const I64 0)
  cmpResultIsGt <- icmp "sgt" cmpResult (Const I64 0)
  let result2 =  case op of
                      LT  => cmpResultIsLt
                      LTE => cmpResultIsLt
                      EQ  => Const I1 0
                      GT  => cmpResultIsGt
                      GTE => cmpResultIsGt
  jump lblEnd
  beginLabel lblEnd

  finalResult <- phi [(result, lblPrefixEq), (result2, lblPrefixNotEq), (Const I1 0, lblSizeCompare)]
  cgMkInt !(mkZext finalResult)

mkSubstring : IRValue IRObjPtr -> IRValue I64 -> IRValue I64 -> Codegen (IRValue IRObjPtr)
mkSubstring strObj startIndexRaw length = do
  -- FIXME: this assumes ASCII
  hdr <- getObjectHeader strObj
  strLen <- mkBinOp "and" (ConstI64 0xffffffff) hdr

  startIndex <- mkMax startIndexRaw (Const I64 0)

  maxResultLength <- mkSub strLen startIndex
  resultLengthRaw <- mkMin maxResultLength length
  resultLength <- mkMax (Const I64 0) resultLengthRaw

  newStr <- dynamicAllocate resultLength
  newHeader <- mkBinOp "or" resultLength (ConstI64 $ header OBJECT_TYPE_ID_STR)
  putObjectHeader newStr newHeader
  newStrPayload <- getObjectSlotAddr {t=I8} newStr 1

  strPayloadStart <- getObjectSlotAddr {t=I8} strObj 1
  strCopyRangeStart <- getElementPtr strPayloadStart startIndex

  voidCall "ccc" "@llvm.memcpy.p0i8.p0i8.i64" [toIR newStrPayload, toIR strCopyRangeStart, toIR resultLength, toIR (Const I1 0)]
  pure newStr

mkStr : Int -> String -> Codegen (IRValue IRObjPtr)
mkStr i s = do
  let len = cast {to=Integer} $ length s
  cn <- addConstant i $ "private unnamed_addr constant [" ++ show len ++ " x i8] c\"" ++ (getStringIR s) ++ "\""
  cn <- assignSSA $ "bitcast [" ++ show len ++ " x i8]* "++cn++" to i8*"
  let newHeader = ConstI64 $ (header OBJECT_TYPE_ID_STR) + len
  newObj <- dynamicAllocate (ConstI64 len)
  putObjectHeader newObj newHeader
  strPayload <- getObjectSlotAddr {t=I8} newObj 1
  appendCode $ "  call void @llvm.memcpy.p0i8.p0i8.i32(" ++ toIR strPayload ++ ", i8* "++cn++", i32 " ++show len ++", i1 false)"
  pure newObj

export
enumerate : List a -> List (Int, a)
enumerate l = enumerate' 0 l where
  enumerate' : Int -> List a -> List (Int, a)
  enumerate' _ [] = []
  enumerate' i (x::xs) = (i, x)::(enumerate' (i+1) xs)

mkCon : Int -> List Reg -> Codegen (IRValue IRObjPtr)
mkCon tag args = do
  newObj <- dynamicAllocate (ConstI64 $ cast (8 * (length args)))
  -- TODO: add object type to header for GC
  putObjectHeader newObj (ConstI64 $ cast tag)
  let enumArgs = enumerate args
  for enumArgs (\x => let (i, arg) = x in do
                            arg <- load (reg2val arg)
                            putObjectSlotG newObj (ConstI64 $ cast (1+i)) arg
                          )
  pure newObj

unboxInt : IRValue (Pointer IRObjPtr) -> Codegen (IRValue I64)
unboxInt src = getObjectSlotT {t=I64} !(load src) 1

unboxInt' : IRValue IRObjPtr -> Codegen (IRValue I64)
unboxInt' src = getObjectSlotT {t=I64} src 1

unboxFloat64 : IRValue (Pointer IRObjPtr) -> Codegen (IRValue F64)
unboxFloat64 src = getObjectSlotT {t=F64} !(load src) 1

unboxFloat64' : IRValue IRObjPtr -> Codegen (IRValue F64)
unboxFloat64' src = getObjectSlotT {t=F64} src 1

total
showConstant : Constant -> String
showConstant (I i) = "(I " ++ show i ++ ")"
showConstant (BI i) = "(BI " ++ show i ++ ")"
showConstant (Str i) = "(Str " ++ show i ++ ")"
showConstant (Ch i) = "(Ch " ++ show i ++ ")"
showConstant (Db i) = "(Db " ++ show i ++ ")"
showConstant other = "(CONST " ++ show other ++ ")"

makeConstCaseLabel : String -> (Constant, a) -> String
makeConstCaseLabel caseId (I i,_) = "i64 " ++ show i ++ ", label %" ++ caseId ++ "_is_" ++ show i
-- FIXME: how to handle this with GMP Integers?
makeConstCaseLabel caseId (BI i,_) = "i64 " ++ show i ++ ", label %" ++ caseId ++ "_is_" ++ show i
makeConstCaseLabel caseId (Ch c,_) = "i32 " ++ show i ++ ", label %" ++ caseId ++ "_is_" ++ show i where i:Int; i = (cast {to=Int} c)
makeConstCaseLabel caseId (c,_) = "const case error: " ++ (showConstant c)

makeNameId : Int -> Int
makeNameId nid = 0x80000000 + nid

makeCaseLabel : {auto conNames : SortedMap Name Int} -> String -> (Either Int Name, a) -> String
makeCaseLabel caseId (Left i,_) = "i64 " ++ show i ++ ", label %" ++ caseId ++ "_tag_is_" ++ show i
makeCaseLabel {conNames} caseId (Right n,_) =
  case lookup n conNames of
       Just nameId => "i64 " ++ show (makeNameId nameId) ++ ", label %" ++ caseId ++ "_name_is_" ++ show (makeNameId nameId)
       Nothing => idris_crash $ "name not found: " ++ show n

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

findConstCaseType : List (Constant, List VMInst) -> Constant
findConstCaseType [] = idris_crash "empty const case"
findConstCaseType ((I _,_)::_) = IntType
findConstCaseType ((BI _,_)::_) = IntegerType
findConstCaseType ((Str _,_)::_) = StringType
findConstCaseType ((Ch _,_)::_) = CharType
findConstCaseType t = idris_crash $ "unknwon const case type" ++ show t

compareStr : IRValue IRObjPtr -> IRValue IRObjPtr -> Codegen (IRValue I1)
compareStr obj1 obj2 = do
  lblStart <- genLabel "strcompare_hdr"
  lblEnd <- genLabel "strcompare_end"
  lblCompareContents <- genLabel "strcompare_content"
  appendCode $ "br " ++ toIR lblStart
  beginLabel lblStart
  h1 <- getObjectHeader obj1
  h2 <- getObjectHeader obj2
  headersEqual <- icmp "eq" h1 h2
  appendCode $ "br " ++ toIR headersEqual ++ ", " ++ toIR lblCompareContents ++ ", " ++ toIR lblEnd
  beginLabel lblCompareContents
  str1 <- getObjectSlotAddr {t=I8} obj1 1
  str2 <- getObjectSlotAddr {t=I8} obj2 1
  length <- mkAnd h1 (ConstI64 0xffffffff)
  contentsEqual <- (SSA I1) <$> assignSSA ("call fastcc i1 @mem_eq(" ++ (showSep ", " ([toIR str1, toIR str2, toIR length])) ++ ")")
  appendCode $ "br " ++ toIR lblEnd
  beginLabel lblEnd
  phi [(headersEqual, lblStart), (contentsEqual, lblCompareContents)]
  --(SSA I1) <$> assignSSA ("phi i1 [ " ++ showWithoutType headersEqual ++ ", " ++ showWithoutType lblStart ++ " ], [ " ++ showWithoutType contentsEqual ++ ", " ++ showWithoutType lblCompareContents ++ " ]")

unboxChar : IRValue (Pointer IRObjPtr) -> Codegen (IRValue I32)
unboxChar objPtr = do
  hdr <- getObjectHeader !(load objPtr)
  chVal64 <- mkAnd (ConstI64 0xffffffff) hdr
  chVal32 <- mkTrunc {to=I32} chVal64
  pure chVal32

mutual
getInstForConstCaseChar : {auto conNames : SortedMap Name Int} -> Int -> Reg -> List (Constant, List VMInst) -> Maybe (List VMInst) -> Codegen ()
getInstForConstCaseChar i r alts def =
  do let def' = fromMaybe [] def
     caseId <- mkVarName "case_"
     let labelEnd = caseId ++ "_end"
     scrutinee <- unboxChar (reg2val r)
     appendCode $ "  switch " ++ toIR scrutinee ++ ", label %" ++ caseId ++ "_default [ " ++ (showSep "\n      " (map (makeConstCaseLabel caseId) alts)) ++ " ]"
     appendCode $ caseId ++ "_default:"
     traverse (getInstIRWithComment i) def'
     appendCode $ "br label %" ++ labelEnd
     traverse (makeCaseAlt caseId) alts
     appendCode $ labelEnd ++ ":"
     pure ()
  where
    makeCaseAlt : String -> (Constant, List VMInst) -> Codegen ()
    makeCaseAlt caseId (Ch ch, is) = do
      let c = cast {to=Int} ch
      appendCode $ caseId ++ "_is_" ++ (show c) ++ ":"
      traverse_ (getInstIRWithComment i) is
      appendCode $ "br label %" ++ caseId ++ "_end"
    makeCaseAlt _ (c, _) = appendCode $ "ERROR: constcase must be Char, got: " ++ show c

getInstForConstCaseInt : {auto conNames : SortedMap Name Int} -> Int -> Reg -> List (Constant, List VMInst) -> Maybe (List VMInst) -> Codegen ()
getInstForConstCaseInt i r alts def =
  do let def' = fromMaybe [] def
     caseId <- mkVarName "case_"
     let labelEnd = caseId ++ "_end"
     scrutinee <- unboxInt (reg2val r)
     appendCode $ "  switch " ++ toIR scrutinee ++ ", label %" ++ caseId ++ "_default [ " ++ (showSep "\n      " (map (makeConstCaseLabel caseId) alts)) ++ " ]"
     appendCode $ caseId ++ "_default:"
     traverse (getInstIRWithComment i) def'
     appendCode $ "br label %" ++ labelEnd
     traverse (makeCaseAlt caseId) alts
     appendCode $ labelEnd ++ ":"
     pure ()
  where
    makeCaseAlt : String -> (Constant, List VMInst) -> Codegen ()
    makeCaseAlt caseId (I c, is) = do
      appendCode $ caseId ++ "_is_" ++ (show c) ++ ":"
      traverse_ (getInstIRWithComment i) is
      appendCode $ "br label %" ++ caseId ++ "_end"
    makeCaseAlt caseId (BI c, is) = makeCaseAlt caseId (I $ cast c, is)
    makeCaseAlt _ (c, _) = appendCode $ "ERROR: constcase must be Int, got: " ++ show c

getInstForConstCaseString : {auto conNames : SortedMap Name Int} -> Int -> Reg -> List (Constant, List VMInst) -> Maybe (List VMInst) -> Codegen ()
getInstForConstCaseString i r alts def =
  do let def' = fromMaybe [] def
     scrutinee <- load (reg2val r)
     let numAlts = enumerate alts
     caseId <- mkVarName "case_"
     labelEnd <- genLabel $ caseId ++ "_end"

     traverse (makeCaseAlt caseId labelEnd scrutinee) numAlts

     labelDefault <- genLabel $ caseId ++ "_default"
     appendCode $ "br " ++ toIR labelDefault
     beginLabel labelDefault

     traverse (getInstIRWithComment i) def'
     appendCode $ "br " ++ toIR labelEnd

     beginLabel labelEnd
  where
    makeCaseAlt : String -> IRLabel -> IRValue IRObjPtr -> (Int, Constant, List VMInst) -> Codegen ()
    makeCaseAlt caseId labelEnd scrutinee (idx, Str s, is) = do
      let labelAltStart = MkLabel (caseId ++ "_alt_" ++ show idx)
      let labelAltNext = MkLabel (caseId ++ "_next" ++ show idx)
      compStr <- mkStr i s
      match <- compareStr compStr scrutinee
      appendCode $ "br " ++ toIR match ++ ", " ++ toIR labelAltStart ++ ", " ++ toIR labelAltNext
      -- compare s == scrut
      beginLabel labelAltStart
      traverse_ (getInstIRWithComment i) is
      appendCode $ "br " ++ toIR labelEnd
      beginLabel labelAltNext
    makeCaseAlt _ _ _ (_, c, _) = appendCode $ "ERROR: constcase must be Str, got: " ++ show c

getInstIR : {auto conNames : SortedMap Name Int} -> Int -> VMInst -> Codegen ()
getInstIR i (DECLARE (Loc r)) = appendCode $ "  %v" ++ show r ++ "Var = alloca %ObjPtr"
getInstIR i (ASSIGN r src) = do
  value <- assignSSA $ "load %ObjPtr, %ObjPtr* " ++ toIR src ++ "Var"
  appendCode $ "  store %ObjPtr " ++ value ++ ", %ObjPtr* " ++ toIR r ++ "Var"

getInstIR i (OP r Crash [r1, r2]) = do
  msg <- load (reg2val r2)
  appendCode $ "  call ccc void @idris_rts_crash_msg(" ++ toIR msg ++ ") noreturn"
  appendCode $ "unreachable"
getInstIR i (ERROR s) = do
  msg <- mkStr i s
  appendCode $ "  call ccc void @idris_rts_crash_msg(" ++ toIR msg ++ ") noreturn"
  appendCode $ "unreachable"
getInstIR i (OP r BelieveMe [_, _, v]) = do
  store !(load (reg2val v)) (reg2val r)

getInstIR i (OP r StrHead [r1]) = do
  o1 <- load (reg2val r1)
  objHeader <- getObjectHeader o1
  let zeroStrHeader = (ConstI64 $ header OBJECT_TYPE_ID_STR)
  strIsZero <- unlikely !(icmp "eq" zeroStrHeader objHeader)
  strHeadOk <- genLabel "strhead_ok"
  strHeadError <- genLabel "strhead_err"
  strHeadFinished <- genLabel "strhead_finished"

  branch strIsZero strHeadError strHeadOk
  beginLabel strHeadOk
  payload <- getObjectSlotAddr {t=I8} o1 1

  -- FIXME: this assumes ASCII
  firstChar <- mkZext {to=I64} !(load payload)

  newCharObj <- dynamicAllocate (ConstI64 0)
  newHeader <- mkOr firstChar (ConstI64 $ header OBJECT_TYPE_ID_CHAR)
  putObjectHeader newCharObj newHeader

  store newCharObj (reg2val r)
  jump strHeadFinished

  beginLabel strHeadError
  appendCode $ "call ccc void @idris_rts_crash(i64 1) noreturn"
  appendCode $ "unreachable"

  beginLabel strHeadFinished

getInstIR i (OP r StrTail [r1]) = do
  o1 <- load (reg2val r1)
  objHeader <- getObjectHeader o1
  strLength <- mkAnd objHeader (Const I64 0xffffffff)
  strIsZero <- unlikely !(icmp "eq" strLength (Const I64 0))
  strTailOk <- genLabel "strtail_ok"
  strTailError <- genLabel "strtail_err"
  strTailFinished <- genLabel "strtail_finished"

  branch strIsZero strTailError strTailOk
  beginLabel strTailOk

  subStr <- mkSubstring o1 (Const I64 1) !(mkSub strLength (Const I64 1))

  store subStr (reg2val r)
  jump strTailFinished

  beginLabel strTailError
  appendCode $ "call ccc void @idris_rts_crash(i64 17) noreturn"
  appendCode $ "unreachable"

  beginLabel strTailFinished

getInstIR i (OP r StrSubstr [r1, r2, r3]) = do
  o1 <- load (reg2val r3)
  offset <- unboxInt (reg2val r1)
  length <- unboxInt (reg2val r2)
  subStr <- mkSubstring o1 offset length
  store subStr (reg2val r)

getInstIR i (OP r StrAppend [r1, r2]) = do
  o1 <- load (reg2val r1)
  o2 <- load (reg2val r2)
  h1 <- getObjectHeader o1
  h2 <- getObjectHeader o2
  l1 <- mkBinOp "and" (ConstI64 0xffffffff) h1
  l2 <- mkBinOp "and" (ConstI64 0xffffffff) h2
  newLength <- mkAddNoWrap l1 l2
  newStr <- dynamicAllocate newLength
  newHeader <- mkBinOp "or" newLength (ConstI64 $ header OBJECT_TYPE_ID_STR)

  str1 <- getObjectSlotAddr {t=I8} o1 1
  str2 <- getObjectSlotAddr {t=I8} o2 1

  newStrPayload1 <- getObjectSlotAddr {t=I8} newStr 1
  newStrPayload2 <- getElementPtr newStrPayload1 l1

  appendCode $ "  call void @llvm.memcpy.p0i8.p0i8.i64(" ++ toIR newStrPayload1 ++ ", " ++ toIR str1 ++ ", " ++ toIR l1 ++ ", i1 false)"
  appendCode $ "  call void @llvm.memcpy.p0i8.p0i8.i64(" ++ toIR newStrPayload2 ++ ", " ++ toIR str2 ++ ", " ++ toIR l2 ++ ", i1 false)"

  putObjectHeader newStr newHeader

  store newStr (reg2val r)

getInstIR i (OP r StrReverse [r1]) = do
  strObj <- load (reg2val r1)
  hdr <- getObjectHeader strObj
  length <- mkBinOp "and" (ConstI64 0xffffffff) hdr
  newStr <- dynamicAllocate length
  newHeader <- mkBinOp "or" length (ConstI64 $ header OBJECT_TYPE_ID_STR)

  origPayload <- getObjectSlotAddr {t=I8} strObj 1
  newStrPayload <- getObjectSlotAddr {t=I8} newStr 1

  appendCode $ "  call ccc void @rapid_strreverse(" ++ toIR newStrPayload ++ ", " ++ toIR origPayload ++ ", " ++ toIR length ++ ")"

  putObjectHeader newStr newHeader

  store newStr (reg2val r)

getInstIR i (OP r StrCons [r1, r2]) = do
  o1 <- load (reg2val r1)
  o2 <- load (reg2val r2)
  h1 <- getObjectHeader o1
  h2 <- getObjectHeader o2
  -- FIXME: this assumes ASCII
  charVal64 <- mkBinOp "and" (ConstI64 0xff) h1
  charVal <- mkTrunc {to=I8} charVal64
  l2 <- mkBinOp "and" (ConstI64 0xffffffff) h2
  newLength <- mkAddNoWrap (ConstI64 1) l2
  newStr <- dynamicAllocate newLength
  newHeader <- mkBinOp "or" newLength (ConstI64 $ header OBJECT_TYPE_ID_STR)

  str2 <- getObjectSlotAddr {t=I8} o2 1

  newStrPayload1 <- getObjectSlotAddr {t=I8} newStr 1
  newStrPayload2 <- getElementPtr newStrPayload1 (ConstI64 1)

  store charVal newStrPayload1
  appendCode $ "  call void @llvm.memcpy.p0i8.p0i8.i64(" ++ toIR newStrPayload2 ++ ", " ++ toIR str2 ++ ", " ++ toIR l2 ++ ", i1 false)"

  putObjectHeader newStr newHeader

  store newStr (reg2val r)

getInstIR i (OP r StrLength [r1]) = do
  h1 <- getObjectHeader !(load (reg2val r1))
  l1 <- mkBinOp "and" (ConstI64 0xffffffff) h1
  sizeIntObj <- cgMkInt l1
  store sizeIntObj (reg2val r)
getInstIR i (OP r StrIndex [r1, r2]) = do
  o1 <- load (reg2val r1)
  objHeader <- getObjectHeader o1
  payload0 <- getObjectSlotAddr {t=I8} o1 1

  index <- unboxInt (reg2val r2)
  payload <- getElementPtr payload0 index

  -- FIXME: this assumes ASCII
  charVal <- mkZext {to=I64} !(load payload)

  newCharObj <- dynamicAllocate (ConstI64 0)
  newHeader <- mkOr charVal (ConstI64 $ header OBJECT_TYPE_ID_CHAR)
  putObjectHeader newCharObj newHeader

  store newCharObj (reg2val r)

getInstIR i (OP r (LT  StringType) [r1, r2]) = store !(stringCompare LT  r1 r2) (reg2val r)
getInstIR i (OP r (LTE StringType) [r1, r2]) = store !(stringCompare LTE r1 r2) (reg2val r)
getInstIR i (OP r (EQ  StringType) [r1, r2]) = store !(stringCompare EQ  r1 r2) (reg2val r)
getInstIR i (OP r (GTE StringType) [r1, r2]) = store !(stringCompare GTE r1 r2) (reg2val r)
getInstIR i (OP r (GT  StringType) [r1, r2]) = store !(stringCompare GT  r1 r2) (reg2val r)

getInstIR i (OP r (Cast IntegerType StringType) [r1]) = do
  theIntObj <- load (reg2val r1)
  theInt <- getObjectSlotT {t=I64} theIntObj 1

  -- max size of 2^64 = 20 + (optional "-" prefix) + NUL byte (from snprintf)
  newStr <- dynamicAllocate (ConstI64 24)
  strPayload <- getObjectSlotAddr {t=I8} newStr 1
  length <- (SSA I64) <$> assignSSA ("call ccc i64 @idris_rts_int_to_str(" ++ toIR strPayload ++ ", " ++ toIR theInt ++ ")")
  newHeader <- mkOr (ConstI64 $ header OBJECT_TYPE_ID_STR) length
  putObjectHeader newStr newHeader
  store newStr (reg2val r)

getInstIR i (OP r (Cast IntType StringType) [r1]) = do
  theIntObj <- load (reg2val r1)
  theInt <- getObjectSlotT {t=I64} theIntObj 1

  -- max size of 2^64 = 20 + (optional "-" prefix) + NUL byte (from snprintf)
  newStr <- dynamicAllocate (ConstI64 24)
  strPayload <- getObjectSlotAddr {t=I8} newStr 1
  length <- (SSA I64) <$> assignSSA ("call ccc i64 @idris_rts_int_to_str(" ++ toIR strPayload ++ ", " ++ toIR theInt ++ ")")
  newHeader <- mkOr (ConstI64 $ header OBJECT_TYPE_ID_STR) length
  putObjectHeader newStr newHeader
  store newStr (reg2val r)
getInstIR i (OP r (Cast DoubleType StringType) [r1]) = do
  obj <- load (reg2val r1)
  theDouble <- getObjectSlotT {t=F64} obj 1

  -- call once with nullptr as dest, to get required length
  length <- (SSA I64) <$> assignSSA ("call ccc i64 @idris_rts_double_to_str(i8* null, i64 0, " ++ toIR theDouble ++ ")")
  -- snprintf writes an additional NUL byte to terminate the cstr
  lengthPlus1 <- mkAddNoWrap length (ConstI64 1)

  newStr <- dynamicAllocate lengthPlus1
  strPayload <- getObjectSlotAddr {t=I8} newStr 1
  length <- (SSA I64) <$> assignSSA ("call ccc i64 @idris_rts_double_to_str(" ++ toIR strPayload ++ ", " ++ toIR lengthPlus1 ++ ", " ++ toIR theDouble ++ ")")
  newHeader <- mkOr (ConstI64 $ header OBJECT_TYPE_ID_STR) length
  putObjectHeader newStr newHeader
  store newStr (reg2val r)
getInstIR i (OP r (Cast DoubleType IntType) [r1]) = do
  fval <- unboxFloat64 (reg2val r1)
  intval <- fptosi fval
  newInt <- cgMkInt intval
  store newInt (reg2val r)
getInstIR i (OP r (Cast DoubleType IntegerType) [r1]) = do
  fval <- unboxFloat64 (reg2val r1)
  intval <- fptosi fval
  newInt <- cgMkInt intval
  store newInt (reg2val r)
getInstIR i (OP r (Cast StringType IntegerType) [r1]) = do
  strObj <- load (reg2val r1)
  parsedVal <- SSA I64 <$> assignSSA ("  call ccc i64 @idris_rts_str_to_int(" ++ toIR strObj ++ ")")
  newInt <- cgMkInt parsedVal
  store newInt (reg2val r)
getInstIR i (OP r (Cast StringType IntType) [r1]) = do
  strObj <- load (reg2val r1)
  parsedVal <- SSA I64 <$> assignSSA ("  call ccc i64 @idris_rts_str_to_int(" ++ toIR strObj ++ ")")
  newInt <- cgMkInt parsedVal
  store newInt (reg2val r)
getInstIR i (OP r (Cast StringType DoubleType) [r1]) = do
  strObj <- load (reg2val r1)
  parsedVal <- SSA F64 <$> assignSSA ("  call ccc double @idris_rts_str_to_double(" ++ toIR strObj ++ ")")
  newDouble <- cgMkDouble parsedVal
  store newDouble (reg2val r)

getInstIR i (OP r (Cast CharType IntegerType) [r1]) = do
  charHdr <- getObjectHeader !(load (reg2val r1))
  charVal <- mkAnd charHdr (ConstI64 0xffffffff)
  newInt <- cgMkInt charVal
  store newInt (reg2val r)

getInstIR i (OP r (Cast CharType IntType) [r1]) = do
  charHdr <- getObjectHeader !(load (reg2val r1))
  charVal <- mkAnd charHdr (ConstI64 0xffffffff)
  newInt <- cgMkInt charVal
  store newInt (reg2val r)

getInstIR i (OP r (Cast IntType CharType) [r1]) = do
  ival <- unboxInt (reg2val r1)
  truncated <- mkAnd (Const I64 0xffffffff) ival
  newCharObj <- dynamicAllocate (Const I64 0)
  hdr <- mkOr (truncated) (Const I64 $ header OBJECT_TYPE_ID_CHAR)
  putObjectHeader newCharObj hdr
  store newCharObj (reg2val r)
getInstIR i (OP r (Cast IntegerType CharType) [r1]) = do
  ival <- unboxInt (reg2val r1)
  truncated <- mkAnd (Const I64 0xffffffff) ival
  newCharObj <- dynamicAllocate (Const I64 0)
  hdr <- mkOr (truncated) (Const I64 $ header OBJECT_TYPE_ID_CHAR)
  putObjectHeader newCharObj hdr
  store newCharObj (reg2val r)

getInstIR i (OP r (Cast CharType StringType) [r1]) = do
  o1 <- load (reg2val r1)
  h1 <- getObjectHeader o1
  -- FIXME: this assumes ASCII
  -- FIXME: handle characters that need to be escaped
  charVal64 <- mkBinOp "and" (ConstI64 0xff) h1
  charVal <- mkTrunc {to=I8} charVal64
  let newLength = (ConstI64 1)
  newStr <- dynamicAllocate newLength
  newHeader <- mkBinOp "or" newLength (ConstI64 $ header OBJECT_TYPE_ID_STR)
  newStrPayload1 <- getObjectSlotAddr {t=I8} newStr 1
  store charVal newStrPayload1
  putObjectHeader newStr newHeader
  store newStr (reg2val r)

getInstIR i (OP r (Cast IntegerType IntType) [r1]) = do
  store !(load (reg2val r1)) (reg2val r)
getInstIR i (OP r (Cast IntType IntegerType) [r1]) = do
  store !(load (reg2val r1)) (reg2val r)

getInstIR i (OP r (Add IntType) [r1, r2]) = do
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  obj <- cgMkInt !(mkAdd i1 i2)
  store obj (reg2val r)
getInstIR i (OP r (Sub IntType) [r1, r2]) = do
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  obj <- cgMkInt !(mkSub i1 i2)
  store obj (reg2val r)
getInstIR i (OP r (Mul IntType) [r1, r2]) = do
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  obj <- cgMkInt !(mkMul i1 i2)
  store obj (reg2val r)
getInstIR i (OP r (Div IntType) [r1, r2]) = do
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  obj <- cgMkInt !(mkSDiv i1 i2)
  store obj (reg2val r)
getInstIR i (OP r (Mod IntType) [r1, r2]) = do
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  obj <- cgMkInt !(mkSRem i1 i2)
  store obj (reg2val r)

getInstIR i (OP r (Add IntegerType) [r1, r2]) = do
  -- FIXME: we treat Integers as bounded Ints -> should use GMP
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  obj <- cgMkInt !(mkAdd i1 i2)
  store obj (reg2val r)
getInstIR i (OP r (Sub IntegerType) [r1, r2]) = do
  -- FIXME: we treat Integers as bounded Ints -> should use GMP
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  obj <- cgMkInt !(mkSub i1 i2)
  store obj (reg2val r)
getInstIR i (OP r (Mul IntegerType) [r1, r2]) = do
  -- FIXME: we treat Integers as bounded Ints -> should use GMP
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  obj <- cgMkInt !(mkMul i1 i2)
  store obj (reg2val r)
getInstIR i (OP r (Div IntegerType) [r1, r2]) = do
  -- FIXME: we treat Integers as bounded Ints -> should use GMP
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  obj <- cgMkInt !(mkSDiv i1 i2)
  store obj (reg2val r)
getInstIR i (OP r (Mod IntegerType) [r1, r2]) = do
  -- FIXME: we treat Integers as bounded Ints -> should use GMP
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  obj <- cgMkInt !(mkSRem i1 i2)
  store obj (reg2val r)
getInstIR i (OP r (ShiftL IntegerType) [r1, r2]) = do
  -- FIXME: we treat Integers as bounded Ints -> should use GMP
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  obj <- cgMkInt !(mkShiftL i1 i2)
  store obj (reg2val r)

getInstIR i (OP r (LT CharType) [r1, r2]) = do
  -- compare Chars by comparing their headers
  o1 <- load (reg2val r1)
  o2 <- load (reg2val r2)
  i1 <- getObjectHeader o1
  i2 <- getObjectHeader o2
  cmp_i1 <- icmp "ult" i1 i2
  cmp_i64 <- assignSSA $ "zext " ++ toIR cmp_i1 ++ " to i64"
  obj <- cgMkInt (SSA I64 cmp_i64)
  store obj (reg2val r)
getInstIR i (OP r (LTE CharType) [r1, r2]) = do
  -- compare Chars by comparing their headers
  o1 <- load (reg2val r1)
  o2 <- load (reg2val r2)
  i1 <- getObjectHeader o1
  i2 <- getObjectHeader o2
  cmp_i1 <- icmp "ule" i1 i2
  cmp_i64 <- assignSSA $ "zext " ++ toIR cmp_i1 ++ " to i64"
  obj <- cgMkInt (SSA I64 cmp_i64)
  store obj (reg2val r)
getInstIR i (OP r (GTE CharType) [r1, r2]) = do
  -- compare Chars by comparing their headers
  o1 <- load (reg2val r1)
  o2 <- load (reg2val r2)
  i1 <- getObjectHeader o1
  i2 <- getObjectHeader o2
  cmp_i1 <- icmp "uge" i1 i2
  cmp_i64 <- assignSSA $ "zext " ++ toIR cmp_i1 ++ " to i64"
  obj <- cgMkInt (SSA I64 cmp_i64)
  store obj (reg2val r)
getInstIR i (OP r (GT CharType) [r1, r2]) = do
  -- compare Chars by comparing their headers
  o1 <- load (reg2val r1)
  o2 <- load (reg2val r2)
  i1 <- getObjectHeader o1
  i2 <- getObjectHeader o2
  cmp_i1 <- icmp "ugt" i1 i2
  cmp_i64 <- assignSSA $ "zext " ++ toIR cmp_i1 ++ " to i64"
  obj <- cgMkInt (SSA I64 cmp_i64)
  store obj (reg2val r)
getInstIR i (OP r (EQ CharType) [r1, r2]) = do
  -- Two Chars are equal, iff their headers are equal
  o1 <- load (reg2val r1)
  o2 <- load (reg2val r2)
  i1 <- getObjectHeader o1
  i2 <- getObjectHeader o2
  cmp_i1 <- icmp "eq" i1 i2
  cmp_i64 <- assignSSA $ "zext " ++ toIR cmp_i1 ++ " to i64"
  obj <- cgMkInt (SSA I64 cmp_i64)
  store obj (reg2val r)

getInstIR i (OP r (LT IntType) [r1, r2]) = do
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  vsum_i1 <- icmp "slt" i1 i2
  vsum_i64 <- mkZext {to=I64} vsum_i1
  obj <- cgMkInt vsum_i64
  store obj (reg2val r)
getInstIR i (OP r (LTE IntType) [r1, r2]) = do
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  vsum_i1 <- icmp "sle" i1 i2
  vsum_i64 <- mkZext {to=I64} vsum_i1
  obj <- cgMkInt vsum_i64
  store obj (reg2val r)
getInstIR i (OP r (EQ IntType) [r1, r2]) = do
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  vsum_i1 <- icmp "eq" i1 i2
  vsum_i64 <- mkZext {to=I64} vsum_i1
  obj <- cgMkInt vsum_i64
  store obj (reg2val r)
getInstIR i (OP r (GTE IntType) [r1, r2]) = do
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  vsum_i1 <- icmp "sge" i1 i2
  vsum_i64 <- mkZext {to=I64} vsum_i1
  obj <- cgMkInt vsum_i64
  store obj (reg2val r)
getInstIR i (OP r (GT IntType) [r1, r2]) = do
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  vsum_i1 <- icmp "sgt" i1 i2
  vsum_i64 <- mkZext {to=I64} vsum_i1
  obj <- cgMkInt vsum_i64
  store obj (reg2val r)

getInstIR i (OP r (EQ IntegerType) [r1, r2]) = do
  -- FIXME: we treat Integers as bounded Ints -> should use GMP
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  vsum_i1 <- icmp "eq" i1 i2
  vsum_i64 <- mkZext {to=I64} vsum_i1
  obj <- cgMkInt vsum_i64
  store obj (reg2val r)
getInstIR i (OP r (GT IntegerType) [r1, r2]) = do
  -- FIXME: we treat Integers as bounded Ints -> should use GMP
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  vsum_i1 <- icmp "sgt" i1 i2
  vsum_i64 <- mkZext {to=I64} vsum_i1
  obj <- cgMkInt vsum_i64
  store obj (reg2val r)
getInstIR i (OP r (GTE IntegerType) [r1, r2]) = do
  -- FIXME: we treat Integers as bounded Ints -> should use GMP
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  vsum_i1 <- icmp "sge" i1 i2
  vsum_i64 <- mkZext {to=I64} vsum_i1
  obj <- cgMkInt vsum_i64
  store obj (reg2val r)
getInstIR i (OP r (LT IntegerType) [r1, r2]) = do
  -- FIXME: we treat Integers as bounded Ints -> should use GMP
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  vsum_i1 <- icmp "slt" i1 i2
  vsum_i64 <- mkZext {to=I64} vsum_i1
  obj <- cgMkInt vsum_i64
  store obj (reg2val r)
getInstIR i (OP r (LTE IntegerType) [r1, r2]) = do
  -- FIXME: we treat Integers as bounded Ints -> should use GMP
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  vsum_i1 <- icmp "sle" i1 i2
  vsum_i64 <- mkZext {to=I64} vsum_i1
  obj <- cgMkInt vsum_i64
  store obj (reg2val r)

getInstIR i (MKCON r (Left tag) args) = do
  obj <- mkCon tag args
  store obj (reg2val r)
getInstIR {conNames} i (MKCON r (Right n) args) = do
  case lookup n conNames of
       Just nameId => do obj <- mkCon (makeNameId nameId) args
                         store obj (reg2val r)
       Nothing => idris_crash $ "MKCON name not found: " ++ show n

getInstIR i (MKCLOSURE r n missingN args) = do
  let missing = cast {to=Integer} missingN
  let len = cast {to=Integer} $ length args
  let totalArgsExpected = missing + len
  if totalArgsExpected > (cast CLOSURE_MAX_ARGS) then idris_crash $ "ERROR : too many closure arguments: " ++ show totalArgsExpected ++ " > " ++ show CLOSURE_MAX_ARGS else do
  let header = (header OBJECT_TYPE_ID_CLOSURE) + (missing * 0x10000) + len
  newObj <- dynamicAllocate $ ConstI64 (8 + 8 * len)
  putObjectHeader newObj (ConstI64 $ header)
  funcPtr <- (if (totalArgsExpected <= (cast FAT_CLOSURE_LIMIT))
             then
               assignSSA $ "bitcast %FuncPtrArgs" ++ show totalArgsExpected ++ " @" ++ (safeName n) ++ " to %FuncPtr"
             else do
               assignSSA $ "bitcast %FuncPtrClosureEntry @" ++ (safeName n) ++ "$$closureEntry to %FuncPtr"
               )

  putObjectSlot newObj 1 (SSA FuncPtr funcPtr)
  for_ (enumerate args) (\iv => do
      let (i, arg) = iv
      argObj <- load {t=IRObjPtr} (reg2val arg)
      putObjectSlot newObj (cast $ i+2) argObj
      pure ()
                              )
  appendCode $ "  store " ++ toIR newObj ++ ", %ObjPtr* " ++ toIR r ++ "Var"

getInstIR i (APPLY r fun arg) = do
  hp <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpVar"
  hpLim <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpLimVar"
  let base = "%RuntimePtr %BaseArg"

  funV <- ((++) "%FuncPtr ") <$> (assignSSA $ "load %FuncPtr, %FuncPtr* " ++ toIR fun ++ "Var")
  argV <- load (reg2val arg)

  result <- assignSSA $ "call fastcc %Return1 @idris_apply_closure(" ++ showSep ", " [hp, base, hpLim, funV, toIR argV] ++ ")"

  newHp <- assignSSA $ "extractvalue %Return1 " ++ result ++ ", 0"
  appendCode $ "store %RuntimePtr " ++ newHp ++ ", %RuntimePtr* %HpVar"
  newHpLim <- assignSSA $ "extractvalue %Return1 " ++ result ++ ", 1"
  appendCode $ "store %RuntimePtr " ++ newHpLim ++ ", %RuntimePtr* %HpLimVar"
  returnValue <- assignSSA $ "extractvalue %Return1 " ++ result ++ ", 2"
  appendCode $ "store %ObjPtr " ++ returnValue ++ ", %ObjPtr* " ++ toIR r ++ "Var"

  pure ()

getInstIR i (MKCONSTANT r (Ch c)) = do
  newObj <- dynamicAllocate (ConstI64 0)
  putObjectHeader newObj (ConstI64 $ ((cast c) + header OBJECT_TYPE_ID_CHAR))
  store newObj (reg2val r)
getInstIR i (MKCONSTANT r (I c)) = do
  obj <- cgMkInt (ConstI64 $ cast c)
  store obj (reg2val r)
getInstIR i (MKCONSTANT r (BI c)) = do
  -- FIXME: we treat Integers as bounded Ints -> should use GMP
  obj <- cgMkInt (ConstI64 $ c)
  store obj (reg2val r)
getInstIR i (MKCONSTANT r (Db d)) = do
  -- TODO: implement
  obj <- cgMkDouble (ConstF64 d)
  store obj (reg2val r)
getInstIR i (MKCONSTANT r WorldVal) = do
  obj <- mkCon 1337 []
  store obj (reg2val r)
getInstIR i (MKCONSTANT r (Str s)) = store !(mkStr i s) (reg2val r)

getInstIR i (CONSTCASE r alts def) = case findConstCaseType alts of
                                          IntType => getInstForConstCaseInt i r alts def
                                          IntegerType => getInstForConstCaseInt i r alts def
                                          StringType => getInstForConstCaseString i r alts def
                                          CharType => getInstForConstCaseChar i r alts def

getInstIR {conNames} i (CASE r alts def) =
  do let def' = fromMaybe [] def
     caseId <- mkVarName "case_"
     let labelEnd = caseId ++ "_end"
     o1 <- load $ reg2val r
     header <- getObjectHeader o1
     -- object tag is stored in the least significat 32 bits of header
     scrutinee <- assignSSA $ "and i64 " ++ (show 0xffffffff) ++ ", " ++ showWithoutType header
     appendCode $ "  switch i64 " ++ scrutinee ++ ", label %" ++ caseId ++ "_default [ " ++ (showSep "\n      " (map (makeCaseLabel caseId) alts)) ++ " ]"
     appendCode $ caseId ++ "_default:"
     traverse (getInstIRWithComment i) def'
     appendCode $ "br label %" ++ labelEnd
     traverse (makeCaseAlt caseId) alts
     appendCode $ labelEnd ++ ":"
     pure ()
  where
    makeCaseAlt : String -> (Either Int Name, List VMInst) -> Codegen ()
    makeCaseAlt caseId (Left c, is) = do
      appendCode $ caseId ++ "_tag_is_" ++ (show c) ++ ":"
      traverse_ (getInstIRWithComment i) is
      appendCode $ "br label %" ++ caseId ++ "_end"
    makeCaseAlt caseId (Right n, is) =
      case lookup n conNames of
           Just nameId => do
             appendCode $ "; " ++ (show n) ++ " -> " ++ (show nameId)
             appendCode (caseId ++ "_name_is_" ++ (show (makeNameId nameId)) ++ ":")
             traverse_ (getInstIRWithComment i) is
             appendCode ("br label %" ++ caseId ++ "_end")
           Nothing => idris_crash $ "name for case not found: " ++ show n

getInstIR i (CALL r tailpos n args) =
  do argsV <- traverse prepareArg args
     hp <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpVar"
     hpLim <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpLimVar"
     let base = "%RuntimePtr %BaseArg"
     result <- assignSSA $ "call fastcc %Return1 @" ++ (safeName n) ++ "(" ++ showSep ", " (hp::base::hpLim::argsV) ++ ")"

     newHp <- assignSSA $ "extractvalue %Return1 " ++ result ++ ", 0"
     appendCode $ "store %RuntimePtr " ++ newHp ++ ", %RuntimePtr* %HpVar"
     newHpLim <- assignSSA $ "extractvalue %Return1 " ++ result ++ ", 1"
     appendCode $ "store %RuntimePtr " ++ newHpLim ++ ", %RuntimePtr* %HpLimVar"
     returnValue <- assignSSA $ "extractvalue %Return1 " ++ result ++ ", 2"
     appendCode $ "store %ObjPtr " ++ returnValue ++ ", %ObjPtr* " ++ toIR r ++ "Var"
     pure ()

getInstIR i (PROJECT r o pos) = do
  obj <- load {t=IRObjPtr} (reg2val o)
  slot <- getObjectSlotT {t=IRObjPtr} obj (pos+1)
  store slot (reg2val r)

getInstIR i (EXTPRIM r n args) =
  do argsV <- traverse prepareArg args
     hp <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpVar"
     hpLim <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpLimVar"
     let base = "%RuntimePtr %BaseArg"
     result <- assignSSA $ "call fastcc %Return1 @_extprim_" ++ (safeName n) ++ "(" ++ showSep ", " (hp::base::hpLim::argsV) ++ ")"

     newHp <- assignSSA $ "extractvalue %Return1 " ++ result ++ ", 0"
     appendCode $ "store %RuntimePtr " ++ newHp ++ ", %RuntimePtr* %HpVar"
     newHpLim <- assignSSA $ "extractvalue %Return1 " ++ result ++ ", 1"
     appendCode $ "store %RuntimePtr " ++ newHpLim ++ ", %RuntimePtr* %HpLimVar"
     returnValue <- SSA IRObjPtr <$> assignSSA ("extractvalue %Return1 " ++ result ++ ", 2")
     store returnValue (reg2val r)

getInstIR i START = pure ()
getInstIR i inst = idris_crash $ ";=============\n; NOT IMPLEMENTED: " ++ show inst ++ "\n;=============\n"

getInstIRWithComment : {auto conNames : SortedMap Name Int} -> Int -> VMInst -> Codegen ()
getInstIRWithComment i instr = do
  appendCode (instrAsComment instr)
  getInstIR i instr

getFunIR : Bool -> SortedMap Name Int -> Int -> Name -> List Reg -> List VMInst -> Codegen ()
getFunIR debug conNames i n args body = do
    fargs <- traverse argIR args
    let visibility = if debug then "external" else "private"
    appendCode ("\n\ndefine " ++ visibility ++ " fastcc %Return1 @" ++ safeName n ++ "(" ++ (showSep ", " $ prepareArgCallConv fargs) ++ ") {")
    appendCode "entry:"
    appendCode funcEntry
    traverse_ appendCode (map copyArg args)
    traverse_ (getInstIRWithComment i) body
    appendCode funcReturn
    appendCode "}\n"
  where
    copyArg : Reg -> String
    copyArg (Loc i) = let r = show i in "  %v" ++ r ++ "Var = alloca %ObjPtr\n  store %ObjPtr %v" ++ r ++ ", %ObjPtr* %v" ++ r ++ "Var"
    copyArg _ = idris_crash "not an argument"

getFunIRClosureEntry : Bool -> SortedMap Name Int -> Int -> Name -> (args : List Int) -> {auto ok : NonEmpty args} -> List VMInst -> Codegen ()
getFunIRClosureEntry debug conNames i n args body = do
    let visibility = if debug then "external" else "private"
    appendCode ("\n\ndefine " ++ visibility ++ " fastcc %Return1 @" ++ safeName n ++ "$$closureEntry(" ++ (showSep ", " $ prepareArgCallConv ["%ObjPtr %clObj", "%ObjPtr %lastArg"]) ++ ") {")
    appendCode "entry:"
    appendCode funcEntry
    traverse_ copyArg (enumerate $ init args)
    appendCode $ "  %v" ++ (show $ last args) ++ "Var = alloca %ObjPtr"
    store (SSA IRObjPtr "%lastArg") (reg2val $ Loc $ last args)
    traverse_ (getInstIRWithComment i) body
    appendCode funcReturn
    appendCode "}\n"
  where
    copyArg : (Int, Int) -> Codegen ()
    copyArg (index, i) =
      let clObj = SSA IRObjPtr "%clObj" in do
        appendCode $ "  %v" ++ show i ++ "Var = alloca %ObjPtr"
        arg <- getObjectSlotT clObj (index + 2)
        store arg (reg2val (Loc i))
    copyArg _ = idris_crash "not an argument"

mk_prim__bufferNew : Vect 2 (IRValue IRObjPtr) -> Codegen ()
mk_prim__bufferNew [sizeObj, _] = do
  size <- unboxInt' sizeObj
  -- TODO: safety check: size < 2^32
  hdrValue <- mkOr (ConstI64 $ header OBJECT_TYPE_ID_BUFFER) size
  newObj <- dynamicAllocate size
  putObjectHeader newObj hdrValue
  store newObj (reg2val RVal)

mk_prim__bufferSize : Vect 1 (IRValue IRObjPtr) -> Codegen ()
mk_prim__bufferSize [arg0] = do
  hdr <- getObjectHeader arg0
  size <- mkAnd hdr (ConstI64 0xffffffff)
  sizeInt <- cgMkInt size
  store sizeInt (reg2val RVal)

mk_prim__bufferGetByte : Vect 3 (IRValue IRObjPtr) -> Codegen ()
mk_prim__bufferGetByte [buf, offsetObj, _] = do
  -- TODO: size check in safe mode
  --hdr <- getObjectHeader buf
  --size <- mkAnd hdr (ConstI64 0xffffffff)
  offset <- unboxInt' offsetObj
  payloadStart <- getObjectSlotAddr {t=I8} buf 1
  bytePtr <- getElementPtr payloadStart offset
  byte <- load bytePtr
  val <- mkZext {to=I64} byte
  store !(cgMkInt val) (reg2val RVal)

mk_prim__bufferGetDouble : Vect 3 (IRValue IRObjPtr) -> Codegen ()
mk_prim__bufferGetDouble [buf, offsetObj, _] = do
  -- TODO: size check in safe mode
  --hdr <- getObjectHeader buf
  --size <- mkAnd hdr (ConstI64 0xffffffff)
  offset <- unboxInt' offsetObj
  payloadStart <- getObjectSlotAddr {t=I8} buf 1
  bytePtr <- getElementPtr payloadStart offset
  doublePtr <- bitcast bytePtr
  val <- load doublePtr
  store !(cgMkDouble val) (reg2val RVal)

mk_prim__bufferSetDouble : Vect 4 (IRValue IRObjPtr) -> Codegen ()
mk_prim__bufferSetDouble [buf, offsetObj, valObj, _] = do
  -- TODO: size check in safe mode
  --hdr <- getObjectHeader buf
  --size <- mkAnd hdr (ConstI64 0xffffffff)
  offset <- unboxInt' offsetObj
  payloadStart <- getObjectSlotAddr {t=I8} buf 1
  bytePtr <- getElementPtr payloadStart offset
  doublePtr <- bitcast bytePtr
  val <- unboxFloat64' valObj
  store val doublePtr

mk_prim__bufferGetInt : Vect 3 (IRValue IRObjPtr) -> Codegen ()
mk_prim__bufferGetInt [buf, offsetObj, _] = do
  -- TODO: size check in safe mode
  --hdr <- getObjectHeader buf
  --size <- mkAnd hdr (ConstI64 0xffffffff)
  offset <- unboxInt' offsetObj
  payloadStart <- getObjectSlotAddr {t=I8} buf 1
  bytePtr <- getElementPtr payloadStart offset
  intPtr <- bitcast {to=I64} bytePtr
  val <- load intPtr
  store !(cgMkInt val) (reg2val RVal)

mk_prim__bufferGetInt32 : Vect 3 (IRValue IRObjPtr) -> Codegen ()
mk_prim__bufferGetInt32 [buf, offsetObj, _] = do
  -- TODO: size check in safe mode
  --hdr <- getObjectHeader buf
  --size <- mkAnd hdr (ConstI64 0xffffffff)
  offset <- unboxInt' offsetObj
  payloadStart <- getObjectSlotAddr {t=I8} buf 1
  bytePtr <- getElementPtr payloadStart offset
  intPtr <- bitcast {to=I32} bytePtr
  val32 <- load intPtr
  val <- mkZext val32
  store !(cgMkInt val) (reg2val RVal)

mk_prim__bufferSetInt : Vect 4 (IRValue IRObjPtr) -> Codegen ()
mk_prim__bufferSetInt [buf, offsetObj, valObj, _] = do
  -- TODO: size check in safe mode
  --hdr <- getObjectHeader buf
  --size <- mkAnd hdr (ConstI64 0xffffffff)
  offset <- unboxInt' offsetObj
  payloadStart <- getObjectSlotAddr {t=I8} buf 1
  bytePtr <- getElementPtr payloadStart offset
  intPtr <- bitcast {to=I64} bytePtr
  val <- unboxInt' valObj
  store val intPtr

mk_prim__bufferSetInt32 : Vect 4 (IRValue IRObjPtr) -> Codegen ()
mk_prim__bufferSetInt32 [buf, offsetObj, valObj, _] = do
  -- TODO: size check in safe mode
  --hdr <- getObjectHeader buf
  --size <- mkAnd hdr (ConstI64 0xffffffff)
  offset <- unboxInt' offsetObj
  payloadStart <- getObjectSlotAddr {t=I8} buf 1
  bytePtr <- getElementPtr payloadStart offset
  intPtr <- bitcast {to=I32} bytePtr
  val <- mkTrunc {to=I32} !(unboxInt' valObj)
  store val intPtr


mk_prim__bufferGetString : Vect 4 (IRValue IRObjPtr) -> Codegen ()
mk_prim__bufferGetString [buf, offsetObj, lengthObj, _] = do
  -- TODO: size check in safe mode
  --hdr <- getObjectHeader buf
  --size <- mkAnd hdr (ConstI64 0xffffffff)
  offset <- unboxInt' offsetObj
  length <- unboxInt' lengthObj
  payloadStart <- getObjectSlotAddr {t=I8} buf 1
  bytePtr <- getElementPtr payloadStart offset

  newStr <- dynamicAllocate length
  newHeader <- mkBinOp "or" length (ConstI64 $ header OBJECT_TYPE_ID_STR)
  putObjectHeader newStr newHeader
  strPayload <- getObjectSlotAddr {t=I8} newStr 1
  appendCode $ "  call void @llvm.memcpy.p0i8.p0i8.i64(" ++ toIR strPayload ++ ", " ++ toIR bytePtr ++ ", " ++ toIR length ++ ", i1 false)"
  store newStr (reg2val RVal)

mk_prim__bufferSetString : Vect 4 (IRValue IRObjPtr) -> Codegen ()
mk_prim__bufferSetString [buf, offsetObj, valObj, _] = do
  -- TODO: size check in safe mode
  --hdr <- getObjectHeader buf
  --size <- mkAnd hdr (ConstI64 0xffffffff)
  offset <- unboxInt' offsetObj
  payloadStart <- getObjectSlotAddr {t=I8} buf 1
  bytePtr <- getElementPtr payloadStart offset
  strHeader <- getObjectHeader valObj
  strLength <- mkAnd strHeader (ConstI64 0xffffffff)
  strPayload <- getObjectSlotAddr {t=I8} valObj 1
  appendCode $ "  call void @llvm.memcpy.p0i8.p0i8.i64(" ++ toIR bytePtr ++ ", " ++ toIR strPayload ++ ", " ++ toIR strLength ++ ", i1 false)"


mk_prim__bufferWriteToFile : Vect 5 (IRValue IRObjPtr) -> Codegen ()
mk_prim__bufferWriteToFile [_, fname, buf, sizeObj, _] = do
  maxSize <- unboxInt' sizeObj
  retval <- call {t=I64} "ccc" "@idris_rts_write_buffer_to_file" [toIR fname, toIR buf, toIR maxSize]
  result <- cgMkInt retval
  store result (reg2val RVal)

mk_prim__bufferReadFromFile : Vect 3 (IRValue IRObjPtr) -> Codegen ()
mk_prim__bufferReadFromFile [_, fname, _] = do
  newObj <- foreignCall {t=IRObjPtr} "@idris_rts_read_buffer_from_file" [toIR fname]
  store newObj (reg2val RVal)

mk_prim__nullAnyPtr : Vect 1 (IRValue IRObjPtr) -> Codegen ()
mk_prim__nullAnyPtr [p] = do
  ptrAsInt <- SSA I64 <$> assignSSA ("ptrtoint " ++ toIR p ++ " to i64")
  ptrIsZero <- icmp "eq" (ConstI64 0) ptrAsInt
  result <- cgMkInt !(mkZext ptrIsZero)
  store result (reg2val RVal)

mk_prim__isBuffer : Vect 1 (IRValue IRObjPtr) -> Codegen ()
mk_prim__isBuffer [obj] = do
  hdr <- getObjectHeader obj
  objType <- mkAnd (ConstI64 0xffffffff00000000) hdr
  isBuf <- icmp "ne" objType (ConstI64 $ header OBJECT_TYPE_ID_BUFFER)
  result <- cgMkInt !(mkZext isBuf)
  store result (reg2val RVal)

mk_prim__currentDir : Vect 1 (IRValue IRObjPtr) -> Codegen ()
mk_prim__currentDir [_] = do
  dummy <- mkStr 1 "/tmp"
  store dummy (reg2val RVal)


mkSupport : {n : Nat} -> Name -> (Vect n (IRValue IRObjPtr) -> Codegen ()) -> String
mkSupport {n} name f = runCodegen (do
          appendCode ("define private fastcc %Return1 @" ++ safeName name ++ "(" ++ (showSep ", " $ prepareArgCallConv $ toList $ map toIR args) ++ ") {")
          appendCode funcEntry
          f args
          appendCode funcReturn
          appendCode "\n}\n"
          )
  where
  args : Vect n (IRValue IRObjPtr)
  args = map (\i => SSA IRObjPtr $ "%arg" ++ show (finToNat i)) range

supportPrelude : String
supportPrelude = fastAppend [
    mkSupport (NS ["Buffer", "Data"] (UN "prim__newBuffer")) mk_prim__bufferNew
  , mkSupport (NS ["Buffer", "Data"] (UN "prim__bufferSize")) mk_prim__bufferSize
  , mkSupport (NS ["Buffer", "Data"] (UN "prim__getByte")) mk_prim__bufferGetByte
  , mkSupport (NS ["Buffer", "Data"] (UN "prim__getDouble")) mk_prim__bufferGetDouble
  , mkSupport (NS ["Buffer", "Data"] (UN "prim__setDouble")) mk_prim__bufferSetDouble
  , mkSupport (NS ["Buffer", "Data"] (UN "prim__getInt")) mk_prim__bufferGetInt
  , mkSupport (NS ["Buffer", "Data"] (UN "prim__setInt")) mk_prim__bufferSetInt
  , mkSupport (NS ["Buffer", "Data"] (UN "prim__getInt32")) mk_prim__bufferGetInt32
  , mkSupport (NS ["Buffer", "Data"] (UN "prim__setInt32")) mk_prim__bufferSetInt32
  , mkSupport (NS ["Buffer", "Data"] (UN "prim__getString")) mk_prim__bufferGetString
  , mkSupport (NS ["Buffer", "Data"] (UN "prim__setString")) mk_prim__bufferSetString
  , mkSupport (NS ["Buffer", "Data"] (UN "prim__writeBuffer")) mk_prim__bufferWriteToFile
  , mkSupport (NS ["Buffer", "Data"] (UN "prim__readBufferFromFile")) mk_prim__bufferReadFromFile
  , mkSupport (NS ["Buffer", "Data"] (UN "prim__isBuffer")) mk_prim__isBuffer
  , mkSupport (NS ["Directory", "System"] (UN "prim_currentDir")) mk_prim__currentDir
  , mkSupport (NS ["PrimIO"] (UN "prim__nullAnyPtr")) mk_prim__nullAnyPtr
  ]

export
getVMIR : Bool -> SortedMap Name Int -> (Int, (Name, VMDef)) -> String
getVMIR debug conNames (i, n, MkVMFun args body) = (runCodegen $ getFunIR debug conNames ((2*i)+1000) n (map Loc args) body) ++ closureEntry where
  closureEntry : String
  closureEntry = case args of
                      [] => ""
                      neArgs@(_::_) => runCodegen $ getFunIRClosureEntry debug conNames ((2*i + 1)+1000) n neArgs body
getVMIR _ _ _ = ""

funcPtrTypes : String
funcPtrTypes = fastAppend $ map funcPtr (rangeFromTo 0 FAT_CLOSURE_LIMIT) where
  funcPtr : Int -> String
  funcPtr i = "%FuncPtrArgs" ++ (show (i + 1)) ++ " = type %Return1 (%RuntimePtr, %RuntimePtr, %RuntimePtr" ++ repeatStr ", %ObjPtr" (integerToNat $ cast (i+1)) ++ ")*\n"

export
closureHelper : String
closureHelper = fastAppend [
  funcPtrTypes,
  "\ndefine fastcc %Return1 @idris_apply_closure(%RuntimePtr %HpArg, %RuntimePtr %BaseArg, %RuntimePtr %HpLimArg, %ObjPtr %closureObjArg, %ObjPtr %argumentObjArg) {\n",
  runCodegen applyClosureHelperFunc,
  "\n}\n\n",
  supportPrelude
  ]
