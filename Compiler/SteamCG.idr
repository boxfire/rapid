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

data IRLabel = MkLabel String

ToIR IRLabel where
  toIR (MkLabel l) = "label %" ++ l
  showWithoutType (MkLabel l) = "%" ++ l

beginLabel : IRLabel -> Codegen ()
beginLabel (MkLabel l) = appendCode (l ++ ":")

genLabel : String -> Codegen IRLabel
genLabel s = MkLabel <$> mkVarName ("glbl_" ++ s)

data IRValue : IRType -> Type where
  ConstI64 : Int -> IRValue I64
  SSA : (t : IRType) ->  String -> IRValue t

ToIR (IRValue t) where
  toIR {t} (SSA t s) = (show t) ++ " " ++ s
  toIR (ConstI64 i) = "i64 " ++ (show i)

  showWithoutType (SSA _ n) = n
  showWithoutType (ConstI64 i) = show i

reg2val : Reg -> IRValue (Pointer IRObjPtr)
reg2val (Loc i) = SSA (Pointer IRObjPtr) ("%v" ++ show i ++ "Var")
reg2val RVal = SSA (Pointer IRObjPtr) ("%rvalVar")
reg2val Discard = SSA (Pointer IRObjPtr) "undef"

load : {auto t : IRType} -> IRValue (Pointer t) -> Codegen (IRValue t)
load {t} mv = do
  loaded <- assignSSA $ "load " ++ (show t) ++ ", " ++ (toIR mv)
  pure $ SSA t loaded

store : {t : IRType} -> IRValue t -> IRValue (Pointer t) -> Codegen ()
store {t} v dst = do
  appendCode $ "  store " ++ (toIR v) ++ ", " ++ (toIR dst)

icmp : {t : IRType} -> String -> IRValue t -> IRValue t -> Codegen (IRValue I1)
icmp {t} cond a b = do
  compare <- assignSSA $ "icmp " ++ cond ++ " " ++ (show t) ++ " " ++ showWithoutType a ++ ", " ++ showWithoutType b
  pure $ SSA I1 compare

mkZext : {to : IRType} -> IRValue from -> Codegen (IRValue to)
mkZext {to} val = (SSA to) <$> assignSSA ("zext " ++ toIR val ++ " to " ++ show to)

phi : {t : IRType} -> List (IRValue t, IRLabel) -> Codegen (IRValue t)
phi xs = (SSA t) <$> assignSSA ("phi " ++ show t ++ " " ++ showSep ", " (map getEdge xs)) where
  getEdge : (IRValue t, IRLabel) -> String
  getEdge (val, lbl) = "[ " ++ showWithoutType val ++ ", " ++ showWithoutType lbl ++ " ]"

getElementPtr : {t : IRType} -> IRValue (Pointer t) -> IRValue ot -> Codegen (IRValue (Pointer t))
getElementPtr {t} ptr offset =
  SSA (Pointer t) <$> assignSSA ("getelementptr inbounds " ++ show t ++ ", " ++ toIR ptr ++ ", " ++ toIR offset)

bitcast : {from : IRType} -> {to : IRType} -> IRValue from -> Codegen (IRValue (Pointer to))
bitcast {from} {to} val = (SSA (Pointer to)) <$> assignSSA ("bitcast " ++ toIR val ++ " to " ++ show to)

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

mkMul : {t : IRType} -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkMul = mkBinOp "mul"

mkSub : {t : IRType} -> IRValue t -> IRValue t -> Codegen (IRValue t)
mkSub = mkBinOp "sub"

--and : Value -> Value -> CodeGen Value
--and v1 v1 = toIR v1 ++ toIR v2

getObjectSlot : IRValue IRObjPtr -> Int -> Codegen (IRValue I64)
getObjectSlot obj n = do
  i64ptr <- assignSSA $ "bitcast " ++ toIR obj ++ " to i64*"
  slotPtr <- assignSSA $ "getelementptr i64, i64* " ++ i64ptr ++ ", i64 " ++ show n
  load {t=I64} (SSA (Pointer I64) slotPtr)

getObjectSlotAddr : {t : IRType} -> IRValue IRObjPtr -> Int -> Codegen (IRValue (Pointer t))
getObjectSlotAddr obj n = do
  i64ptr <- assignSSA $ "bitcast " ++ toIR obj ++ " to i64*"
  slotPtr <- assignSSA $ "getelementptr i64, i64* " ++ i64ptr ++ ", i64 " ++ show n
  slotPtrT <- assignSSA $ "bitcast i64* " ++ slotPtr ++ " to " ++ show t ++ "*"
  pure (SSA (Pointer t) slotPtrT)

getObjectSlotT : {t : IRType} -> IRValue IRObjPtr -> Int -> Codegen (IRValue t)
getObjectSlotT obj n = do
  i64ptr <- assignSSA $ "bitcast " ++ toIR obj ++ " to i64*"
  slotPtr <- assignSSA $ "getelementptr i64, i64* " ++ i64ptr ++ ", i64 " ++ show n
  slotPtrT <- assignSSA $ "bitcast i64* " ++ slotPtr ++ " to " ++ show t ++ "*"
  load {t=t} (SSA (Pointer t) slotPtrT)

getObjectHeader : IRValue IRObjPtr -> Codegen (IRValue I64)
getObjectHeader o = getObjectSlotT o 0

putObjectSlotG : {t : IRType} -> IRValue IRObjPtr -> IRValue I64 -> IRValue t -> Codegen ()
putObjectSlotG {t} obj pos val = do
  i64ptr <- assignSSA $ "bitcast " ++ toIR obj ++ " to i64*"
  slotPtr <- assignSSA $ "getelementptr i64, i64* " ++ i64ptr ++ ", " ++ toIR pos
  slotPtrObj <- assignSSA $ "bitcast i64* " ++ slotPtr ++ " to " ++ show t ++ "*"
  appendCode $ "  store " ++ toIR val ++ ", " ++ show t ++ " * " ++ slotPtrObj

putObjectSlot : {t : IRType} -> IRValue IRObjPtr -> Int -> IRValue t -> Codegen ()
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
  totalSize <- SSA I64 <$> assignSSA ("add " ++ (toIR payloadSize) ++ ", " ++ HEADER_SIZE)

  hp <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpVar"
  hpLim <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpLimVar"
  let base = "%RuntimePtr %BaseArg"

  allocated <- assignSSA $ "call hhvmcc %Return1 @rapid_allocate(" ++ showSep ", " [hp, base, hpLim] ++ ", "++(toIR totalSize)++") alwaysinline optsize nounwind"
  newHp <- assignSSA $ "extractvalue %Return1 " ++ allocated ++ ", 0"
  appendCode $ "store %RuntimePtr " ++ newHp ++ ", %RuntimePtr* %HpVar"
  newHpLim <- assignSSA $ "extractvalue %Return1 " ++ allocated ++ ", 1"
  appendCode $ "store %RuntimePtr " ++ newHpLim ++ ", %RuntimePtr* %HpLimVar"
  SSA IRObjPtr <$> assignSSA ("extractvalue %Return1 " ++ allocated ++ ", 2")

header : Int -> Integer
header i = (cast i) `prim__shl_Integer` 32

applyClosureHelperFunc : Codegen ()
applyClosureHelperFunc = do
  appendCode $ funcEntry

  let maxArgs = 7

  let closureObj = SSA IRObjPtr "%closureObjArg"
  let argValue = SSA IRObjPtr "%argumentObjArg"
  closureHeader <- getObjectHeader closureObj
  argCount <- assignSSA $ "and i64 65535, " ++ showWithoutType closureHeader
  missingArgCountShifted <- assignSSA $ "and i64 4294901760, " ++ showWithoutType closureHeader
  missingArgCount <- assignSSA $ "lshr i64 " ++ missingArgCountShifted ++ ", 16"
  isSaturated <- assignSSA $ "icmp eq i64 1, " ++ missingArgCount
  labelName <- mkVarName "closure_saturated"
  appendCode $ "br i1 " ++ isSaturated ++ ", label %" ++ labelName ++ "_yes, label %" ++ labelName ++ "_no"
  appendCode $ labelName ++ "_yes:"
  funcPtrI64 <- getObjectSlot closureObj 1
  func <- assignSSA $ "inttoptr " ++ (toIR funcPtrI64) ++ " to %FuncPtrArgs" ++ show (maxArgs+1)

  let hp = "%RuntimePtr %HpArg"
  let base = "%RuntimePtr %BaseArg"
  let hpLim = "%RuntimePtr %HpLimArg"

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

  --appliedArgCount <- SSA I64 <$> assignSSA ("add i64 " ++ argCount ++ ", 1")
  appliedArgCount <- mkAdd (SSA I64 argCount) (ConstI64 1)
  newArgsSize <- mkMul appliedArgCount (ConstI64 8)
  -- add 8 bytes for entry func ptr
  newPayloadSize <- mkAdd newArgsSize (ConstI64 8)
  newClosureTotalSize <- mkAdd newPayloadSize (ConstI64 $ cast HEADER_SIZE)
  newClosure <- dynamicAllocate newPayloadSize

  let newHeader = ConstI64 $ cast $ header OBJECT_TYPE_ID_CLOSURE
  newMissingArgs <- mkSub (SSA I64 missingArgCount) (ConstI64 1)
  newMissingArgsShifted <- mkBinOp "shl" newMissingArgs (ConstI64 16)

  newHeader' <- mkOr newHeader newMissingArgsShifted
  newHeader'' <- mkOr newHeader' appliedArgCount

  appendCode $ "  call void @llvm.memcpy.p0i8.p0i8.i64(" ++ toIR newClosure ++ ", " ++ toIR closureObj ++ ", " ++ toIR newClosureTotalSize ++ ", i1 false)"
  putObjectHeader newClosure newHeader''

  newArgSlotNumber <- mkAdd appliedArgCount (ConstI64 1)
  putObjectSlotG newClosure newArgSlotNumber argValue

  store newClosure (reg2val RVal)

  appendCode $ funcReturn

  appendCode $ applyClosure ++ "_error:"
  appendCode $ "call ccc void @idris_rts_crash(i64 42)"
  appendCode $ "ret %Return1 undef"

cgMkInt : IRValue I64 -> Codegen (IRValue IRObjPtr)
cgMkInt val = do
  newObj <- dynamicAllocate (ConstI64 8)
  putObjectHeader newObj (ConstI64 $ cast $ header OBJECT_TYPE_ID_INT)
  putObjectSlotG newObj (ConstI64 1) val
  pure newObj

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

mkStr : Int -> String -> Codegen (IRValue IRObjPtr)
mkStr i s = do
  let len = cast {to=Integer} $ length s
  cn <- addConstant i $ "private unnamed_addr constant [" ++ show len ++ " x i8] c\"" ++ (getStringIR s) ++ "\""
  cn <- assignSSA $ "bitcast [" ++ show len ++ " x i8]* "++cn++" to i8*"
  let newHeader = ConstI64 $ cast $ (header OBJECT_TYPE_ID_STR) + len
  newObj <- dynamicAllocate (ConstI64 $ cast len)
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
  putObjectHeader newObj (ConstI64 tag)
  let enumArgs = enumerate args
  for enumArgs (\x => let (i, arg) = x in do
                            arg <- load (reg2val arg)
                            putObjectSlotG newObj (ConstI64 (1+i)) arg
                          )
  pure newObj

unboxInt : IRValue (Pointer IRObjPtr) -> Codegen (IRValue I64)
unboxInt src = getObjectSlotT {t=I64} !(load src) 1

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
makeConstCaseLabel caseId (c,_) = "const case error: " ++ (showConstant c)

makeCaseLabel : String -> (Either Int Name, a) -> String
makeCaseLabel caseId (Left i,_) = "i64 " ++ show i ++ ", label %" ++ caseId ++ "_tag_is_" ++ show i
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

findConstCaseType : List (Constant, List VMInst) -> Constant
findConstCaseType [] = idris_crash "empty const case"
findConstCaseType ((I _,_)::_) = IntType
findConstCaseType ((BI _,_)::_) = IntegerType
findConstCaseType ((Str _,_)::_) = StringType
findConstCaseType _ = idris_crash "unknwon const case type"

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

mutual
getInstForConstCaseInt : Int -> Reg -> List (Constant, List VMInst) -> Maybe (List VMInst) -> Codegen ()
getInstForConstCaseInt i r alts def =
  do let def' = fromMaybe [] def
     caseId <- mkVarName "case_"
     let labelEnd = caseId ++ "_end"
     scrutinee <- unboxInt (reg2val r)
     appendCode $ "  switch " ++ toIR scrutinee ++ ", label %" ++ caseId ++ "_default [ " ++ (showSep "\n      " (map (makeConstCaseLabel caseId) alts)) ++ " ]"
     appendCode $ caseId ++ "_default:"
     uniq <- getUnique
     traverse (getInstIR i) def'
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
    makeCaseAlt caseId (BI c, is) = makeCaseAlt caseId (I $ cast c, is)
    makeCaseAlt _ (c, _) = appendCode $ "ERROR: constcase must be Int, got: " ++ show c

getInstForConstCaseString : Int -> Reg -> List (Constant, List VMInst) -> Maybe (List VMInst) -> Codegen ()
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

     traverse (getInstIR i) def'
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
      traverse_ (getInstIR i) is
      appendCode $ "br " ++ toIR labelEnd
      beginLabel labelAltNext
    makeCaseAlt _ _ _ (_, c, _) = appendCode $ "ERROR: constcase must be Str, got: " ++ show c

getInstIR : Int -> VMInst -> Codegen ()
getInstIR i (DECLARE (Loc r)) = appendCode $ "  %v" ++ show r ++ "Var = alloca %ObjPtr"
getInstIR i (ASSIGN r src) = do
  value <- assignSSA $ "load %ObjPtr, %ObjPtr* " ++ toIR src ++ "Var"
  appendCode $ "  store %ObjPtr " ++ value ++ ", %ObjPtr* " ++ toIR r ++ "Var"

getInstIR i (OP r StrHead [r1]) = do
  o1 <- load (reg2val r1)
  -- FIXME: this assumees strlen > 0
  --objHeader <- getObjectSlotT {t=I64} o1 0
  --strLength <- (SSA I64) <$> assignSSA "  and " ++ (toIR objHeader) ++ ", " ++ (show 0xffffffff)

  first8Bytes <- getObjectSlotT {t=I64} o1 1

  -- FIXME: this assumes LE-encoding
  -- FIXME: this assumes ASCII
  firstChar <- (SSA I64) <$> assignSSA ("  and " ++ (toIR first8Bytes) ++ ", " ++ (show 0xff))

  newCharObj <- dynamicAllocate (ConstI64 0)
  newHeader <- (SSA I64) <$> assignSSA ("  or " ++ (toIR firstChar) ++ ", " ++ (showWithoutType $ ConstI64 $ cast $ header OBJECT_TYPE_ID_STR))
  putObjectHeader newCharObj newHeader

  store newCharObj (reg2val r)

getInstIR i (OP r StrAppend [r1, r2]) = do
  o1 <- load (reg2val r1)
  o2 <- load (reg2val r2)
  h1 <- getObjectHeader o1
  h2 <- getObjectHeader o2
  l1 <- mkBinOp "and" (ConstI64 0xffffffff) h1
  l2 <- mkBinOp "and" (ConstI64 0xffffffff) h2
  newLength <- mkAdd l1 l2
  newStr <- dynamicAllocate newLength
  newHeader <- mkBinOp "or" newLength (ConstI64 $ cast $ header OBJECT_TYPE_ID_STR)

  str1 <- getObjectSlotAddr {t=I8} o1 1
  str2 <- getObjectSlotAddr {t=I8} o2 1

  newStrPayload1 <- getObjectSlotAddr {t=I8} newStr 1
  newStrPayload2 <- getElementPtr newStrPayload1 l1

  appendCode $ "  call void @llvm.memcpy.p0i8.p0i8.i64(" ++ toIR newStrPayload1 ++ ", " ++ toIR str1 ++ ", " ++ toIR l1 ++ ", i1 false)"
  appendCode $ "  call void @llvm.memcpy.p0i8.p0i8.i64(" ++ toIR newStrPayload2 ++ ", " ++ toIR str2 ++ ", " ++ toIR l2 ++ ", i1 false)"

  putObjectHeader newStr newHeader

  store newStr (reg2val r)

getInstIR i (OP r (Cast IntegerType StringType) [r1]) = do
  theIntObj <- load (reg2val r1)
  theInt <- getObjectSlotT {t=I64} theIntObj 1

  -- max size of 2^64 = 20 + (optional "-" prefix) + NUL byte (from snprintf)
  newStr <- dynamicAllocate (ConstI64 24)
  strPayload <- getObjectSlotAddr {t=I8} newStr 1
  length <- (SSA I64) <$> assignSSA ("call ccc i64 @idris_rts_int_to_str(" ++ toIR strPayload ++ ", " ++ toIR theInt ++ ")")
  newHeader <- mkAdd (ConstI64 $ cast $ header OBJECT_TYPE_ID_STR) length
  putObjectHeader newStr newHeader
  store newStr (reg2val r)

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

getInstIR i (OP r (EQ CharType) [r1, r2]) = do
  -- Two Chars are equal, when their headers are equal
  o1 <- load (reg2val r1)
  o2 <- load (reg2val r2)
  i1 <- getObjectHeader o1
  i2 <- getObjectHeader o2
  cmp_i1 <- icmp "eq" i1 i2
  cmp_i64 <- assignSSA $ "zext " ++ toIR cmp_i1 ++ " to i64"
  obj <- cgMkInt (SSA I64 cmp_i64)
  store obj (reg2val r)
getInstIR i (OP r (EQ IntType) [r1, r2]) = do
  i1 <- unboxInt (reg2val r1)
  i2 <- unboxInt (reg2val r2)
  vsum_i1 <- icmp "eq" i1 i2
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

getInstIR i (MKCON r (Just tag) args) = do
  obj <- mkCon tag args
  store obj (reg2val r)

getInstIR i (MKCLOSURE r n missingN args) = do
  let missing = cast {to=Integer} missingN
  let len = cast {to=Integer} $ length args
  let totalArgsExpected = missing + len
  let header = (header OBJECT_TYPE_ID_CLOSURE) + (missing * 0x10000) + len
  newObj <- dynamicAllocate $ ConstI64 (8 + 8 * (cast len))
  putObjectHeader newObj (ConstI64 $ cast header)
  funcPtr <- assignSSA $ "bitcast %Return1 (%RuntimePtr,%RuntimePtr,%RuntimePtr" ++ (repeatStr ", %ObjPtr" (integerToNat totalArgsExpected)) ++ ")* @" ++ (safeName n) ++ " to %FuncPtr"
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
  argV <- load (reg2val arg)

  result <- assignSSA $ "call hhvmcc %Return1 @idris_apply_closure(" ++ showSep ", " [hp, base, hpLim, funV, toIR argV] ++ repeatStr ", %ObjPtr undef" 6  ++ ")"

  newHp <- assignSSA $ "extractvalue %Return1 " ++ result ++ ", 0"
  appendCode $ "store %RuntimePtr " ++ newHp ++ ", %RuntimePtr* %HpVar"
  newHpLim <- assignSSA $ "extractvalue %Return1 " ++ result ++ ", 1"
  appendCode $ "store %RuntimePtr " ++ newHpLim ++ ", %RuntimePtr* %HpLimVar"
  returnValue <- assignSSA $ "extractvalue %Return1 " ++ result ++ ", 2"
  appendCode $ "store %ObjPtr " ++ returnValue ++ ", %ObjPtr* " ++ toIR r ++ "Var"

  pure ()

getInstIR i (MKCONSTANT r (Ch c)) = do
  newObj <- dynamicAllocate (ConstI64 0)
  putObjectHeader newObj (ConstI64 $ cast $ ((cast c) + header OBJECT_TYPE_ID_CHAR))
  store newObj (reg2val r)
getInstIR i (MKCONSTANT r (I c)) = do
  obj <- cgMkInt (ConstI64 c)
  store obj (reg2val r)
getInstIR i (MKCONSTANT r (BI c)) = do
  -- FIXME: we treat Integers as bounded Ints -> should use GMP
  obj <- cgMkInt (ConstI64 $ cast c)
  store obj (reg2val r)
getInstIR i (MKCONSTANT r WorldVal) = do
  obj <- mkCon 1337 []
  store obj (reg2val r)
getInstIR i (MKCONSTANT r (Str s)) = store !(mkStr i s) (reg2val r)

getInstIR i (CONSTCASE r alts def) = case findConstCaseType alts of
                                          IntType => getInstForConstCaseInt i r alts def
                                          IntegerType => getInstForConstCaseInt i r alts def
                                          StringType => getInstForConstCaseString i r alts def

getInstIR i (CASE r alts def) =
  do let def' = fromMaybe [] def
     caseId <- mkVarName "case_"
     let labelEnd = caseId ++ "_end"
     o1 <- load $ reg2val r
     header <- getObjectHeader o1
     -- object tag is stored in the least significat 32 bits of header
     scrutinee <- assignSSA $ "and i64 " ++ (show 0xffffffff) ++ ", " ++ showWithoutType header
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
    makeCaseAlt : String -> (Either Int Name, List VMInst) -> Codegen ()
    makeCaseAlt caseId (Left c, is) = do
      appendCode $ caseId ++ "_tag_is_" ++ (show c) ++ ":"
      uniq <- getUnique
      let nextI = uniq + (i * 100)
      traverse_ (getInstIR nextI) is
      appendCode $ "br label %" ++ caseId ++ "_end"
    makeCaseAlt _ (Right n, _) = appendCode $ "ERROR: case can only match on tag, got name: " ++ fullShow n

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

getInstIR i (PROJECT r o pos) = do
  obj <- load {t=IRObjPtr} (reg2val o)
  slot <- getObjectSlotT {t=IRObjPtr} obj (pos+1)
  store slot (reg2val r)

getInstIR i (EXTPRIM r n args) =
  do argsV <- traverse prepareArg args
     hp <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpVar"
     hpLim <- ((++) "%RuntimePtr ") <$> assignSSA "load %RuntimePtr, %RuntimePtr* %HpLimVar"
     let base = "%RuntimePtr %BaseArg"
     result <- assignSSA $ "call hhvmcc %Return1 @_extprim_" ++ (safeName n) ++ "(" ++ showSep ", " (hp::base::hpLim::argsV) ++ ")"
     pure ()

getInstIR i START = pure ()
getInstIR i inst = appendCode $ ";=============\n; NOT IMPLEMENTED: " ++ show inst ++ "\n;=============\n"

getFunIR : Bool -> Int -> String -> List Reg -> List VMInst -> Codegen ()
getFunIR debug i n args body = do
    fargs <- traverse argIR args
    let visibility = if debug then "external" else "private"
    appendCode ("\n\ndefine " ++ visibility ++ " hhvmcc %Return1 @" ++ (safeName' n) ++ "(" ++ (showSep ", " $ prepareArgCallConv fargs) ++ ") {")
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
getVMIR : Bool -> (Int, (String, VMDef)) -> String
getVMIR debug (i, n, MkVMFun args body) = runCodegen $ getFunIR debug i n (map Loc args) body
getVMIR _ _ = ""

export
closureHelper : String
closureHelper = "define hhvmcc %Return1 @idris_apply_closure(%RuntimePtr %HpArg, %RuntimePtr %BaseArg, %RuntimePtr %HpLimArg, %ObjPtr %closureObjArg, %ObjPtr %argumentObjArg, i8* %unused0, i8* %unused1, i8* %unused2, i8* %unused3, i8* %unused4, i8* %unused5) {\n" ++ runCodegen applyClosureHelperFunc ++ "\n}"
