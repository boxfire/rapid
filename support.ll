target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15"

%ObjPtr = type i8*
%RuntimePtr = type i8*
%FuncPtr = type i8*

%VoidReturn = type {%RuntimePtr, %RuntimePtr, %RuntimePtr}
%Return1 = type {%RuntimePtr, %RuntimePtr, %ObjPtr}

declare ccc void @idris_rts_gc(i8*)
declare ccc void @idris_rts_crash(i64)
declare ccc void @idris_rts_crash_msg(%ObjPtr)
declare ccc i64 @idris_rts_int_to_str(i8*, i64)
declare ccc i64 @idris_rts_double_to_str(i8*, i64, double)
declare ccc double @idris_rts_str_to_double(%ObjPtr)
declare ccc i64 @idris_rts_str_to_int(%ObjPtr)

declare void @llvm.memcpy.p0i8.p0i8.i32(i8* nocapture, i8* nocapture, i32, i1) nounwind
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture, i8* nocapture, i64, i1) nounwind

declare i8* @llvm.frameaddress(i32)

define private ccc %Return1 @rapid_gc_enter() noinline {
  %frame = call i8* @llvm.frameaddress(i32 0)
  call ccc void @idris_rts_gc(i8* %frame)
  ret %Return1 undef
}

declare ccc i1 @llvm.expect.i1(i1, i1)

declare ccc %ObjPtr @GC_malloc(i64)

define private fastcc i1 @mem_eq(i8* %v1, i8* %v2, i64 %size) alwaysinline optsize nounwind {
entry:
  br label %loop
loop:
  %i = phi i64 [%iPlus, %loopend], [0, %entry]

  %p1 = getelementptr inbounds i8, i8* %v1, i64 %i
  %p2 = getelementptr inbounds i8, i8* %v2, i64 %i
  %b1 = load i8, i8* %p1
  %b2 = load i8, i8* %p2
  %beq = icmp eq i8 %b1, %b2

  br i1 %beq, label %loopend, label %finished

loopend:
  %continue = icmp ule i64 %i, %size
  %iPlus = add i64 %i, 1
  br i1 %continue, label %loop, label %finished
finished:
  ret i1 %beq
}

define external fastcc %Return1 @rapid_allocate (%RuntimePtr %HpPtrArg, %RuntimePtr %BaseArg, %RuntimePtr %HpLimPtrArg, i64 %size) alwaysinline optsize nounwind {
  %addr = call ccc %ObjPtr @GC_malloc(i64 %size)
  ;%addr = call ccc %ObjPtr @malloc(i64 %size)

  %packed1 = insertvalue %Return1 undef, %RuntimePtr %HpPtrArg, 0
  %packed2 = insertvalue %Return1 %packed1, %RuntimePtr %HpLimPtrArg, 1
  %packed3 = insertvalue %Return1 %packed2, %RuntimePtr %addr, 2
  ret %Return1 %packed3
}

define external fastcc %Return1 @rapid_allocate_mutable (%RuntimePtr %HpPtrArg, %RuntimePtr %BaseArg, %RuntimePtr %HpLimPtrArg, i64 %size) alwaysinline optsize nounwind {
  %addr = call ccc %ObjPtr @GC_malloc(i64 %size)

  %packed1 = insertvalue %Return1 undef, %RuntimePtr %HpPtrArg, 0
  %packed2 = insertvalue %Return1 %packed1, %RuntimePtr %HpLimPtrArg, 1
  %packed3 = insertvalue %Return1 %packed2, %RuntimePtr %addr, 2
  ret %Return1 %packed3
}

define external fastcc %Return1 @rapid_allocate_fast (%RuntimePtr %HpPtrArg, %RuntimePtr %BaseArg, %RuntimePtr %HpLimPtrArg, i64 %size) alwaysinline optsize nounwind {
  %Hp = ptrtoint %RuntimePtr %HpPtrArg to i64
  %HpLim = ptrtoint %RuntimePtr %HpLimPtrArg to i64

  %HpNew = add i64 %Hp, %size
  %HpNewPtr = inttoptr i64 %HpNew to %RuntimePtr

  %overflow.in = icmp ugt i64 %HpNew, %HpLim
  %overflow = call ccc i1 @llvm.expect.i1(i1 %overflow.in, i1 0)
  br i1 %overflow, label %gc_enter, label %continue
continue:
  %retptr = inttoptr i64 %Hp to i64*
  ;TODO: do it
  %packed1 = insertvalue %Return1 undef, %RuntimePtr %HpNewPtr, 0
  %packed2 = insertvalue %Return1 %packed1, %RuntimePtr %HpLimPtrArg, 1
  %packed3 = insertvalue %Return1 %packed2, %RuntimePtr %HpPtrArg, 2
  ret %Return1 %packed3
gc_enter:
  %gcresult = call ccc %Return1 @rapid_gc_enter() noreturn
  ret %Return1 %gcresult
}

declare ccc i64 @write(i32, i8*, i64)

define private fastcc %Return1 @PrimIO.prim__putStr(%RuntimePtr %HpArg, %RuntimePtr %BaseArg, %RuntimePtr %HpLimArg, %ObjPtr %t0, %ObjPtr %unused0) {
  %payloadPtr = getelementptr i8, %ObjPtr %t0, i64 8
  %sizePtr = bitcast %ObjPtr %t0 to i32*
  %size32 = load i32, i32* %sizePtr
  %size64 = zext i32 %size32 to i64

  call ccc i64 @write(i32 1, i8* %payloadPtr, i64 %size64)
  %packed1 = insertvalue %Return1 undef, %RuntimePtr %HpArg, 0
  %packed2 = insertvalue %Return1 %packed1, %RuntimePtr %HpLimArg, 1
  ;%packed3 = insertvalue %Return1 %packed2, %RuntimePtr %HpPtrArg, 3
  ret %Return1 %packed2
}

define private fastcc %Return1 @PrimIO.prim__getString(%RuntimePtr %HpArg, %RuntimePtr %BaseArg, %RuntimePtr %HpLimArg, %ObjPtr %unused0) {
; TODO: implement
  %nullptr = inttoptr i64 0 to %ObjPtr
  %packed1 = insertvalue %Return1 undef, %RuntimePtr %HpArg, 0
  %packed2 = insertvalue %Return1 %packed1, %RuntimePtr %HpLimArg, 1
  %packed3 = insertvalue %Return1 %packed2, %ObjPtr %nullptr, 2
  ret %Return1 %packed3
}

define private fastcc %Return1 @PrimIO.prim__nullAnyPtr(%RuntimePtr %HpArg, %RuntimePtr %BaseArg, %RuntimePtr %HpLimArg, %ObjPtr %unused0) {
; TODO: implement
  %nullptr = inttoptr i64 0 to %ObjPtr
  %packed1 = insertvalue %Return1 undef, %RuntimePtr %HpArg, 0
  %packed2 = insertvalue %Return1 %packed1, %RuntimePtr %HpLimArg, 1
  %packed3 = insertvalue %Return1 %packed2, %ObjPtr %nullptr, 2
  ret %Return1 %packed3
}

define private fastcc %Return1 @_extprim_Data.IORef.prim__newIORef(%RuntimePtr %HpArg, %RuntimePtr %BaseArg, %RuntimePtr %HpLimArg, %ObjPtr %discard0, %ObjPtr %val, %ObjPtr %world) {
  %allocated.ret = call fastcc %Return1 @rapid_allocate_mutable (%RuntimePtr %HpArg, %RuntimePtr %BaseArg, %RuntimePtr %HpLimArg, i64 16)
  %hpnew = extractvalue %Return1 %allocated.ret, 0
  %hplimnew = extractvalue %Return1 %allocated.ret, 1
  %newobj = extractvalue %Return1 %allocated.ret, 2

  %objptr = bitcast %ObjPtr %newobj to i64*
  %hdr.ptr = getelementptr inbounds i64, i64* %objptr, i64 0
  ; putObjectHeader 0x05 `shl` 32
  store i64 21474836480, i64* %hdr.ptr

  %ref.ptr = getelementptr inbounds i64, i64* %objptr, i64 1
  %ref.objptr = bitcast i64* %ref.ptr to %ObjPtr*
  store %ObjPtr %val, %ObjPtr* %ref.objptr

  %packed1 = insertvalue %Return1 undef, %RuntimePtr %hpnew, 0
  %packed2 = insertvalue %Return1 %packed1, %RuntimePtr %hplimnew, 1
  %packed3 = insertvalue %Return1 %packed2, %ObjPtr %newobj, 2
  ret %Return1 %packed3
}

define private fastcc %Return1 @_extprim_Data.IORef.prim__readIORef(%RuntimePtr %HpArg, %RuntimePtr %BaseArg, %RuntimePtr %HpLimArg, %ObjPtr %discard0, %ObjPtr %ref, %ObjPtr %world) alwaysinline {
  %objptr = bitcast %ObjPtr %ref to i64*
  %payload.ptr = getelementptr inbounds i64, i64* %objptr, i64 1
  %payload.objptr = bitcast i64* %payload.ptr to %ObjPtr*
  %payload.obj = load %ObjPtr, %ObjPtr* %payload.objptr

  %packed1 = insertvalue %Return1 undef, %RuntimePtr %HpArg, 0
  %packed2 = insertvalue %Return1 %packed1, %RuntimePtr %HpLimArg, 1
  %packed3 = insertvalue %Return1 %packed2, %ObjPtr %payload.obj, 2
  ret %Return1 %packed3
}

define private fastcc %Return1 @_extprim_Data.IORef.prim__writeIORef(%RuntimePtr %HpArg, %RuntimePtr %BaseArg, %RuntimePtr %HpLimArg, %ObjPtr %discard0, %ObjPtr %ref, %ObjPtr %val, %ObjPtr %world) alwaysinline {
  %objptr = bitcast %ObjPtr %ref to i64*
  %payload.ptr = getelementptr inbounds i64, i64* %objptr, i64 1
  %payload.objptr = bitcast i64* %payload.ptr to %ObjPtr*
  ; future write barrier required?
  store %ObjPtr %val, %ObjPtr* %payload.objptr

  %nullptr = inttoptr i64 0 to %ObjPtr

  %packed1 = insertvalue %Return1 undef, %RuntimePtr %HpArg, 0
  %packed2 = insertvalue %Return1 %packed1, %RuntimePtr %HpLimArg, 1
  %packed3 = insertvalue %Return1 %packed2, %ObjPtr %nullptr, 2
  ret %Return1 %packed3
}

declare ccc i8* @malloc(i64)

define private fastcc i64 @idris_enter_stackbridge(i8* %BaseTSO, i8* %heapStart, i8* %heapEnd) {
  call fastcc %Return1 @$7b__mainExpression$3a0$7d(%RuntimePtr %heapStart, %RuntimePtr %BaseTSO, %RuntimePtr %heapEnd)
  ;call hhvmcc %Return1 @Main$2e$7bmain$3a0$7d(%RuntimePtr %heapStart, %RuntimePtr %BaseTSO, %RuntimePtr %heapEnd, %ObjPtr undef)
  ret i64 0
}

%Idris_TSO.struct = type {
  i8*, ; nurseryStart
  i8*, ; nurseryNext
  i8*  ; nurseryEnd
}

define external ccc i64 @idris_enter(%Idris_TSO.struct* %BaseTSO) {
  %heapStartPtr = getelementptr inbounds %Idris_TSO.struct, %Idris_TSO.struct* %BaseTSO, i32 0, i32 1
  %heapStart = load %RuntimePtr, %RuntimePtr* %heapStartPtr

  %heapEndPtr = getelementptr inbounds %Idris_TSO.struct, %Idris_TSO.struct* %BaseTSO, i32 0, i32 2
  %heapEnd = load %RuntimePtr, %RuntimePtr* %heapEndPtr

  %BaseTSO.raw = bitcast %Idris_TSO.struct* %BaseTSO to %RuntimePtr
  call fastcc i64 @idris_enter_stackbridge(%RuntimePtr %BaseTSO.raw, %RuntimePtr %heapStart, %RuntimePtr %heapEnd)
  ret i64 0
}
