target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"

; generic boxed object
%Object = type {
    i64 ; object header
  , [0 x i8*] ; payload
}

%ObjPtr = type %Object addrspace(1)*
%ObjPtrPtr = type %Object addrspace(1)* addrspace(1)*
%RawPtr = type i8*
%RuntimePtr = type i8*
%FuncPtr = type i8*

%i8p1 = type i8 addrspace(1)*
%i32p1 = type i32 addrspace(1)*
%i64p1 = type i64 addrspace(1)*

%Idris_TSO.struct = type {
    i8* ; nurseryStart
  , i8* ; nurseryNext
  , i8* ; nurseryEnd
  , i32 ; errno
  , i8* ; stack_bottom
  , i8* ; stack_top
  , i8* ; stack_size
  , i64 ; heap_alloc
}

%TSOPtr = type %Idris_TSO.struct*

%Word = type i64

%VoidReturn = type {%RuntimePtr, %RuntimePtr, %RuntimePtr}
%Return1 = type {%RuntimePtr, %RuntimePtr, %ObjPtr}

%FuncPtrClosureEntry = type %Return1 (%RuntimePtr, %TSOPtr, %RuntimePtr, %ObjPtr, %ObjPtr)*

declare ccc void @idris_rts_gc(%TSOPtr, i8*)
declare ccc void @idris_rts_crash(i64) noreturn
declare ccc void @idris_rts_crash_msg(%ObjPtr) noreturn
declare ccc void @idris_rts_crash_typecheck(%ObjPtr, i64) noreturn
declare ccc i32 @dump_obj(%ObjPtr)

declare ccc void @idris_mkcon_ok(%ObjPtr)
declare ccc void @idris_mkcon_arg_ok(%ObjPtr, i64)

declare ccc i64 @idris_rts_int_to_str(%i8p1 noalias nocapture nofree nonnull, i64) readonly argmemonly
declare ccc i64 @idris_rts_double_to_str(%i8p1 noalias nocapture nofree writeonly, i64, double) argmemonly
declare ccc double @idris_rts_str_to_double(%ObjPtr noalias nocapture nofree nonnull) readonly argmemonly
declare ccc i64 @idris_rts_str_to_int(%ObjPtr noalias nocapture nofree nonnull) readonly argmemonly

declare ccc void @rapid_strreverse(%i8p1 noalias nocapture nofree nonnull writeonly, %i8p1 noalias nocapture nofree nonnull readonly, i64) argmemonly

declare ccc i64 @idris_rts_write_buffer_data(%TSOPtr, %ObjPtr, %ObjPtr, i64, i64, %ObjPtr)
declare ccc i64 @idris_rts_read_buffer_data(%TSOPtr, %ObjPtr, %ObjPtr, i64, i64, %ObjPtr)
declare ccc %ObjPtr @rapid_system_file_open(%TSOPtr, %ObjPtr, %ObjPtr, i64, %ObjPtr)
declare ccc void @rapid_system_file_close(%TSOPtr, %ObjPtr, %ObjPtr)
declare ccc %Word @rapid_system_file_eof(%TSOPtr, %ObjPtr, %ObjPtr)
declare ccc i64 @rapid_system_file_size(%TSOPtr, %ObjPtr, %ObjPtr)
declare ccc %Word @rapid_system_file_write_string(%TSOPtr, %ObjPtr, %ObjPtr, %ObjPtr)
declare ccc %ObjPtr @rapid_system_file_read_line(%TSOPtr, %ObjPtr, %ObjPtr)
declare ccc %ObjPtr @rapid_system_getargs(%TSOPtr, %ObjPtr)
declare ccc %ObjPtr @rapid_fast_pack(%TSOPtr, %ObjPtr)
declare ccc %ObjPtr @rapid_fast_append(%TSOPtr, %ObjPtr)
declare ccc void @rapid_putstr(%TSOPtr, %ObjPtr, %ObjPtr)

declare void @llvm.memcpy.p0i8.p0i8.i32(i8* nocapture, i8* nocapture, i32, i1) nounwind
declare void @llvm.memcpy.p1i8.p0i8.i32(%i8p1 nocapture, i8* nocapture, i32, i1) nounwind
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture, i8* nocapture, i64, i1) nounwind
declare void @llvm.memcpy.p1i8.p1i8.i32(%i8p1 nocapture, %i8p1 nocapture, i32, i1) nounwind
declare void @llvm.memcpy.p1i8.p1i8.i64(%i8p1 nocapture, %i8p1 nocapture, i64, i1) nounwind
declare void @llvm.dbg.addr(metadata, metadata, metadata)

declare i8* @llvm.frameaddress(i32)
declare i8* @llvm.addressofreturnaddress()

declare ccc i1 @llvm.expect.i1(i1, i1)

declare ccc noalias i8* @GC_malloc(i64)
declare ccc noalias %ObjPtr @log_GC_malloc(i64)

define private ccc %Return1 @rapid_gc_enter(%TSOPtr %BaseArg, i64 %size.aligned) noinline gc "statepoint-example" {
  %frame = call i8* @llvm.addressofreturnaddress()

  %heapAllocPtr = getelementptr inbounds %Idris_TSO.struct, %Idris_TSO.struct* %BaseArg, i32 0, i32 7
  store i64 %size.aligned, i64* %heapAllocPtr

  call ccc void @idris_rts_gc(%TSOPtr %BaseArg, i8* %frame)

  ; get updated heap pointer from BaseTSO
  %heapPtr = getelementptr inbounds %Idris_TSO.struct, %Idris_TSO.struct* %BaseArg, i32 0, i32 1
  %heap = load %RuntimePtr, %RuntimePtr* %heapPtr

  %heapNext = getelementptr i8, %RuntimePtr %heap, i64 %size.aligned

  %newObject = addrspacecast %RuntimePtr %heap to %ObjPtr

  %packed1 = insertvalue %Return1 undef, %RuntimePtr %heapNext, 0
  %packed2 = insertvalue %Return1 %packed1, %RuntimePtr undef, 1
  %packed3 = insertvalue %Return1 %packed2, %ObjPtr %newObject, 2
  ret %Return1 %packed3
}

define private fastcc i1 @mem_eq(%i8p1 noalias nocapture nofree nonnull %v1, %i8p1 noalias nocapture nofree nonnull %v2, i64 %size) argmemonly readonly nounwind {
entry:
  br label %loop
loop:
  %i = phi i64 [%iPlus, %loopend], [0, %entry]

  %p1 = getelementptr inbounds i8, %i8p1 %v1, i64 %i
  %p2 = getelementptr inbounds i8, %i8p1 %v2, i64 %i
  %b1 = load i8, %i8p1 %p1
  %b2 = load i8, %i8p1 %p2
  %beq = icmp eq i8 %b1, %b2

  br i1 %beq, label %loopend, label %finished

loopend:
  %iPlus = add nuw nsw i64 %i, 1
  %continue = icmp ult i64 %iPlus, %size
  br i1 %continue, label %loop, label %finished
finished:
  ret i1 %beq
}

define private fastcc i32 @rapid.memcmp(%i8p1 noalias nocapture nofree nonnull %v1, %i8p1 noalias nocapture nofree nonnull %v2, i64 %size) argmemonly readonly nounwind {
;define external fastcc i32 @rapid.memcmp(%i8p1 %v1, %i8p1 %v2, i64 %size) noinline optsize nounwind {
entry:
  br label %loop
loop:
  %i = phi i64 [%iPlus, %loopend], [0, %entry]

  %p1 = getelementptr inbounds i8, %i8p1 %v1, i64 %i
  %p2 = getelementptr inbounds i8, %i8p1 %v2, i64 %i
  %b1 = load i8, %i8p1 %p1
  %b2 = load i8, %i8p1 %p2
  %beq = icmp eq i8 %b1, %b2

  br i1 %beq, label %loopend, label %finished_neq

loopend:
  %iPlus = add nuw nsw i64 %i, 1
  %continue = icmp ult i64 %iPlus, %size
  br i1 %continue, label %loop, label %finished_eq
finished_neq:
  %bcmp = icmp ult i8 %b1, %b2
  %result = select i1 %bcmp, i32 -1, i32 1
  ret i32 %result

finished_eq:
  ret i32 0
}

define external fastcc %Return1 @rapid_allocate (%RuntimePtr %HpPtrArg, %TSOPtr %BaseArg, %RuntimePtr %HpLimPtrArg, i64 %size) allocsize(3) alwaysinline optsize nounwind {
  ;%addr = call ccc %ObjPtr @log_GC_malloc(i64 %size)
  %addr.raw = call ccc noalias i8* @GC_malloc(i64 %size)
  %addr = addrspacecast i8* %addr.raw to %ObjPtr

  %packed1 = insertvalue %Return1 undef, %RuntimePtr %HpPtrArg, 0
  %packed2 = insertvalue %Return1 %packed1, %RuntimePtr %HpLimPtrArg, 1
  %packed3 = insertvalue %Return1 %packed2, %ObjPtr %addr, 2
  ret %Return1 %packed3
}

define external ccc noalias i8* @rapid_C_allocate (%TSOPtr %Base, i64 %size) allocsize(1) {
  %addr = call ccc noalias i8* @GC_malloc(i64 %size)

  ret i8* %addr
}

define external fastcc %Return1 @rapid_allocate_mutable (%RuntimePtr %HpPtrArg, %TSOPtr %BaseArg, %RuntimePtr %HpLimPtrArg, i64 %size) alwaysinline optsize nounwind {
  %addr.raw = call ccc noalias i8* @GC_malloc(i64 %size)
  %addr = addrspacecast i8* %addr.raw to %ObjPtr

  %packed1 = insertvalue %Return1 undef, %RuntimePtr %HpPtrArg, 0
  %packed2 = insertvalue %Return1 %packed1, %RuntimePtr %HpLimPtrArg, 1
  %packed3 = insertvalue %Return1 %packed2, %ObjPtr %addr, 2
  ret %Return1 %packed3
}

define external fastcc %Return1 @rapid_allocate_fast (%RuntimePtr %HpPtrArg, %TSOPtr %BaseArg, %RuntimePtr %HpLimPtrArg, i64 %size) alwaysinline optsize nounwind gc "statepoint-example" {
  %Hp = ptrtoint %RuntimePtr %HpPtrArg to i64

  %nurseryEndPtr = getelementptr inbounds %Idris_TSO.struct, %Idris_TSO.struct *%BaseArg, i32 0, i32 2
  %nurseryEnd = load %RuntimePtr, %RuntimePtr* %nurseryEndPtr

  %HpLim = ptrtoint %RuntimePtr %nurseryEnd to i64

  %size.plus7 = add i64 %size, 7
  %size.aligned = and i64 -8, %size.plus7

  %HpNew = add i64 %Hp, %size.aligned
  %HpNewPtr = inttoptr i64 %HpNew to %RuntimePtr

  %overflow.in = icmp ugt i64 %HpNew, %HpLim
  %overflow = call ccc i1 @llvm.expect.i1(i1 %overflow.in, i1 0)

  br i1 %overflow, label %gc_enter, label %continue
continue:
  %retptr = inttoptr i64 %Hp to i64*
  ;TODO: do it
  %packed1 = insertvalue %Return1 undef, %RuntimePtr %HpNewPtr, 0
  %packed2 = insertvalue %Return1 %packed1, %RuntimePtr %HpLimPtrArg, 1
  %newAddrIn1 = addrspacecast %RuntimePtr %HpPtrArg to i8 addrspace(1)*
  %newAddr = bitcast i8 addrspace(1)* %newAddrIn1 to %ObjPtr
  %packed3 = insertvalue %Return1 %packed2, %ObjPtr %newAddr, 2
  ret %Return1 %packed3
gc_enter:
  %gcresult = call ccc %Return1 @rapid_gc_enter(%TSOPtr %BaseArg, i64 %size.aligned)
  ret %Return1 %gcresult
}

define private fastcc %Return1 @_extprim_Data.IORef.prim__newIORef(%RuntimePtr %HpArg, %TSOPtr %BaseArg, %RuntimePtr %HpLimArg, %ObjPtr %discard0, %ObjPtr %val, %ObjPtr %world) {
  %allocated.ret = call fastcc %Return1 @rapid_allocate_mutable (%RuntimePtr %HpArg, %TSOPtr %BaseArg, %RuntimePtr %HpLimArg, i64 16)
  %hpnew = extractvalue %Return1 %allocated.ret, 0
  %hplimnew = extractvalue %Return1 %allocated.ret, 1
  %newobj = extractvalue %Return1 %allocated.ret, 2

  %objptr = bitcast %ObjPtr %newobj to %i64p1
  %hdr.ptr = getelementptr inbounds i64, %i64p1 %objptr, i64 0
  ; putObjectHeader 0x05 `shl` 32
  store i64 21474836480, %i64p1 %hdr.ptr

  %ref.ptr = getelementptr inbounds i64, %i64p1 %objptr, i64 1
  %ref.objptr = bitcast %i64p1 %ref.ptr to %ObjPtrPtr
  store %ObjPtr %val, %ObjPtrPtr %ref.objptr

  %packed1 = insertvalue %Return1 undef, %RuntimePtr %hpnew, 0
  %packed2 = insertvalue %Return1 %packed1, %RuntimePtr %hplimnew, 1
  %packed3 = insertvalue %Return1 %packed2, %ObjPtr %newobj, 2
  ret %Return1 %packed3
}

define private fastcc %Return1 @_extprim_Data.IORef.prim__readIORef(%RuntimePtr %HpArg, %TSOPtr %BaseArg, %RuntimePtr %HpLimArg, %ObjPtr %discard0, %ObjPtr %ref, %ObjPtr %world) alwaysinline {
  %objptr = bitcast %ObjPtr %ref to %i64p1
  %payload.ptr = getelementptr inbounds i64, %i64p1 %objptr, i64 1
  %payload.objptr = bitcast %i64p1 %payload.ptr to %ObjPtrPtr
  %payload.obj = load %ObjPtr, %ObjPtrPtr %payload.objptr

  %packed1 = insertvalue %Return1 undef, %RuntimePtr %HpArg, 0
  %packed2 = insertvalue %Return1 %packed1, %RuntimePtr %HpLimArg, 1
  %packed3 = insertvalue %Return1 %packed2, %ObjPtr %payload.obj, 2
  ret %Return1 %packed3
}

define private fastcc %Return1 @_extprim_Data.IORef.prim__writeIORef(%RuntimePtr %HpArg, %TSOPtr %BaseArg, %RuntimePtr %HpLimArg, %ObjPtr %discard0, %ObjPtr %ref, %ObjPtr %val, %ObjPtr %world) alwaysinline {
  %objptr = bitcast %ObjPtr %ref to %i64p1
  %payload.ptr = getelementptr inbounds i64, %i64p1 %objptr, i64 1
  %payload.objptr = bitcast %i64p1 %payload.ptr to %ObjPtrPtr
  ; future write barrier required?
  store %ObjPtr %val, %ObjPtrPtr %payload.objptr

  %packed1 = insertvalue %Return1 undef, %RuntimePtr %HpArg, 0
  %packed2 = insertvalue %Return1 %packed1, %RuntimePtr %HpLimArg, 1
  %packed3 = insertvalue %Return1 %packed2, %ObjPtr null, 2
  ret %Return1 %packed3
}

define private fastcc i64 @idris_enter_stackbridge(%TSOPtr %BaseTSO, i8* %heapStart, i8* %heapEnd) {
  call fastcc %Return1 @$7b__mainExpression$3a0$7d(%RuntimePtr %heapStart, %TSOPtr %BaseTSO, %RuntimePtr %heapEnd)
  ;call hhvmcc %Return1 @Main$2e$7bmain$3a0$7d(%RuntimePtr %heapStart, %RuntimePtr %BaseTSO, %RuntimePtr %heapEnd, %ObjPtr undef)
  ret i64 0
}

define external ccc i64 @idris_enter(%Idris_TSO.struct* %BaseTSO) {
  %heapStartPtr = getelementptr inbounds %Idris_TSO.struct, %Idris_TSO.struct* %BaseTSO, i32 0, i32 1
  %heapStart = load %RuntimePtr, %RuntimePtr* %heapStartPtr

  %heapEndPtr = getelementptr inbounds %Idris_TSO.struct, %Idris_TSO.struct* %BaseTSO, i32 0, i32 2
  %heapEnd = load %RuntimePtr, %RuntimePtr* %heapEndPtr

  call fastcc i64 @idris_enter_stackbridge(%TSOPtr %BaseTSO, %RuntimePtr %heapStart, %RuntimePtr %heapEnd)
  ret i64 0
}
