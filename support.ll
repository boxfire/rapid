target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15"

declare ccc void @idris_rts_gc(i8*)
declare ccc void @idris_rts_crash(i64)

@g_Hp = weak global i64 undef
@g_HpLim = weak global i64 undef

%ObjPtr = type i8*
%RuntimePtr = type i8*
%FuncPtr = type i8*

%VoidReturn = type {%RuntimePtr, %RuntimePtr, %RuntimePtr}
%Return1 = type {%RuntimePtr, %RuntimePtr, %ObjPtr}

;%FuncPtrArgs0 = type %Return1 (%RuntimePtr, %RuntimePtr, %RuntimePtr)*
%FuncPtrArgs1 = type %Return1 (%RuntimePtr, %RuntimePtr, %RuntimePtr, %ObjPtr)*
%FuncPtrArgs2 = type %Return1 (%RuntimePtr, %RuntimePtr, %RuntimePtr, %ObjPtr, %ObjPtr)*
%FuncPtrArgs3 = type %Return1 (%RuntimePtr, %RuntimePtr, %RuntimePtr, %ObjPtr, %ObjPtr, %ObjPtr)*
%FuncPtrArgs4 = type %Return1 (%RuntimePtr, %RuntimePtr, %RuntimePtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr)*
%FuncPtrArgs5 = type %Return1 (%RuntimePtr, %RuntimePtr, %RuntimePtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr)*
%FuncPtrArgs6 = type %Return1 (%RuntimePtr, %RuntimePtr, %RuntimePtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr)*
%FuncPtrArgs7 = type %Return1 (%RuntimePtr, %RuntimePtr, %RuntimePtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr)*
%FuncPtrArgs8 = type %Return1 (%RuntimePtr, %RuntimePtr, %RuntimePtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr)*
%FuncPtrArgs9 = type %Return1 (%RuntimePtr, %RuntimePtr, %RuntimePtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr)*
%FuncPtrArgs10 = type %Return1 (%RuntimePtr, %RuntimePtr, %RuntimePtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr)*
%FuncPtrArgs11 = type %Return1 (%RuntimePtr, %RuntimePtr, %RuntimePtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr)*
%FuncPtrArgs12 = type %Return1 (%RuntimePtr, %RuntimePtr, %RuntimePtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr, %ObjPtr)*

declare void @llvm.memcpy.p0i8.p0i8.i32(i8* nocapture, i8* nocapture, i32, i1) nounwind
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture, i8* nocapture, i64, i1) nounwind

declare i8* @llvm.frameaddress(i32)

define private hhvm_ccc %Return1 @rapid_gc_enter() noinline {
  %frame = call i8* @llvm.frameaddress(i32 0)
  call ccc void @idris_rts_gc(i8* %frame)
  ret %Return1 undef
}

declare ccc i1 @llvm.expect.i1(i1, i1)

define external hhvmcc %Return1 @rapid_allocate (%RuntimePtr %HpPtrArg, %RuntimePtr %BaseArg, %RuntimePtr %HpLimPtrArg, i64 %size) alwaysinline optsize nounwind {
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
  %gcresult = call hhvm_ccc %Return1 @rapid_gc_enter() noreturn
  ret %Return1 %gcresult
}

declare ccc i64 @write(i32, i8*, i64)
define private hhvmcc %Return1 @_extprim_PrimIO_2e_prim_5f__5f_putStr(%RuntimePtr %HpArg, %RuntimePtr %BaseArg, %RuntimePtr %HpLimArg, %ObjPtr %t0, %ObjPtr %unused0) {
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

declare ccc i8* @malloc(i64)

define private hhvmcc i64 @idris_enter_stackbridge(i8* %BaseTSO, i8* %heapStart, i8* %heapEnd) {
  call hhvmcc %Return1 @_7b__5f__5f_mainExpression_3a_0_7d_(%RuntimePtr %heapStart, %RuntimePtr %BaseTSO, %RuntimePtr %heapEnd)
  ;call hhvmcc %Return1 @Main_2e__7b_main_3a_0_7d_(%RuntimePtr %heapStart, %RuntimePtr %BaseTSO, %RuntimePtr %heapEnd, %ObjPtr undef)
  ret i64 0
}

define external ccc i64 @idris_enter(i8* %BaseTSO, i8* %heapStart, i8* %heapEnd) {
  call hhvmcc i64 @idris_enter_stackbridge(%RuntimePtr %BaseTSO, %RuntimePtr %heapStart, %RuntimePtr %heapEnd)
  ret i64 0
}
