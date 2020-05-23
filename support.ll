target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15"

declare ccc void @exit(i64) noreturn

@g_Hp = weak global i64 undef
@g_HpLim = weak global i64 undef

%ObjPtr = type i8*
%RuntimePtr = type i8*

%VoidReturn = type {%RuntimePtr, %RuntimePtr, %RuntimePtr}
%Return1 = type {%RuntimePtr, %RuntimePtr, %RuntimePtr, %ObjPtr}

declare void @llvm.memcpy.p0i8.p0i8.i32(i8* nocapture, i8* nocapture, i32, i1) nounwind
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture, i8* nocapture, i64, i1) nounwind

define private hhvmcc %Return1 @rapid_gc_enter() noinline {
  call ccc void @exit(i64 1) noreturn
  ret %Return1 undef
}

declare ccc i1 @llvm.expect.i1(i1, i1)

define private hhvmcc %Return1 @rapid_allocate (%RuntimePtr %HpPtrArg, i64 %size, %RuntimePtr %BaseArg, i64 %unused2, %RuntimePtr %HpLimPtrArg) alwaysinline optsize nounwind {
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
  %packed0 = insertvalue %Return1 undef, %RuntimePtr %HpNewPtr, 0
  %packed1 = insertvalue %Return1 %packed0, %RuntimePtr %BaseArg, 1
  %packed2 = insertvalue %Return1 %packed1, %RuntimePtr %HpLimPtrArg, 2
  %packed3 = insertvalue %Return1 %packed2, %RuntimePtr %HpPtrArg, 3
  ret %Return1 %packed3
gc_enter:
  %gcresult = call hhvmcc %Return1 @rapid_gc_enter() noreturn
  ret %Return1 %gcresult
}

declare ccc i64 @write(i32, i8*, i64)
define private hhvmcc %Return1 @_extprim_PrimIO_2e_prim_5f__5f_putStr(%RuntimePtr %HpArg, %ObjPtr %t0, %RuntimePtr %BaseArg, %ObjPtr %t1, %RuntimePtr %HpLimArg) alignstack(16) {
  %payloadPtr = getelementptr i8, %ObjPtr %t0, i64 8

  call ccc i64 @write(i32 0, i8* %payloadPtr, i64 5)
  %packed0 = insertvalue %Return1 undef, %RuntimePtr %HpArg, 0
  %packed1 = insertvalue %Return1 %packed0, %RuntimePtr %BaseArg, 1
  %packed2 = insertvalue %Return1 %packed1, %RuntimePtr %HpLimArg, 2
  ;%packed3 = insertvalue %Return1 %packed2, %RuntimePtr %HpPtrArg, 3
  ret %Return1 %packed2
}

declare ccc i8* @malloc(i64)

define external ccc i64 @main () {
  %heapStart = call ccc %RuntimePtr @malloc(i64 1024)
  %heapStartInt = ptrtoint %RuntimePtr %heapStart to i64
  %heapEndInt = add i64 %heapStartInt, 1024
  %heapEnd = inttoptr i64 %heapEndInt to %RuntimePtr
  ;call hhvmcc %Return1 @_7b__5f__5f_mainExpression_3a_0_7d_(%RuntimePtr %heapStart, i64 undef, %RuntimePtr undef, i64 undef, %RuntimePtr %heapEnd)
  call hhvmcc %Return1 @Main_2e__7b_main_3a_0_7d_(%RuntimePtr %heapStart, %ObjPtr undef, %RuntimePtr undef, %ObjPtr undef, %RuntimePtr %heapEnd)
  ret i64 0
}
