declare ccc void @exit(i64) noreturn

@g_Hp = weak global i64 undef
@g_HpLim = weak global i64 undef

%ObjPtr = type i8*
%RuntimePtr = type i8*

%VoidReturn = type {%RuntimePtr, %RuntimePtr, %RuntimePtr}
%Return1 = type {%RuntimePtr, %RuntimePtr, %RuntimePtr, %ObjPtr}

define private hhvmcc %Return1 @rapid_allocate (%RuntimePtr %HpPtrArg, i64 %size, %RuntimePtr %BaseArg, i64 %unused2, %RuntimePtr %HpLimPtrArg) alwaysinline optsize nounwind {
  ;%Hp = load i64, i64* %HpPtrArg
  ;%HpLim = load i64, i64* %HpLimPtrArg
  %Hp = ptrtoint %RuntimePtr %HpPtrArg to i64
  %HpLim = ptrtoint %RuntimePtr %HpLimPtrArg to i64

  %HpNew = add i64 %Hp, %size
  %HpNewPtr = inttoptr i64 %HpNew to %RuntimePtr

  %overflow = icmp ugt i64 %HpNew, %HpLim
  br i1 %overflow, label %gc_enter, label %continue
continue:
  ;store i64 %HpNew, i64* %HpPtrArg

  %retptr = inttoptr i64 %Hp to i64*
  ;ret i64* %retptr
  ;TODO: do it
  %packed0 = insertvalue %Return1 undef, %RuntimePtr %HpNewPtr, 0
  %packed1 = insertvalue %Return1 %packed0, %RuntimePtr %BaseArg, 1
  %packed2 = insertvalue %Return1 %packed1, %RuntimePtr %HpLimPtrArg, 2
  %packed3 = insertvalue %Return1 %packed2, %RuntimePtr %HpPtrArg, 3
  ret %Return1 %packed3
gc_enter:
  call ccc void @exit(i64 1) noreturn
  ret %Return1 undef
}
