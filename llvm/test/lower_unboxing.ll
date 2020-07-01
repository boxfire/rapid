; RUN: opt -load ../librapid.so -hello -S < %s | FileCheck %s
%Object = type {
    i64 ; object header
  , [0 x i8*] ; payload
}

%ObjPtr = type %Object addrspace(1)*

declare fastcc i64 @llvm.rapid.unboxint(%ObjPtr noalias nocapture nofree) "gc-leaf-function" readnone nounwind

define fastcc i64 @main(%ObjPtr %p) {
; CHECK: @main
; CHECK: addrspacecast
; CHECK-NEXT: ptrtoint
; CHECK-NEXT: ashr
; CHECK-NOT: @llvm.rapid.unboxint
  %int = call fastcc i64 @llvm.rapid.unboxint(%ObjPtr %p)
  ret i64 %int
}
