# HHVM Calling Conv (x86_64)

Call:
    CCIfType<[i64], CCAssignToReg<[
    RBX, R12, RBP, R15, RDI, RSI, RDX, RCX, R8, R9, RAX, R10, R11, R13, R14
    ]>>

Return:
    CCIfType<[i64], CCAssignToReg<[
    RBX, RBP, RDI, RSI, RDX, RCX, R8, R9, RAX, R10, R11, R13, R14, R15
    ]>>

OLD:
    RBX -> Heap
    RBP -> Base
    RDI -> HeapLim

NEW:
    RBX -> Heap
    R12 -> Base (callee saved)
    RBP -> HeapLim
    R15, RDI, RSI... -> arg0, arg1, arg2...


    RBX, R12, RBP, R15, RDI, RSI, RDX, RCX, R8, R9, RAX, R10, R11, R13, R14
    |         |         |    |    |    |    |   |   |    |    |    |    |
    V         V         V    V    V    V    V   V   V    V    V    V    V
    RBX,      RBP,      RDI, RSI, RDX, RCX, R8, R9, RAX, R10, R11, R13, R14, R15

Object Header:

Constructors:
  MSB           LSB
  4 Bytes | 4 Bytes
  ObjTyp  |     Tag

  ObjTyp : 0 == Constructor without Args

Opaque Data:
  MSB           LSB
  4 Bytes | 4 Bytes
  ObjTyp  |    Size (may not contain GC pointers)

  ObjTyp : 1 == Int64 (8 Bytes no gc) TODO: "tag" field is size -> 8 bytes
  ObjTyp : 2 == String (Length in bytes)

  ObjTyp : 3 == Closure
    Closure layout:
    HEADER (4bytes type `<<` 32, 2bytes argsMissing `<<` 16, 2bytes argsStored)
    FUNCPTR (8bytes, no gc)
    ARGS [repeat argsStored] * 8 bytes -> ObjPtr (yes gc)

  ObjTyp : 4 == Char
    HEADER: (MSB 4 Bytes ObjType, LSB 4 Bytes Unicode Codepoint)

  ObjTyp : 5 == IORef
