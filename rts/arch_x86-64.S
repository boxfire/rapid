#define SYM(s) _##s

# args:
#   %rdi  func_ptr
#   %rsi  tso
#   %rdx  tso->sched_jmp_buf
#   %rcx  tso->stack_top
.globl SYM(rapid_run_task)
SYM(rapid_run_task):

  # save jmp_buf in a callee-saved register
  movq %rdx, %r15

  movq %rcx, %rsp

  movq %rdi, %rax
  movq %rsi, %rdi
  callq *%rax

  movq %r15, %rdi
  movq $127, %rsi
  callq SYM(longjmp)

# longjmp does not return, trigger a crash if it still does
  movq $0x0, %rcx
  movq $0x0, 0(%rcx)
