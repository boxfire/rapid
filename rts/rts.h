#pragma once

#define _POSIX_C_SOURCE 200809L

#include <unistd.h>
#include <setjmp.h>
#include <stdbool.h>
#include <stdint.h>

struct RTSConfig {
  bool debug_always_gc;
  bool debug_heap_write_poison;
};

extern struct RTSConfig *rapid_global_config;

struct Idris_TSO_t {
  uint8_t *nurseryStart;
  uint8_t *nurseryNext;
  uint8_t *nurseryEnd;

  int rapid_errno;

  void *stack_bottom;
  void *stack_top;
  size_t stack_size;

  uint64_t heap_alloc;
  uint64_t next_nursery_size;
  uint8_t *heap_aux;
  uint8_t *heap_aux_end;

  jmp_buf sched_jmp_buf;
};

typedef struct Idris_TSO_t Idris_TSO;

typedef uint64_t Word;

#define POINTER_SIZE (sizeof(void*))

void rapid_C_crash(const char *msg);

void *rapid_C_allocate(Idris_TSO *base, int32_t size) __attribute__((__malloc__)) __attribute__((alloc_size(2)));
