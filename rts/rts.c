#include <stdio.h>
#include <stdlib.h>

#include <gc/gc.h>

const size_t IDRIS_ALIGNMENT = 8;
const size_t NURSERY_SIZE = 1 * 1024 * 1024;

typedef struct {
  void *nurseryStart;
  void *nurseryNext;
  void *nurseryEnd;
} Idris_TSO;

extern long idris_enter(void *baseTSO);

void idris_rts_crash(long arg0) {
  printf("CRASH called: %ld\n", arg0);
  exit(3);
}

void idris_rts_gc(long arg0) {
  printf("GC called: 0x%016lx\n", arg0);
  exit(2);
}

int64_t idris_rts_int_to_str(char *dst, int64_t val) {
  int64_t size = snprintf(dst, 24, "%lld", val);
  return size;
}

int main(int argc, char **argv) {
  GC_init();

  Idris_TSO *tso = malloc(sizeof(Idris_TSO));
  tso->nurseryStart = malloc(NURSERY_SIZE);
  tso->nurseryNext = tso->nurseryStart;
  tso->nurseryEnd = (void *)((long int)tso->nurseryStart + NURSERY_SIZE);

  return idris_enter(tso);
}
