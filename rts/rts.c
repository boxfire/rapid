#include <stdio.h>
#include <stdlib.h>

const size_t IDRIS_ALIGNMENT = 8;
const size_t NURSERY_SIZE = 1024;

typedef struct {
  void *nurseryStart;
  void *nurseryEnd;
} Idris_TSO;

extern long idris_enter(void *baseTSO, void *nurseryStart, void *nurseryEnd);

void idris_rts_gc(long arg0) {
  puts("GC called\n");
  exit(2);
}

int main(int argc, char **argv) {
  Idris_TSO *tso = malloc(sizeof(Idris_TSO));
  tso->nurseryStart = malloc(NURSERY_SIZE);
  tso->nurseryEnd = (void *)((long int)tso->nurseryStart + NURSERY_SIZE);

  return idris_enter(tso, tso->nurseryStart, tso->nurseryEnd);
}
