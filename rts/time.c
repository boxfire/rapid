#include <assert.h>
#include <time.h>
#include <unistd.h>

#include "gc.h"
#include "getline.h"
#include "object.h"
#include "rts.h"

#define CLOCK_TYPE_UTC         1
#define CLOCK_TYPE_MONOTONIC   2
#define CLOCK_TYPE_DURATION    3
#define CLOCK_TYPE_PROCESS     4
#define CLOCK_TYPE_THREAD      5
#define CLOCK_TYPE_GCCPU       6
#define CLOCK_TYPE_GCREAL      7

int rapid_clock_read(Idris_TSO *base, ObjPtr dstClockObj, int clockType) {
  assert(OBJ_TYPE(dstClockObj) == OBJ_TYPE_CLOCK);

  struct timespec t;
  clockid_t clock_id;
  switch (clockType) {
    case CLOCK_TYPE_UTC:
      clock_id = CLOCK_REALTIME;
      break;
    case CLOCK_TYPE_MONOTONIC:
      clock_id = CLOCK_MONOTONIC;
      break;
    case CLOCK_TYPE_DURATION:
      // this is actually never used
      clock_id = CLOCK_MONOTONIC;
      break;
    case CLOCK_TYPE_PROCESS:
      clock_id = CLOCK_PROCESS_CPUTIME_ID;
      break;
    case CLOCK_TYPE_THREAD:
      clock_id = CLOCK_THREAD_CPUTIME_ID;
      break;
    case CLOCK_TYPE_GCCPU:
      // TODO
      clock_id = CLOCK_REALTIME;
      break;
    case CLOCK_TYPE_GCREAL:
      // TODO
      clock_id = CLOCK_REALTIME;
      break;
    default:
      rapid_C_crash("invalid clock type");
  }

  int r = clock_gettime(clock_id, &t);

  if (r == 0) {
    dstClockObj->hdr = MAKE_HEADER(OBJ_TYPE_CLOCK, 1);
    *(uint64_t *)(OBJ_GET_SLOT_ADDR(dstClockObj, 0)) = t.tv_sec;
    *(uint64_t *)(OBJ_GET_SLOT_ADDR(dstClockObj, 1)) = t.tv_nsec;
  } else {
    dstClockObj->hdr = MAKE_HEADER(OBJ_TYPE_CLOCK, 0);
  }

  return r;
}
