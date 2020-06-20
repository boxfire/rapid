#pragma once

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include <sys/errno.h>
#include <sys/mman.h>
#include <sys/stat.h>

#include "rts.h"

#define OBJ_TYPE_CON_NO_ARGS 0xff
#define OBJ_TYPE_INT         0x01
#define OBJ_TYPE_DOUBLE      0x01
#define OBJ_TYPE_STRING      0x02
#define OBJ_TYPE_CLOSURE     0x03
#define OBJ_TYPE_CHAR        0x04
#define OBJ_TYPE_IOREF       0x05
#define OBJ_TYPE_BUFFER      0x06
#define OBJ_TYPE_OPAQUE      0x07
#define OBJ_TYPE_PTR         0x08

#define OBJ_TYPE_FWD_REF     0xfd

typedef uint64_t RapidObjectHeader;

#define HEADER_SIZE 8

typedef struct {
  RapidObjectHeader hdr;
  void *data;
} RapidObject_t;

typedef RapidObject_t *ObjPtr;

int dump_obj(ObjPtr o);
int dump_obj_i(ObjPtr o, int indent);


static inline uint32_t OBJ_TYPE(ObjPtr p) {
  //return ((p->hdr >> 32) & 0xffffffff);
  return ((p->hdr >> 32) & 0xff);
}

static inline uint32_t OBJ_SIZE(ObjPtr p) {
  return ((p->hdr) & 0xffffffff);
}

static inline uint32_t OBJ_TAG(ObjPtr p) {
  return ((p->hdr) & 0xffffffff);
}

static inline ObjPtr OBJ_PROJECT(ObjPtr p, int32_t pos) {
  return ((ObjPtr*)&p->data)[pos];
}

static inline void OBJ_PUT_SLOT(ObjPtr p, int32_t pos, ObjPtr d) {
  ((ObjPtr*)&p->data)[pos] = d;
}

static inline ObjPtr OBJ_GET_SLOT(ObjPtr p, int32_t pos) {
  return ((ObjPtr*)&p->data)[pos];
}

static inline void *OBJ_GET_SLOT_ADDR(ObjPtr p, int32_t pos) {
  return &((&p->data)[pos]);
}

static inline void *OBJ_PAYLOAD(ObjPtr p) {
  return &(p->data);
}

static inline bool OBJ_IS_INLINE(ObjPtr p) {
  return (p == NULL || (uint64_t)p & 0x07);
}

static inline bool OBJ_IS_FWD_INPLACE(ObjPtr p) {
  return (p->hdr & 0x8000000000000000ull);
}

static inline RapidObjectHeader MAKE_HEADER(int64_t objType, int32_t sizeOrTag) {
  return (objType << 32) | sizeOrTag;
}

static inline uint32_t OBJ_TOTAL_SIZE(ObjPtr p) {
  assert(!OBJ_IS_INLINE(p));

  uint64_t h = p->hdr;
  switch (OBJ_TYPE(p)) {
    case OBJ_TYPE_CON_NO_ARGS:
      return 8 + 8 * (h >> 40);
    case OBJ_TYPE_PTR:
      return 8 + POINTER_SIZE;
    case OBJ_TYPE_IOREF:
      return 8 + POINTER_SIZE;
    case OBJ_TYPE_OPAQUE:
      return 8 + POINTER_SIZE;
    case OBJ_TYPE_CHAR:
      return 8;
    case OBJ_TYPE_INT:
      return 8 + 8;
    case OBJ_TYPE_BUFFER:
    case OBJ_TYPE_STRING:
      return 8 + OBJ_SIZE(p);
    case OBJ_TYPE_CLOSURE:
      return 16 + 8 * (h & 0xffff);
    case OBJ_TYPE_FWD_REF:
      rapid_C_crash("invalid fwd ref in OBJ_TOTAL_SIZE");
      return 0;
    default:
      fprintf(stderr, "unknown object type: 0x%08llx\n", (h>>32));
      rapid_C_crash("unknown object type");
      return -1;
  }
}
