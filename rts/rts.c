#include <assert.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/errno.h>
#include <sys/mman.h>
#include <sys/stat.h>

#include <gc.h>

const size_t IDRIS_ALIGNMENT = 8;
const size_t NURSERY_SIZE = 1 * 1024 * 1024;

static const size_t RAPID_STACK_SIZE = 128 * 1024 * 1024;

const int HEADER_SIZE = 8;
const int POINTER_SIZE = sizeof(void*);

const int64_t OBJ_TYPE_CON_NO_ARGS = 0xff;
const int64_t OBJ_TYPE_INT         = 0x01;
const int64_t OBJ_TYPE_STRING      = 0x02;
const int64_t OBJ_TYPE_CLOSURE     = 0x03;
const int64_t OBJ_TYPE_CHAR        = 0x04;
const int64_t OBJ_TYPE_IOREF       = 0x05;
const int64_t OBJ_TYPE_BUFFER      = 0x06;
const int64_t OBJ_TYPE_OPAQUE      = 0x07;
const int64_t OBJ_TYPE_PTR         = 0x08;

typedef struct {
  void *nurseryStart;
  void *nurseryNext;
  void *nurseryEnd;

  int rapid_errno;

  void *stack_bottom;
  void *stack_top;
  size_t stack_size;

  jmp_buf sched_jmp_buf;
} Idris_TSO;

static int rapid_global_argc = 0;
static char **rapid_global_argv = NULL;

typedef uint64_t RapidObjectHeader;

typedef uint64_t Word;

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

static inline RapidObjectHeader MAKE_HEADER(int64_t objType, int32_t sizeOrTag) {
  return (objType << 32) | sizeOrTag;
}

extern long idris_enter(void *baseTSO);

void idris_mkcon_ok(ObjPtr o) {
  fprintf(stderr, "MKCON ok: %p\n", (void *)o);
}

void idris_mkcon_arg_ok(ObjPtr o, int64_t idx) {
  fprintf(stderr, "MKCON arg ok: %lld -> %p\n", idx, (void *)OBJ_GET_SLOT(o, idx));
  dump_obj_i(OBJ_GET_SLOT(o, idx), 0);
}

void idris_rts_crash(long arg0) {
  printf("CRASH called: %ld\n", arg0);
  exit(3);
}

void idris_rts_crash_msg(ObjPtr msg) {
  int length = OBJ_SIZE(msg);
  const char *str = (const char *)&(msg->data);
  fprintf(stderr, "ERROR: ");
  fwrite(str, length, 1, stderr);
  fprintf(stderr, "\n");
  exit(4);
}

void rapid_C_crash(const char *msg) {
  fprintf(stderr, "ERROR: %s\n", msg);
  exit(5);
}

void rapid_strreverse(char *restrict dst, const char *restrict src, int64_t size) {
  // FIXME: this reverses bytes, not characters (i.e. works only for ASCII)
  for (int64_t i = 0; i < size; ++i) {
    dst[size - 1 - i] = src[i];
  }
}

void idris_rts_gc(long arg0) {
  printf("GC called: 0x%016lx\n", arg0);
  exit(2);
}

void *rapid_C_allocate(Idris_TSO *base, int64_t size) {
  return GC_malloc(size);
}

int64_t idris_rts_int_to_str(char *dst, int64_t val) {
  int64_t size = snprintf(dst, 24, "%lld", val);
  return size;
}

int64_t idris_rts_double_to_str(char *dst, int64_t size, double val) {
  int64_t needed_size = snprintf(dst, size, "%g", val);
  return needed_size;
}

double idris_rts_str_to_double(ObjPtr obj) {
  int length = OBJ_SIZE(obj);
  const char *str = (const char *)OBJ_PAYLOAD(obj);
  char *scopy = (char *)alloca(length + 1);
  memcpy(scopy, str, length);
  scopy[length] = '\0';
  return strtod(scopy, NULL);
}

int64_t idris_rts_str_to_int(ObjPtr obj) {
  int length = OBJ_SIZE(obj);
  const char *str = (const char *)&(obj->data);
  char *scopy = (char *)alloca(length + 1);
  memcpy(scopy, str, length);
  scopy[length] = '\0';
  return strtoll(scopy, NULL, 10);
}

int64_t rapid_system_file_size(Idris_TSO *base, ObjPtr filePtrObj, ObjPtr _world) {
  assert (OBJ_TYPE(filePtrObj) == OBJ_TYPE_OPAQUE);
  assert (OBJ_SIZE(filePtrObj) == POINTER_SIZE);

  struct stat stat_buf;

  FILE *f = *(FILE **)OBJ_GET_SLOT_ADDR(filePtrObj, 0);
  int fd = fileno(f);
  int stat_error = fstat(fd, &stat_buf);

  if (stat_error != 0) {
    base->rapid_errno = errno;
    return -1;
  }

  return stat_buf.st_size;
}

int64_t idris_rts_write_buffer_data(Idris_TSO *base, ObjPtr filePtrObj, ObjPtr bufObj, int64_t loc, int64_t maxSize, ObjPtr _world) {
  assert (loc >= 0);
  assert (maxSize >= 0);
  assert (OBJ_TYPE(filePtrObj) == OBJ_TYPE_OPAQUE && OBJ_SIZE(filePtrObj) == POINTER_SIZE);
  assert (OBJ_TYPE(bufObj) == OBJ_TYPE_BUFFER && OBJ_SIZE(bufObj) >= (loc + maxSize));

  FILE *f = *(FILE **)OBJ_GET_SLOT_ADDR(filePtrObj, 0);
  const char *payload = OBJ_PAYLOAD(bufObj);
  const char *begin = payload + loc;

  size_t written = fwrite(begin, 1, maxSize, f);
  if (written != maxSize) {
    return -1;
  }

  return written;
}

int64_t idris_rts_read_buffer_data(Idris_TSO *base, ObjPtr filePtrObj, ObjPtr bufObj, int64_t loc, int64_t maxSize, ObjPtr _world) {
  assert (loc >= 0);
  assert (maxSize >= 0);
  assert (OBJ_TYPE(filePtrObj) == OBJ_TYPE_OPAQUE && OBJ_SIZE(filePtrObj) == POINTER_SIZE);
  assert (OBJ_TYPE(bufObj) == OBJ_TYPE_BUFFER && OBJ_SIZE(bufObj) >= (loc + maxSize));

  FILE *f = *(FILE **)OBJ_GET_SLOT_ADDR(filePtrObj, 0);
  char *payload = OBJ_PAYLOAD(bufObj);
  char *begin = payload + loc;

  size_t read = fread(begin, 1, maxSize, f);
  if (read != maxSize) {
    return -1;
  }

  return read;
}

ObjPtr rapid_system_file_open(Idris_TSO *base, ObjPtr fnameObj, ObjPtr modeObj, int64_t _unused0, ObjPtr _world) {
  ObjPtr ptrObj = rapid_C_allocate(base, HEADER_SIZE + POINTER_SIZE);
  ptrObj->hdr = MAKE_HEADER(OBJ_TYPE_OPAQUE, POINTER_SIZE);

  int length = OBJ_SIZE(fnameObj);
  const char *str = (const char *)OBJ_PAYLOAD(fnameObj);
  char *fnameCstr = (char *)alloca(length + 1);
  memcpy(fnameCstr, str, length);
  fnameCstr[length] = '\0';

  str = (const char *)OBJ_PAYLOAD(modeObj);
  char *modeCstr = (char *)alloca(length + 1);
  memcpy(modeCstr, str, length);
  modeCstr[length] = '\0';

  FILE *f = fopen(fnameCstr, modeCstr);
  if (f) {
    base->rapid_errno = 0;
  } else {
    base->rapid_errno = errno;
  }

  ptrObj->data = f;

  return ptrObj;
}

void rapid_putstr(Idris_TSO *base, ObjPtr strObj, ObjPtr _world) {
  assert(OBJ_TYPE(strObj) == OBJ_TYPE_STRING);
  int64_t length = OBJ_SIZE(strObj);
  fwrite(OBJ_PAYLOAD(strObj), length, 1, stdout);
}

void rapid_system_file_close(Idris_TSO *base, ObjPtr filePtrObj, ObjPtr _world) {
  if (OBJ_TYPE(filePtrObj) != OBJ_TYPE_OPAQUE || OBJ_SIZE(filePtrObj) != POINTER_SIZE) {
    rapid_C_crash("invalid object passed to file_close");
  }

  FILE *f = *(FILE **)OBJ_PAYLOAD(filePtrObj);
  fclose(f);
}

Word rapid_system_file_write_string(Idris_TSO *base, ObjPtr filePtrObj, ObjPtr strObj, ObjPtr _world) {
  if (OBJ_TYPE(filePtrObj) != OBJ_TYPE_OPAQUE || OBJ_SIZE(filePtrObj) != POINTER_SIZE) {
    rapid_C_crash("invalid object passed to file_write_string");
  }

  FILE *f = *(FILE **)OBJ_PAYLOAD(filePtrObj);
  const void *str = OBJ_PAYLOAD(strObj);
  size_t size = OBJ_SIZE(strObj);
  size_t written = fwrite(str, 1, size, f);
  if (written != size) {
    return 1;
  }
  return 0;
}

// return type: Ptr String
ObjPtr rapid_system_file_read_line(Idris_TSO *base, ObjPtr filePtrObj, ObjPtr _world) {
  if (OBJ_TYPE(filePtrObj) != OBJ_TYPE_OPAQUE || OBJ_SIZE(filePtrObj) != POINTER_SIZE) {
    rapid_C_crash("invalid object passed to file_read_line");
  }

  FILE *f = *(FILE **)OBJ_PAYLOAD(filePtrObj);
  size_t length = 0;
  char *buffer = fgetln(f, &length);
  if (errno != 0) {
    base->rapid_errno = errno;
    rapid_C_crash("getline failed");
  }

  ObjPtr newStr = rapid_C_allocate(base, HEADER_SIZE + length);
  newStr->hdr = MAKE_HEADER(OBJ_TYPE_STRING, length);
  memcpy(OBJ_PAYLOAD(newStr), buffer, length);

  ObjPtr newPtr = rapid_C_allocate(base, HEADER_SIZE + POINTER_SIZE);
  newPtr->hdr = MAKE_HEADER(OBJ_TYPE_PTR, 1);
  OBJ_PUT_SLOT(newPtr, 0, newStr);

  return newPtr;
}

Word rapid_system_file_eof(Idris_TSO *base, ObjPtr filePtrObj, ObjPtr _world) {
  if (OBJ_TYPE(filePtrObj) != OBJ_TYPE_OPAQUE || OBJ_SIZE(filePtrObj) != POINTER_SIZE) {
    rapid_C_crash("invalid object passed to file_eof");
  }

  FILE *f = *(FILE **)OBJ_PAYLOAD(filePtrObj);
  return (0 != feof(f));
}

const int TAG_LIST_NIL = 0;
const int TAG_LIST_CONS = 1;

static inline ObjPtr MK_LIST_NIL(Idris_TSO *base) {
  ObjPtr p = rapid_C_allocate(base, HEADER_SIZE);
  p->hdr = MAKE_HEADER(OBJ_TYPE_CON_NO_ARGS, TAG_LIST_NIL);
  return p;
}

static inline ObjPtr MK_LIST_CONS(Idris_TSO *base, ObjPtr head, ObjPtr tail) {
  ObjPtr p = rapid_C_allocate(base, HEADER_SIZE + 2 * POINTER_SIZE);
  p->hdr = MAKE_HEADER(OBJ_TYPE_CON_NO_ARGS | (2 << 8), TAG_LIST_CONS);
  OBJ_PUT_SLOT(p, 0, head);
  OBJ_PUT_SLOT(p, 1, tail);
  return p;
}

ObjPtr rapid_fast_pack(Idris_TSO *base, ObjPtr charListObj) {
  assert(OBJ_TYPE(charListObj) == OBJ_TYPE_CON_NO_ARGS);

  // FIXME: only works for ASCII
  int32_t strLength = 0;
  ObjPtr cursor = charListObj;
  while (OBJ_TAG(cursor) == TAG_LIST_CONS) {
    assert(OBJ_TYPE(cursor) == OBJ_TYPE_CON_NO_ARGS);
    strLength += 1;
    cursor = OBJ_PROJECT(cursor, 1);
  }

  ObjPtr newStr = rapid_C_allocate(base, HEADER_SIZE + strLength);
  newStr->hdr = MAKE_HEADER(OBJ_TYPE_STRING, strLength);

  int32_t strPos = 0;
  char *dst = OBJ_PAYLOAD(newStr);
  cursor = charListObj;
  while (OBJ_TAG(cursor) == TAG_LIST_CONS) {
    assert(OBJ_TYPE(cursor) == OBJ_TYPE_CON_NO_ARGS);
    assert(strPos < strLength);
    ObjPtr charObj = OBJ_PROJECT(cursor, 0);
    assert(OBJ_TYPE(charObj) == OBJ_TYPE_CHAR);
    dst[strPos] = OBJ_SIZE(charObj) & 0xff;
    strPos += 1;
    cursor = OBJ_PROJECT(cursor, 1);
  }
  return newStr;
}

ObjPtr rapid_fast_append(Idris_TSO *base, ObjPtr strListObj) {
  uint64_t strLength = 0;
  ObjPtr cursor = strListObj;
  while (OBJ_TAG(cursor) == TAG_LIST_CONS) {
    assert(OBJ_TYPE(cursor) == OBJ_TYPE_CON_NO_ARGS);
    ObjPtr partObj = OBJ_PROJECT(cursor, 0);
    assert(OBJ_TYPE(partObj) == OBJ_TYPE_STRING);
    strLength += OBJ_SIZE(partObj);

    cursor = OBJ_PROJECT(cursor, 1);
  }

  if (strLength > 0xffffffffull) {
    rapid_C_crash("fastAppend string is too large");
  }

  ObjPtr newStr = rapid_C_allocate(base, HEADER_SIZE + strLength);
  newStr->hdr = MAKE_HEADER(OBJ_TYPE_STRING, strLength);

  uint32_t strPos = 0;
  char *dst = OBJ_PAYLOAD(newStr);
  cursor = strListObj;
  while (OBJ_TAG(cursor) == TAG_LIST_CONS) {
    ObjPtr partObj = OBJ_PROJECT(cursor, 0);
    assert(OBJ_TYPE(partObj) == OBJ_TYPE_STRING);
    int32_t partLength = OBJ_SIZE(partObj);

    if (partLength > 0) {
      assert(strPos < strLength);
      memcpy(&dst[strPos], OBJ_PAYLOAD(partObj), partLength);
    }

    strPos += partLength;
    cursor = OBJ_PROJECT(cursor, 1);
  }
  return newStr;
}

ObjPtr rapid_system_getargs(Idris_TSO *base, ObjPtr _world) {
  ObjPtr result = MK_LIST_NIL(base);

  for (int i = rapid_global_argc - 1; i >= 0; --i) {
    char *thisArg = rapid_global_argv[i];
    size_t thisLength = strlen(thisArg);
    ObjPtr argStrObj = rapid_C_allocate(base, HEADER_SIZE + thisLength);
    argStrObj->hdr = MAKE_HEADER(OBJ_TYPE_STRING, thisLength);
    memcpy(OBJ_PAYLOAD(argStrObj), thisArg, thisLength);

    result = MK_LIST_CONS(base, argStrObj, result);
  }

  return result;
}

void *log_GC_malloc(int64_t size) {
  void *mem = GC_malloc(size);
  fprintf(stderr, "GC_malloc: %p (%lld)\n", mem, size);
  return mem;
}

#define INDENT(i) for(int indent_i=0;indent_i<i;++indent_i){fprintf(stderr, "  ");};
int dump_obj_i(ObjPtr o, int indent) {
  if (indent > 4) {
    fprintf(stderr, "MAX DEPTH REACHED\n");
    return 0;
  }

  INDENT(indent); fprintf(stderr, "DUMP OBJ AT %p\n", (void *)o);
  if (o == NULL) {
    return 0;
  }
  INDENT(indent); fprintf(stderr, "header: 0x%016llx\n", o->hdr);
  if (OBJ_TYPE(o) == OBJ_TYPE_CON_NO_ARGS) {
    int64_t num_args = o->hdr >> 40;
    for (int i = 0; i < num_args; ++i) {
      INDENT(indent); fprintf(stderr, "CON ARG %d:\n", i);
      dump_obj_i(OBJ_GET_SLOT(o, i), indent+1);
    }
  }
  if (OBJ_TYPE(o) == OBJ_TYPE_CLOSURE) {
    int64_t num_args = o->hdr & 0xffff;
    INDENT(indent); fprintf(stderr, "CLOSURE func: %p\n", (void *)OBJ_GET_SLOT(o, 0));
    for (int i = 0; i < num_args; ++i) {
      INDENT(indent); fprintf(stderr, "CLOSURE ARG %d:\n", i);
      dump_obj_i(OBJ_GET_SLOT(o, i+1), indent+1);
    }
  }
  if (OBJ_TYPE(o) == OBJ_TYPE_INT) {
    INDENT(indent); fprintf(stderr, "INT value: %lld\n", (int64_t)OBJ_GET_SLOT(o, 0));
  }
  if (OBJ_TYPE(o) == OBJ_TYPE_STRING) {
    char *strCopy = malloc(1 + OBJ_SIZE(o));
    memcpy(strCopy, OBJ_PAYLOAD(o), OBJ_SIZE(o));
    strCopy[OBJ_SIZE(o)] = '\0';
    INDENT(indent); fprintf(stderr, "STRING(%d): \"%s\"\n", OBJ_SIZE(o), strCopy);
    free(strCopy);
  }
  return 0;
}

int dump_obj(ObjPtr o) {
  dump_obj_i(o, 0);
  return 0;
}

void idris_rts_crash_typecheck(ObjPtr obj, int64_t expectedType) {
  fprintf(stderr, "Object failed typecheck, expected type: %04llx\n", expectedType);
  fprintf(stderr, "  object address: %p\n", (void *)obj);
  fprintf(stderr, "  object header:  0x%016llx\n", obj->hdr);
  dump_obj(obj);
  exit(123);
}

void  task_start(Idris_TSO *tso) {
  int jump_result;
  if ((jump_result = setjmp(tso->sched_jmp_buf)) == 0) {
    register void *top = tso->stack_top;
    __asm__ volatile(
        "mov %[rs], %%rsp \n"
        : [ rs ] "+r" (top) ::
        );

    idris_enter(tso);

    longjmp(tso->sched_jmp_buf, 127);
  } else {
    // fprintf(stderr, "task finished: %d\n", jump_result);
  }

  // we can not return from this function
}

int main(int argc, char **argv) {
  GC_INIT();

  rapid_global_argc = argc;
  rapid_global_argv = argv;

  Idris_TSO *tso = malloc(sizeof(Idris_TSO));
  tso->nurseryStart = malloc(NURSERY_SIZE);
  tso->nurseryNext = tso->nurseryStart;
  tso->nurseryEnd = (void *)((long int)tso->nurseryStart + NURSERY_SIZE);
  tso->rapid_errno = 1;

  tso->stack_size = RAPID_STACK_SIZE;
  size_t pagesize = getpagesize();
  void *stackmem = mmap(0, tso->stack_size + 2 * pagesize, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (stackmem == MAP_FAILED) {
    fprintf(stderr, "stack alloc failed\n");
    exit(8);
  }

  // map guard page below the stack
  void *guard_page_bottom = mmap(stackmem, pagesize, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED, -1, 0);
  if (guard_page_bottom == MAP_FAILED) {
    fprintf(stderr, "stack bottom guard page alloc failed\n");
    exit(9);
  }
  void *guard_page_top = mmap((char *)stackmem + pagesize + tso->stack_size, pagesize, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED, -1, 0);
  if (guard_page_top == MAP_FAILED) {
    fprintf(stderr, "stack top guard page alloc failed\n");
    exit(9);
  }

  tso->stack_bottom = (char *)stackmem + pagesize;
  tso->stack_top = (char *)(tso->stack_bottom) + tso->stack_size - pagesize;
  *(int64_t *)tso->stack_top = 0x7f7d7c7b12345678;
  *((int64_t *)tso->stack_top - 1) = 0x7f7d7c7b12345678;
  *((int64_t *)tso->stack_top - 2) = 0x7f7d7c7b12345678;
  //fprintf(stderr, "stack allocated at: %p (+%ld)\n", tso->stack_bottom, tso->stack_size);

  /*GC_disable();*/
  task_start(tso);

  return 0;
}
