#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include <sys/errno.h>
#include <sys/mman.h>
#include <sys/stat.h>

#include "gc.h"
#include "object.h"
#include "rts.h"

static int rapid_global_argc = 0;
static char **rapid_global_argv = NULL;

void rapid_strreverse(char *restrict dst, const char *restrict src, int64_t size) {
  // FIXME: this reverses bytes, not characters (i.e. works only for ASCII)
  for (int64_t i = 0; i < size; ++i) {
    dst[size - 1 - i] = src[i];
  }
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
  fflush(stdout);
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

void rapid_builtin_init(int argc, char **argv) {
  rapid_global_argc = argc;
  rapid_global_argv = argv;
}