#include <pthread.h>

#include "object.h"
#include "rts.h"

extern ObjPtr rapid_apply_closure(Idris_TSO* base, ObjPtr closure, ObjPtr args);

void *rapid_thread(void* arg) {
  return NULL;
}

ObjPtr rapid_system_thread(Idris_TSO *base, ObjPtr ioObj) {
  rapid_C_crash("NOT IMPLEMENTED: rapid_system_thread");
  ObjPtr threadObj = rapid_C_allocate(base, HEADER_SIZE + POINTER_SIZE);
  threadObj->hdr = MAKE_HEADER(OBJ_TYPE_OPAQUE, POINTER_SIZE);

  pthread_t *thread = rapid_C_allocate(base, sizeof(pthread_t));
  threadObj->data = thread;

  if (thread == NULL) {
    rapid_C_crash("FIXME: fail gracefully OOM in rapid_system_thread");
  }

  //FIXME: VERIFY ioObj is IO()
  int status = pthread_create(thread, NULL, rapid_thread, OBJ_PAYLOAD(ioObj));
  if (status == 0) {
    base->rapid_errno = 0;
  } else {
    base->rapid_errno = status;
    rapid_C_crash("FIXME: fail gracefully error in rapid_system_thread");
  }

  return threadObj;
}

ObjPtr rapid_system_thread_wait(Idris_TSO *base, ObjPtr threadObj) {

  rapid_C_crash("NOT IMPLEMENTED: rapid_system_thread_wait");
  return NULL;
}

typedef struct {
  Idris_TSO *base;
  pthread_t thread;
  ObjPtr work;
  ObjPtr result;
  bool ready;
  pthread_mutex_t mutex;
  pthread_cond_t signal;
} rapid_future_t;

void *rapid_future_thread(void* arg) {
  rapid_future_t *future = (rapid_future_t*)arg;

  future->result = rapid_apply_closure(future->base, future->work, NULL);

  future->ready = true;
  pthread_cond_signal(&future->signal);
  return NULL;
}

ObjPtr rapid_system_make_future(Idris_TSO *base, ObjPtr workObj) {
  ObjPtr futureObj = rapid_C_allocate(base, HEADER_SIZE + POINTER_SIZE);
  futureObj->hdr = MAKE_HEADER(OBJ_TYPE_OPAQUE, POINTER_SIZE);

  rapid_future_t *future = rapid_C_allocate(base, sizeof(rapid_future_t));
  futureObj->data = future;

  if (future == NULL) {
    base->rapid_errno = 1;
    rapid_C_crash("FIXME: fail gracefully OOM in rapid_system_make_future");
  }

  future->ready = false;
  future->base = base; //FIXME!!
  future->work = workObj;

  int status = pthread_mutex_init(&future->mutex, NULL);

  if (status != 0) {
    base->rapid_errno = status;
    rapid_C_crash("FIXME: fail gracefully error in rapid_system_make_future failed mutex_init");
  }

  status = pthread_cond_init(&future->signal, NULL);

  if (status != 0) {
    base->rapid_errno = status;
    rapid_C_crash("FIXME: fail gracefully error in rapid_system_make_future failed cond_init");
  }

  status = pthread_create(&future->thread, NULL, rapid_future_thread, future);

  if (status == 0) {
    base->rapid_errno = 0;
  } else {
    base->rapid_errno = status;
    rapid_C_crash("FIXME: fail gracefully error in rapid_system_make_future create thread");
  }

  return futureObj;
}

ObjPtr rapid_system_await_future(Idris_TSO *base, ObjPtr FIXMEWhat, ObjPtr futureObj) {
  assert(OBJ_TYPE(futureObj) == OBJ_TYPE_OPAQUE);
  assert(OBJ_SIZE(futureObj) == POINTER_SIZE);

  rapid_future_t *future = *(rapid_future_t**)OBJ_PAYLOAD(futureObj);
  int status = pthread_mutex_lock(&future->mutex);

  if (status != 0) {
    base->rapid_errno = status;
    rapid_C_crash("FIXME: rapid_system_await_future failed to lock mutex");
  }

  if (!future->ready) {
    status = pthread_cond_wait(&future->signal, &future->mutex);

    if (status != 0) {
      base->rapid_errno = status;
      rapid_C_crash("FIXME: rapid_system_await_future failed to cond_wait");
    }
  }

  //FIXME: clean up future!? Copy obj from stack/heap?
  ObjPtr result = future->result;

  return result;
}

