// Code in this file is partially copied from idris/support/c/idris_net.c:
//     (C) Simon Fowler, 2014
//     MIT Licensed. Have fun!

#include "gc.h"
#include "object.h"
#include "rts.h"

#include <assert.h>
#include <netinet/in.h>
#include <stdio.h>
#include <unistd.h>

// Get the address family constants out of C and into Idris
int idrnet_af_unspec(Idris_TSO *base, ObjPtr _world) {
    return AF_UNSPEC;
}

int idrnet_af_unix(Idris_TSO *base, ObjPtr _world) {
    return AF_UNIX;
}

int idrnet_af_inet(Idris_TSO *base, ObjPtr _world) {
    return AF_INET;
}

int idrnet_af_inet6(Idris_TSO *base, ObjPtr _world) {
    return AF_INET6;
}

int64_t idrnet_socket(Idris_TSO *base, int64_t domain, int64_t type, int64_t protocol, ObjPtr _world) {
  return socket(domain, type, protocol);
}

ObjPtr idrnet_create_sockaddr(Idris_TSO *base, ObjPtr _world) {
  int size = sizeof(struct sockaddr_storage);
  ObjPtr saObj = rapid_C_allocate(base, HEADER_SIZE + size);
  saObj->hdr = MAKE_HEADER(OBJ_TYPE_OPAQUE, size);
  return saObj;
}

int64_t idrnet_bind(Idris_TSO *base, int64_t sockfd, int64_t family, int64_t type, ObjPtr hostnameStrObj, int64_t port, ObjPtr _world) {
  rapid_C_crash("bind not implemented");
}

int64_t idrnet_listen(Idris_TSO *base, int64_t sockfd, int64_t backlog, ObjPtr _world) {
  rapid_C_crash("listen not implemented");
}

int64_t idrnet_accept(Idris_TSO *base, int64_t sockfd, ObjPtr destSockAddrPtr, ObjPtr _world) {
  rapid_C_crash("accept not implemented");
}

int64_t idrnet_sockaddr_family(Idris_TSO *base, ObjPtr sockAddrPtr, ObjPtr _world) {
  struct sockaddr *saddr = (struct sockaddr *)OBJ_PAYLOAD(sockAddrPtr);
  return saddr->sa_family;
}

ObjPtr idrnet_sockaddr_ipv4(Idris_TSO *base, ObjPtr sockAddrPtr, ObjPtr _world) {
  rapid_C_crash("sockaddr_ipv4 not implemented");
}

void idrnet_free(Idris_TSO *base, ObjPtr ptr, ObjPtr _world) {
  // NOOP, wait for GC
}
