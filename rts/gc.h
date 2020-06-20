#pragma once

#include "rts.h"

void rapid_gc_init();
void *rapid_C_allocate(Idris_TSO *base, int32_t size);
