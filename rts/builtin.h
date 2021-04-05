#pragma once

#include <gmp.h>

#include <stdint.h>

void rapid_builtin_init(int argc, char **argv);

int64_t rapid_bigint_real_size(const mp_limb_t *p, int64_t n);
