/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

void append(PSLL_ENTRY* z, PSLL_ENTRY y) {
  while(*z != NULL) {
    z = &(*z)->Flink;
  }
  *z = y;
}

void main() {
  PSLL_ENTRY x, y, z;
  x = SLL_create(nondet());
  y = SLL_create(nondet());
  z = SLL_create(nondet());
  append(&x, y);
  free(&z);
  SLL_destroy(x);
}
