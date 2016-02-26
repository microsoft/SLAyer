/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

void append(PSLL_ENTRY* z, PSLL_ENTRY y) {
  while(*z != NULL) {
    z = &(*z)->Flink;
  }
  *z = y;
}

void main() {
  PSLL_ENTRY x, y;
  x = SLL_create(nondet());
  y = SLL_create(nondet());
  append(&x, y);
  SLL_destroy(x);
}
