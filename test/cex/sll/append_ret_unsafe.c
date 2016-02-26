/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

PSLL_ENTRY append(PSLL_ENTRY x, PSLL_ENTRY y) {
  PSLL_ENTRY t, n, r;
  if (x != NULL) {
    t = x;
    n = x->Flink;
    while (n != NULL) {
      n = n->Flink;
      t = n;
    }
    t->Flink = y;
    return x;
  } else {
    return y;
  }
}

void main() {
  PSLL_ENTRY x, y;
  x = SLL_create(nondet());
  y = SLL_create(nondet());
  x = append(x, y);
  SLL_destroy(x);
}
