/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

// return first link of x greater than n
PSLL_ENTRY find(PSLL_ENTRY x, int n) {
  while (x != NULL) {
    if (x->Data > n) return x;
    x = x->Flink;
  }
  return NULL;
}

void main() {
  PSLL_ENTRY x = NULL, y = NULL;
  x = cons(4, x);
  x = cons(3, x);
  x = cons(2, x);
  x = cons(1, x);
  y = find(x, 2);
  SLL_destroy(x);
}
