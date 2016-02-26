/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

// advance to first link of x greater than n
void find(PSLL_ENTRY *a, int n) {
  PSLL_ENTRY *z = a;

  while(*z != NULL && (*z)->Data <= n) {
    z = &(*z)->Flink;
  }
  *a = *z;
}

void main() {
  PSLL_ENTRY x = NULL, y;
  x = cons(4, x);
  x = cons(3, x);
  x = cons(2, x);
  x = cons(1, x);
  y = x;
  find(&x, 2);
  SLL_destroy(y);
}
