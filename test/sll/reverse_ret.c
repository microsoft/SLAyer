/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

PSLL_ENTRY reverse(PSLL_ENTRY a) {
  PSLL_ENTRY x = a;
  PSLL_ENTRY o, t;
  o = NULL;
  while (x != NULL) {
    t = x->Flink;
    x->Flink = o;
    o = x;
    x = t;
  }
  return o;
}

void main() {
  PSLL_ENTRY x = NULL, y = NULL;
  x = cons(4, x);
  x = cons(3, x);
  x = cons(2, x);
  x = cons(1, x);
  x = reverse(x);
  SLL_destroy(x);
}
