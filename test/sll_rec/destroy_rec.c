/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

void destroy(PSLL_ENTRY x) {
  PSLL_ENTRY t, u;
  if (x != NULL) {
    t = x;
    u = x->Flink;
    free(t);
    destroy(u);
  }
}

void main() {
  PSLL_ENTRY x = NULL;
  x = cons(4, x);
  x = cons(3, x);
  x = cons(2, x);
  x = cons(1, x);
  destroy(x);
}
