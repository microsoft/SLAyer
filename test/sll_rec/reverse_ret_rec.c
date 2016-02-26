/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

PSLL_ENTRY reverse(PSLL_ENTRY a) {
  PSLL_ENTRY x = a;
  PSLL_ENTRY t, r;

  if (x != NULL) {
    t = x->Flink;
    if (t != NULL) {
      x->Flink = NULL;
      r = reverse(t);
      t->Flink = x;
      return r;
    }
  }
  return x;
}

void main() {
  PSLL_ENTRY x = NULL, y = NULL;
  x = cons(4, x);
  x = cons(3, x);
  x = cons(2, x);
  x = cons(1, x);
  print_list(x); printf_s("\n");
  x = reverse(x);
  print_list(x);
}
