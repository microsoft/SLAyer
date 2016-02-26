/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

PSLL_ENTRY append(PSLL_ENTRY x, PSLL_ENTRY y) {
  PSLL_ENTRY r;

  if (x != NULL) {
    x->Flink = append(x->Flink, y);
    return x;
  } else {
    return y;
  }
}

void main() {
  PSLL_ENTRY x = NULL, y = NULL;
  x = cons(4, x);
  x = cons(3, x);
  x = cons(2, x);
  x = cons(1, x);
  print_list(x); printf_s("\n");
  y = cons(6, y);
  y = cons(1, y);
  y = cons(4, y);
  x = append(x, y);
  print_list(x);
}
