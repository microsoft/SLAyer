/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

PSLL_ENTRY splice(PSLL_ENTRY x, PSLL_ENTRY y) {
  PSLL_ENTRY t;

  t = y;
  if (x != NULL) {
    x->Flink = splice(y, x->Flink);
    t = x;
  }
  return t;
}

void main() {
  PSLL_ENTRY x = NULL, y = NULL, z;
  x = cons(3, x);
  x = cons(2, x);
  x = cons(1, x);
  y = cons(7, y);
  y = cons(6, y);
  y = cons(5, y);
  y = cons(4, y);
  print_list(x); printf_s("\n");
  print_list(y); printf_s("\n");
  z = splice(x, y);
  print_list(z);
}
