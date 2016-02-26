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
  return r;
}

/* reverse recursively using append */
PSLL_ENTRY reverse(PSLL_ENTRY x) {
  PSLL_ENTRY xf, t = NULL, u;

  if (x == NULL) return t;
  xf = x->Flink;
  x->Flink = NULL;
  t->Flink = append(reverse(xf), x);
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
