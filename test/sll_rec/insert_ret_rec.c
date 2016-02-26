/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

PSLL_ENTRY insert(PSLL_ENTRY a, PSLL_ENTRY x) {
  PSLL_ENTRY l = a;

  if (l == NULL) {
    x->Flink = NULL;
    return x;
  } else {
    if (x->Data > l->Data) {
      l->Flink = insert(l->Flink, x);
      return l;
    } else {
      x->Flink = l;
      return x;
    }
  }
  return l;
}

void main() {
  PSLL_ENTRY x = NULL, y = NULL;
  x = cons(4, x);
  x = cons(2, x);
  x = cons(1, x);
  print_list(x); printf_s("\n");
  x = insert(x, cons(3, y));
  print_list(x);
}
