/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

PSLL_ENTRY sll_remove(PSLL_ENTRY a, PSLL_ENTRY x) {
  PSLL_ENTRY l = a;

  if (l != NULL) {
    if (l == x) {
      l = l->Flink;
      free(x);
    } else {
      l->Flink = sll_remove(l->Flink, x);
    }
  }
  return l;
}

void main() {
  PSLL_ENTRY x = NULL, y = NULL;
  y = cons(3, y);
  y = cons(2, y);
  x = cons(1, y);
  print_list(x); printf_s("\n");
  x = sll_remove(x, y);
  print_list(x);
}
