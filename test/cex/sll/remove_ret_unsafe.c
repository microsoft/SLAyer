/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

PSLL_ENTRY sll_remove(PSLL_ENTRY l, PSLL_ENTRY x) {
  PSLL_ENTRY elem, prev, t;
  elem = l;
  prev = NULL;
  while (elem != NULL) {
    if (elem == x) {
      if (prev == NULL) {
        t = elem->Flink;
        free(elem);
        return t;
      } else {
        free(elem);
        t = elem->Flink;
        prev->Flink = t;
      }
      return l;
    }
    prev = elem;
    elem = elem->Flink;
  }
  return l;
}

void main() {
  PSLL_ENTRY x = NULL, y = NULL;
  y = cons(3, y);
  y = cons(2, y);
  x = cons(1, y);
  x = sll_remove(x, y);
  SLL_destroy(x);
}
