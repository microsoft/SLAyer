/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

PSLL_ENTRY insert(PSLL_ENTRY l, PSLL_ENTRY x) {
  PSLL_ENTRY elem, prev;
  elem = l;
  prev = NULL;
  while (elem != NULL) {
    if (elem->Data >= x->Data) {
      x->Flink = elem;
      if (prev == NULL) return x;
      prev->Flink = x;
      return l;
    }
    prev = elem;
    elem = elem->Flink;
  }
  x->Flink = elem;
  if (prev == NULL) return x;
  prev->Flink = x;
  return l;
}

void main() {
  PSLL_ENTRY x = NULL, y = NULL;
  x = cons(4, x);
  x = cons(2, x);
  x = cons(1, x);
  x = insert(x, cons(3, y));
  SLL_destroy(x);
}
