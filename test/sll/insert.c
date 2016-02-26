/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

void insert(PSLL_ENTRY *a, PSLL_ENTRY x) {
  PSLL_ENTRY *l = a;

  while(*l != NULL && (*l)->Data < x->Data) {
    l = &(*l)->Flink;
  }
  x->Flink = *l;
  *l = x;
}

void main() {
  PSLL_ENTRY x = NULL, y = NULL;
  x = cons(4, x);
  x = cons(2, x);
  x = cons(1, x);
  insert(&x, cons(3, y));
  SLL_destroy(x);
}
