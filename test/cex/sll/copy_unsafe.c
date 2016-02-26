/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

PSLL_ENTRY copy(PSLL_ENTRY a) {
  PSLL_ENTRY y, x = a;
  SLL_ENTRY* * z = &y;

  while(x != NULL) /* listseg(y,*z) * listseg(-,x) * list(x) */ {
    *z = (SLL_ENTRY*)malloc(sizeof(SLL_ENTRY));
    (*z)->Data = x->Data;
    z = &(*z)->Flink;
    x = x->Flink;
  }
  *z = NULL;
  return y;
}

void main() {
  PSLL_ENTRY x = NULL, y = NULL;
  x = SLL_create(nondet());
  SLL_destroy(x);
  y = copy(x);
}
