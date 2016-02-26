/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

PSLL_ENTRY copy(PSLL_ENTRY x) {
  PSLL_ENTRY y;
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
  y = copy(x);
  SLL_destroy(x);
  SLL_destroy(y);
}
