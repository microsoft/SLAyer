/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

PSLL_ENTRY copy(PSLL_ENTRY a) {
  PSLL_ENTRY y, x = a;
  SLL_ENTRY* * z = &y;

  while(x != NULL) /* listseg(y,*z) * listseg(-,x) * list(x) */ {
     x = x->Flink;
    *z = (SLL_ENTRY*)malloc(sizeof(SLL_ENTRY));
    (*z)->Data = x->Data;
    z = &(*z)->Flink;
    x = x->Flink;
  }
  *z = NULL;
  return y;
}

void main() {
  PSLL_ENTRY x, x1, x2, x3, x4, y;
  x4 = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY)); x3->Flink = NULL;
  x3 = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY)); x3->Flink = x4;
  x2 = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY)); x2->Flink = x3;
  x1 = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY)); x1->Flink = x2;
  x = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY)); x->Flink = x1;
  y = copy(x);
  SLL_destroy(x);
  SLL_destroy(y);
}
