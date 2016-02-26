/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

void main() {
  PSLL_ENTRY x, x1, x2, x3;
  x3 = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY)); x3->Flink = NULL;
  x2 = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY)); x2->Flink = x3;
  x1 = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY)); x1->Flink = x2;
  x = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY)); x->Flink = x1;
  // ls(x,0) holds here.
  SLL_destroy(x);
}
