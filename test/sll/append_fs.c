/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

void append(PSLL_ENTRY *a, PSLL_ENTRY y) {
  PSLL_ENTRY *z = a;

  while(*z != NULL) {
    z = &(*z)->Flink;
  }
  *z = y;
}

void main() {
  PSLL_ENTRY x, x1, x2, x3, y, y1, y2;
  x3 = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY)); x3->Flink = NULL;
  x2 = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY)); x2->Flink = x3;
  x1 = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY)); x1->Flink = x2;
  x = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY)); x->Flink = x1;
  y2 = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY)); y2->Flink = NULL;
  y1 = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY)); y1->Flink = y2;
  y = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY)); y->Flink = y1;
  append(&x, y);
  SLL_destroy(x);
}
