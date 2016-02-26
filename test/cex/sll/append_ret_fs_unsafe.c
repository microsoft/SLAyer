/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

PSLL_ENTRY append(PSLL_ENTRY x, PSLL_ENTRY y) {
  PSLL_ENTRY t, n, r;
  if (x != NULL) {
    t = x;
    n = x->Flink;
    while (n != NULL) {
      n = n->Flink;
      t = n;
    }
    t->Flink = y;
    return x;
  } else {
    return y;
  }
}

void main() {
  PSLL_ENTRY x, x1, x2, x3, y, y1, y2;
  x3 = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY)); x3->Flink = NULL;
  x2 = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY)); x2->Flink = x3;
  x1 = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY)); x1->Flink = x2;
  x = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY)); x->Flink = x1;
  free(x3->Flink);
  y2 = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY)); y2->Flink = NULL;
  y1 = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY)); y1->Flink = y2;
  y = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY)); y->Flink = y1;
  x = append(x, y);
  SLL_destroy(x);
}
