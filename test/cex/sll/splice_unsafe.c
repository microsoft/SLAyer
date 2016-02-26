/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

PSLL_ENTRY splice(PSLL_ENTRY x, PSLL_ENTRY y) {
  PSLL_ENTRY p, q, t;
  p = x;
  q = y;
  if (p == NULL) return q;
  if (q == NULL) return p;
  while (1) {
    q = t->Flink;
    t = q;
    t->Flink = p->Flink;
    p->Flink = t;
    if (p == NULL) return x;
    p = t->Flink;
    if (q == NULL) return x;
    if (p == NULL) {
      t->Flink = q;
      return x;
    }
  }
  if (p == NULL)
    t->Flink = q;
  return x;
}

void main() {
  PSLL_ENTRY x, y, z;
  x = SLL_create(nondet());
  y = SLL_create(nondet());
  z = splice(x, y);
  SLL_destroy(z);
}
