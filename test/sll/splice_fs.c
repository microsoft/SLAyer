/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

PSLL_ENTRY splice(PSLL_ENTRY x, PSLL_ENTRY y) {
  PSLL_ENTRY p, q, t;
  p = x;
  q = y;
  if (p == NULL) return q;
  if (q == NULL) return p;
  while (1) {
    t = q;
    q = t->Flink;
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
  PSLL_ENTRY x, x1, x2, y, y1, y2, y3, z;
  x2 = cons(3, NULL);
  x1 = cons(2, x2);
  x = cons(1, x1);
  y3 = cons(7, NULL);
  y2 = cons(6, y3);
  y1 = cons(5, y2);
  y = cons(4, y1);
  z = splice(x, y);
  SLL_destroy(z);
}
