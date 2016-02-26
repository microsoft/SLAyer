/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

PSLL_ENTRY merge(PSLL_ENTRY p, PSLL_ENTRY q) {
  PSLL_ENTRY r, t, t2, p2, q2;
  if (p == NULL) return q;
  if (q == NULL) return p;
  if (p->Data <= q->Data) {
    t = q;
    q2 = q->Flink;
    p2 = p;
  } else {
    t = p;
    p2 = p->Flink;
    q2 = q;
  }
  t2 = merge(p2,q2);
  t->Flink = t2;
  return t;
}

void main() {
  PSLL_ENTRY x = NULL, y = NULL, z;
  x = cons(5, x);
  x = cons(3, x);
  x = cons(1, x);
  y = cons(6, y);
  y = cons(4, y);
  y = cons(2, y);
  z = merge(x, y);
  print_list(z);
}
