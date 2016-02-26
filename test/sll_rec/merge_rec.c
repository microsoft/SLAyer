/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

PSLL_ENTRY merge(PSLL_ENTRY p, PSLL_ENTRY q) {
  PSLL_ENTRY r, t;

  if (p == NULL) return q;
  if (q == NULL) return p;
  if (p->Data <= q->Data) {
    r = p->Flink;
    t = merge(r, q);
    p->Flink = t;
    return p;
  } else {
    r = q->Flink;
    t = merge(p, r);
    q->Flink = t;
    return q;
  }
 retn:
  return r;
}

void main() {
  PSLL_ENTRY x = NULL, y = NULL, z;
  x = cons(5, x);
  x = cons(3, x);
  x = cons(1, x);
  y = cons(6, y);
  y = cons(4, y);
  y = cons(2, y);
  print_list(x); printf_s("\n");
  print_list(y); printf_s("\n");
  z = merge(x, y);
  print_list(z);
}
