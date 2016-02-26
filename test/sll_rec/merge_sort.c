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

PSLL_ENTRY split(PSLL_ENTRY p) {
  PSLL_ENTRY t1, t3;
  if (p == NULL) return NULL;
  t1 = p->Flink;
  if (t1 == NULL) return NULL;
  t3 = split(t1->Flink);
  p->Flink = t1->Flink;
  t1->Flink = t3;
  return t1;
}

PSLL_ENTRY merge_sort(PSLL_ENTRY p) {
  PSLL_ENTRY q, r;

  if (p == NULL) return NULL;
  if (p->Flink == NULL) return p;
  q = split(p);
  q = merge_sort(q);
  p = merge_sort(p);
  r = merge(p, q);
  return r;
}

void main() {
  PSLL_ENTRY x = NULL, y = NULL;
  x = cons(4, x);
  x = cons(1, x);
  x = cons(9, x);
  x = cons(5, x);
  x = cons(7, x);
  x = cons(3, x);
  print_list(x); printf_s("\n");
  x = merge_sort(x);
  print_list(x);
}
