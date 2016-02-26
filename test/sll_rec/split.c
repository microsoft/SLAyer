/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

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

void main() {
  PSLL_ENTRY x = NULL, y;
  x = cons(6, x);
  x = cons(4, x);
  x = cons(2, x);
  x = cons(5, x);
  x = cons(3, x);
  x = cons(1, x);
  print_list(x); printf_s("\n");
  y = split(x);
  print_list(x); printf_s("\n");
  print_list(y);
}
