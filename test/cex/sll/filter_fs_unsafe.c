/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

void filter(PSLL_ENTRY *a, int i) {
  PSLL_ENTRY t, *z = a;

  while(*z != NULL)
    if((*z)->Data == i) {
      t = *z;
      free(t);
      *z = t->Flink;
    } else {
      z = &(*z)->Flink;
    }
}

void main() {
  PSLL_ENTRY x, x1, x2, x3, x4, x5;
  x5 = cons(1, NULL);
  x4 = cons(4, x5);
  x3 = cons(1, x4);
  x2 = cons(3, x3);
  x1 = cons(2, x2);
  x = cons(1, x1);
  print_list(x); printf_s("\n");
  filter(&x, 1);
  print_list(x);
  SLL_destroy(x);
}
