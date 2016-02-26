/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

void filter(PSLL_ENTRY *a, int i) {
  PSLL_ENTRY t, *z = a;

  while(*z != NULL)
    if((*z)->Data == i) {
      t = *z;
      *z = t->Flink;
      free(t);
    } else {
      z = &(*z)->Flink;
    }

  free(t);
}

void main() {
  PSLL_ENTRY x = SLL_create(nondet());

  print_list(x); printf_s("\n");

  filter(&x, 1);

  print_list(x);

  SLL_destroy(x);
}
