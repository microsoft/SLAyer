/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

void sll_remove(PSLL_ENTRY *l, PSLL_ENTRY x) {
  PSLL_ENTRY t;
  if(*l != NULL) {
    if(*l == x) {
      *l = (*l)->Flink;
      free(x);
    } else {
      t = (*l)->Flink;
      sll_remove(&t, x);
      (*l)->Flink = t;
    }
  }
}

void main() {
  PSLL_ENTRY x = NULL, y = NULL;
  y = cons(3, y);
  y = cons(2, y);
  x = cons(1, y);
  print_list(x); printf_s("\n");
  sll_remove(&x, y);
  print_list(x);
}
