/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

void insert(PSLL_ENTRY *l, PSLL_ENTRY x) {
  PSLL_ENTRY u;
  if(*l == NULL) {
    x->Flink = NULL;
    *l = x;
  } else {
    if(x->Data > (*l)->Data) {
      u = (*l)->Flink;
      insert(&u, x);
      (*l)->Flink = u;
    } else {
      x->Flink = *l;
      *l = x;
    }
  }
}

void main() {
  PSLL_ENTRY x = NULL, y = NULL;
  x = cons(4, x);
  x = cons(2, x);
  x = cons(1, x);
  print_list(x); printf_s("\n");
  insert(&x, cons(3, y));
  print_list(x);
}
