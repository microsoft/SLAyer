/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

void reverse_negative_sublists(PSLL_ENTRY *z) {
  PSLL_ENTRY t, x, y, *w;
  x = *z;
  while(x != NULL) {
    while(x != NULL && x->Data >= 0) {
      z = &(x->Flink);
      x = *z;
    }
    if(x != NULL) {
      y = x;
      w = &(x->Flink);
      do {
        t = x;
        x = x->Flink;
        t->Flink = y;
        y = t;
      }
      while(x != NULL && x->Data < 0);
      *w = x;
      *z = y;
      z = w;
    }
    else {
      *z = NULL;
    }
  }
}

void main() {
  PSLL_ENTRY x = NULL;
  x = SLL_create(nondet());
  reverse_negative_sublists(&x);
  SLL_destroy(x);
}
