/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

/* a version that doesn't use pointers into records, call-by-reference, or
   conditional assertions */
PSLL_ENTRY reverse_negative_sublists2(PSLL_ENTRY x) {
  PSLL_ENTRY t, y, w, v, z;
  v = cons(0,x);
  z = v;
  while(x != NULL) {
    while(x != NULL && x->Data >= 0) {
      z = x;
      x = x->Flink;
    }
    if(x != NULL) {
      y = w = x;
      while(x != NULL && x->Data < 0) {
        t = x;
        x = x->Flink;
        t->Flink = y;
        y = t;
      }
      w->Flink = x;
      z->Flink = y;
      z = w;
    }
    else {
      z->Flink = NULL;
    }
  }
  t = v->Flink;
  free(v);
  return t;
}

void main() {
  PSLL_ENTRY x = NULL;
  x = SLL_create(nondet());
  x = reverse_negative_sublists2(x);
  SLL_destroy(x);
}
