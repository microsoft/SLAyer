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
      do {
        t = x;
        x = x->Flink;
        t->Flink = y;
        y = t;
      }
      while(x != NULL && x->Data < 0);
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
  reverse_negative_sublists(&x);
  SLL_destroy(x);
}
