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
  PSLL_ENTRY x, x0, x1, x2, x3, x4, x5, x6, x7;
  x7 = cons(7, NULL);
  x6 = cons(-6, x7);
  x5 = cons(-5, x6);
  x4 = cons(-4, x5);
  x3 = cons(0, x4);
  x2 = cons(3, x3);
  x1 = cons(-2, x2);
  x0 = cons(-1, x1);
  x = x0;
  x = reverse_negative_sublists2(x);
  reverse_negative_sublists(&x);
  free(x7);
  free(x6);
  free(x5);
  free(x4);
  free(x3);
  free(x2);
  free(x1);
  free(x0);
}
