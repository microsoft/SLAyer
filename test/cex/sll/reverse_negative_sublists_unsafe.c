/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

void reverse_negative_sublists(PSLL_ENTRY *a) /* o==z | list(*z) */ {
  PSLL_ENTRY *z = a;

  PSLL_ENTRY t, x = *z, y, *w;
  while(x != NULL) /* *z==x | listseg(*o,*z) * list(x) */ {
    while(x != NULL && x->Data >= 0) /* listseg(*o,*z) * list(x) */ {
      z = &(x->Flink);
      x = *z;
    }
    if(x != NULL) {
      /* listseg(*o,*z) * x|->x' * list(x') */
      y = x;
      w = &(x->Flink);
      /* y==x /| *w==x' | listseg(*o,*z) * x|->-,x' * list(x') */
      while(x != NULL && x->Data < 0) /* listseg(y,*w) * list(x) */ {
        t = x;
        x = x->Flink;
        t->Flink = y;
        y = t;
      }
      /* listseg(*o,*z) * listseg(y,*w) * list(x) */
      *w = x;
      /* listseg(*o,*z) * listseg(y,x) * list(x) */
      *z = y;
      /* listseg(*o,x) * list(x) */
    } else {
      *z = NULL;
    }
  }
} /* list(*o) */

/* a version that doesn't use pointers into records, call-by-reference, or
   conditional assertions */
PSLL_ENTRY reverse_negative_sublists2(PSLL_ENTRY a) /* o==x | list(x) */ {
  PSLL_ENTRY x = a;

  PSLL_ENTRY t, y, w, v=cons(0,x), z=v;
  while(x != NULL) /* listseg(o,z) * z|->-,x * list(x) */ {
    while(x != NULL && x->Data >= 0) /* listseg(o,z) * z|->-,x * list(x) */ {
      z = x;
      x = x->Flink;
    }
    if(x != NULL) {
      /* listseg(o,z) * z|->-,x * x|->x' * list(x') */
      y = w = x;
      /* y==w==x | listseg(o,z) * z|->-,w * w|->-,x' * list(x') */
      do {
        x = x->Flink;
        t = x;
        t->Flink = y;
        y = t;
      } while(x != NULL && x->Data < 0); /* listseg(y,w) * w|->- * list(x) */
      /* listseg(o,z) * z|->-,w * listseg(y,w) * w|->- * list(x) */
      w->Flink = x;
      /* listseg(o,z) * z|->-,w * listseg(y,w) * w|->-,x * list(x) */
      z->Flink = y;
      /* listseg(o,z) * z|->-,y * listseg(y,w) * w|->-,x * list(x) */
      /* listseg(o,w) * w|->-,x * list(x) */
      /* the following assignment is operationally irrelevant, but we can't
         express the following stronger invariant for the outer loop:
         (x != NULL && x->Data >= 0 ? listseg(o,x) : listseg(o,z) * z|->-,x)
         * list(x) */
      z = w;
      /* listseg(o,z) * z|->-,x * list(x) */
      /* listseg(o,x) * list(x) */
    } else {
      z->Flink = NULL;
    }
  }
  t = v->Flink;
  free(v);
  return t;
} /* list(o) */

void main() {
  PSLL_ENTRY x = NULL;
  x = SLL_create(nondet());
  x = reverse_negative_sublists2(x);
  reverse_negative_sublists(&x);
  SLL_destroy(x);
}
