/**
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Create, reverse, and then leak a singly-linked list segment.

  Similar to reverse_seg.c but the segment might be cyclic or a lasso.
**/

#include "sll.h"


void reverse_seg(PSLL_ENTRY *z, SLL_ENTRY *w) {
  PSLL_ENTRY t, x = *z, y = w;
  while(x != w) {
    t = x;
    x = x->Flink;
    t->Flink = y;
    y = t;
  }
  *z = y;
}

void main() {
  int length;
  PSLL_ENTRY head, tail;

  head = SLL_create_seg(length, tail);
  head = NULL;
  reverse_seg(&head, tail);
}
