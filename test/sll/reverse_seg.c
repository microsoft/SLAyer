/**
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Create, reverse, and then destroy a singly-linked list segment.
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

  tail = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY));

  head = SLL_create_seg(length, tail);

  reverse_seg(&head, tail);

  SLL_destroy_seg(head, tail);
  free(tail);
}
