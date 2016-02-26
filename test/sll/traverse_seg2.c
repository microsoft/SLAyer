/**
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Create, traverse, and then destroy a singly-linked list segment.

  Similar to traverse_seg.c but the segment might by cyclic or a lasso.
**/

#include "sll.h"


void traverse_seg(PSLL_ENTRY x, PSLL_ENTRY y) {
  while(x != y) {
    x = x->Flink;
  }
}

void main(void) {
  PSLL_ENTRY head, tail;

  head = SLL_create_seg(nondet(), tail);

  traverse_seg(head, tail);

  SLL_destroy_seg(head, tail);
}
