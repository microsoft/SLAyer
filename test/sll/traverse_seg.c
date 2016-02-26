/**
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Create, traverse, and then destroy a singly-linked list segment.
**/

#include "sll.h"


void traverse_seg(PSLL_ENTRY x, PSLL_ENTRY y) {
  while(x != y) {
    x = x->Flink;
  }
}

void main(void) {
  PSLL_ENTRY head, tail;

  tail = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY));

  head = SLL_create_seg(nondet(), tail);

  traverse_seg(head, tail);

  SLL_destroy_seg(head, tail);
  free(tail);
}
