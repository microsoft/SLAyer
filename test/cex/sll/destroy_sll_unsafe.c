/**
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Create and destroy a singly-linked list segment.
**/

#include "sll.h"


void main() {
  PSLL_ENTRY head, tail;
  tail = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY));
  head = SLL_create_seg(nondet(), tail);
  SLL_destroy_seg(head, tail);
  free(tail);
  free(head);
}
