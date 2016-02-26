/* Copyright (c) Microsoft Corporation.  All rights reserved. */

/**
   Create and destroy a cyclic singly-linked list.

   Works by iteratively unlinking and freeing nodes from the list until it has
   length one, then frees the remaining node.
**/

#include "sll.h"


void CSLL_destroy(PSLL_ENTRY head) {
  PSLL_ENTRY curr, next;
  curr = head->Flink;
  while( head != curr ) {
    next = curr->Flink;
    head->Flink = next;
    free(curr);
    curr = next;
  }
  free(head);
}


void main() {
  PSLL_ENTRY head, tail;
  tail = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY));
  head = SLL_create_seg(nondet(), tail);
  tail->Flink = head;
  CSLL_destroy(head);
}
