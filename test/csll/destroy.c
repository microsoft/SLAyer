/* Copyright (c) Microsoft Corporation.  All rights reserved. */

/**
   Create and destroy a cyclic singly-linked list.

   Works by remembering the first node in the list, freeing all others, then
   freeing the first one.
**/

#include "csll.h"


void main() {
  PSLL_ENTRY head, tail;
  tail = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY));
  head = SLL_create_seg(nondet(), tail);
  tail->Flink = head;
  CSLL_destroy(head);
}
