/* Copyright (c) Microsoft Corporation.  All rights reserved. */

/**
   Create and destroy a cyclic singly-linked list.

   Works by freeing the first node, followed by all others until seeing a
   pointer back to the first one.  Since this performs a test against a
   dangling pointer, it has unspecified behavior according to section 6.2.4.2
   of the ISO C99 standard.
**/

#include "sll.h"


void destroy(PSLL_ENTRY x) {
  PSLL_ENTRY h = x, c;
  do {
    c = x;
    x = x->Flink;
    free(c);
  } while(x != h);
}


void main() {
  PSLL_ENTRY head, tail;
  tail = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY));
  head = SLL_create_seg(nondet(), tail);
  //  tail->Flink = head;
  destroy(head);
}
