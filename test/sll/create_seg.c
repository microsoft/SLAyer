/**
   Copyright (c) Microsoft Corporation.  All rights reserved.

   Create and then leak a singly-linked list segment.
**/

#include "sll.h"


void main(void) {
  PSLL_ENTRY head, tail;

  head = SLL_create_seg(nondet(), tail);
}
