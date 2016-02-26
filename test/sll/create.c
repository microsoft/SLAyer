/**
   Copyright (c) Microsoft Corporation.  All rights reserved.

   Create and then leak a singly-linked list.
**/

#include "sll.h"


void main(void) {
  PSLL_ENTRY head;

  head = SLL_create(nondet());
}
