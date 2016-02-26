/* Copyright (c) Microsoft Corporation.  All rights reserved. */

/**
   Create, attempt to reverse, and then leak a singly-linked list.

   Similar to reverse_div3.c but never advances the cursor pointer.
**/

#include "sll.h"


void reverse(PSLL_ENTRY *l) {
  PSLL_ENTRY c = *l, r = NULL;
  while(c != NULL) {
    PSLL_ENTRY t;
    t = c;
    t->Flink = r;
    r = t;
  }
  *l = r;
}

void main() {
  PSLL_ENTRY head;

  head = SLL_create(nondet());

  reverse(&head);

  SLL_destroy(head);
}
