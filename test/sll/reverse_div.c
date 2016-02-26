/**
  Copyright (c) Microsoft Corporation.  All rights reserved.
  Create, reverse, and then destroy a singly-linked list.

  Similar to reverse.c but may nondeterministically do nothing during
  reversal, thereby causing divergence.
**/

#include "sll.h"


void reverse_div(PSLL_ENTRY *l) {
  PSLL_ENTRY c = *l, r = NULL;
  while(c != NULL) {
    if (nondet()) { /* nondeterministically do nothing */
      PSLL_ENTRY t;
      t = c;
      c = c->Flink;
      t->Flink = r;
      r = t;
    }
  }
  *l = r;
}


void main() {
  PSLL_ENTRY head;

  head = SLL_create(nondet());

  reverse_div(&head);

  SLL_destroy(head);
}
