/**
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Create, reverse, and then destroy a singly-linked list.

  Similar to reverse_div2.c but is also unsafe due to an omitted NULL test.
**/

#include "sll.h"


void reverse_div(PSLL_ENTRY *l) {
  PSLL_ENTRY c = *l, r = NULL;
  while(c != NULL) {
    PSLL_ENTRY t;
    t = c;
    c = c->Flink;
    t->Flink = r;
    r = t;
    if (nondet() /* && z!=NULL */) {
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
