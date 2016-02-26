/**
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Create, reverse, and then destroy a singly-linked list.
**/

#include "sll.h"


/*
  Reverse the list pointed to by l.
  Implemented by poping off each item of *l into r.
*/
void reverse(PSLL_ENTRY *l) {
  PSLL_ENTRY c = *l, r = NULL;
  while(c != NULL) {
    PSLL_ENTRY t;
    t = c;
    c = c->Flink;
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
