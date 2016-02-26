/**
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Create, attempt to reverse, and then destroy a singly-linked list.

  Similar to reverse.c but sometimes breaks the list, gets lost, and
  diverges.  All executions that break the list must diverge in
  reverse, so this program is still safe.
**/

#include "sll.h"


void reverse(PSLL_ENTRY *l) {
  PSLL_ENTRY c = *l, r = NULL;
  while(c != NULL) {
    PSLL_ENTRY t;
    t = c;
    if (c->Data != 5)
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
