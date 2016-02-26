/**
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Create, traverse, and then destroy a singly-linked list.
**/

#include "sll.h"


void traverse(PSLL_ENTRY head) {
  while(head != NULL) {
    head = head->Flink ;
  }
}

void main(void) {
  PSLL_ENTRY head;

  head = SLL_create(nondet());
  traverse(head);
  SLL_destroy(head);
}
