/**
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Create, traverse, and then destroy a singly-linked list.

  Similar to traverse.c but using a temporary variable.
**/

#include "sll.h"


void traverse(PSLL_ENTRY head) {
  PSLL_ENTRY tmp = head;

  while(tmp != NULL) {
    tmp = tmp->Flink ;
  }
}

void main(void) {
  PSLL_ENTRY head;

  head = SLL_create(nondet());
  traverse(head);
  SLL_destroy(head);
}
