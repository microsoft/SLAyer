/**
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Create, traverse, and then destroy a singly-linked list.

  Similar to traverse.c but only conditionally performs the creation and
  traversal.
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
  int length;

  if (length == 1000) {
    head = SLL_create(length);
    traverse(head);
    SLL_destroy(head);
  }
}
