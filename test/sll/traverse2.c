/**
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Create, traverse, and then destroy a singly-linked list.

  Similar to traverse.c but using a pointer to the Flink field itself instead
  of to the struct containing Flink.
**/

#include "sll.h"


void traverse(PSLL_ENTRY *a) {
  PSLL_ENTRY *z = a;

  while(*z != NULL) {
    z = &(*z)->Flink;
  }
}

void main(void) {
  PSLL_ENTRY head;

  head = SLL_create(nondet());
  traverse(&head);
  SLL_destroy(head);
}
