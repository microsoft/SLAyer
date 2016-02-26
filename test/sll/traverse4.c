/**
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Create, traverse, and then destroy a singly-linked list.

  Similar to traverse3.c but the length variable is global.
**/

#include "sll.h"


void traverse(PSLL_ENTRY head) {
  PSLL_ENTRY tmp = head;

  while(tmp != NULL) {
    tmp = tmp->Flink ;
  }
}

int length;

void main(void) {
  PSLL_ENTRY head;

  if (length == 1000) {
    head = SLL_create(length);
    traverse(head);
    SLL_destroy(head);
  }
}
