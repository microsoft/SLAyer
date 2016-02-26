/**
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Create and destroy a singly-linked list segment.
  Similar to destroy_seg except that the tail cell is free'd separately.
**/

#include "sll.h"


void main() {
  PSLL_ENTRY head, tail;
  tail = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY));
  head = SLL_create_seg(nondet(), tail);
  if( head == tail->Flink ) {
    free(head);
  } else {
    SLL_destroy_seg(head, tail->Flink);
  }
  /* tail->Flink may have been in the list, so this may leak a cycle */
}
