/* Copyright (c) Microsoft Corporation.  All rights reserved. */

/*
  Similar to remove, but using a for loop.
 */
#include "csll.h"


void CSLL_remove(PSLL_ENTRY head, int fo) {
  PSLL_ENTRY prev, entry, tmp;
  for( prev = head, entry = head->Flink;
       entry != head;
       prev = entry, entry = entry->Flink ) {
    if( entry->Data == fo ) {
      tmp = entry->Flink;
      prev->Flink = tmp;
      free(entry);
      entry = prev;
    }
  }
}


void main() {
  PSLL_ENTRY head, tail;

  tail = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY));
  head = SLL_create_seg(nondet(), tail);
  tail->Flink = head;

  CSLL_remove(head, 42);

  CSLL_destroy(head);
}
