/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "csll.h"


/* remove any entry of the cyclic list containing head whose Data field is fo,
   except head itself */
void CSLL_remove(PSLL_ENTRY head, int fo) {
  PSLL_ENTRY prev, entry, tmp;

  prev = head;
  entry = head->Flink;
  while( entry != head ) {
    if( entry->Data == fo ) {
      /* remove entry */
      tmp = entry->Flink;
      prev->Flink = tmp;
      free(entry);
      entry = tmp;
    } else {
      prev = entry;
      entry = entry->Flink;
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
