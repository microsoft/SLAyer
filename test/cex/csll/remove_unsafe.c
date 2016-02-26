/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

PSLL_ENTRY find(PSLL_ENTRY head, int fo) {
  PSLL_ENTRY prev, entry, tmp;

  prev = head;
  entry = head->Flink;
  while(entry != head) {
    if (entry->Data == fo) {
      /* remove entry */
      tmp = entry->Flink;
      prev->Flink = tmp;
      if (nondet()) {
	return entry;
      } else {
	free(entry);
      }
    } else {
      prev = entry;
    }
    entry = entry->Flink;
  }

  return NULL;
}

void main() {
  PSLL_ENTRY head, tail, mark;
  int i;
  PSLL_ENTRY tmp;

  tail = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY));

  head = tail;
  for(i = 0; i < 4; i++) {
    tmp = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY));
    tmp->Flink = head;
    head = tmp;
  }

  tail->Flink = head;

  find(head, 42);

}
