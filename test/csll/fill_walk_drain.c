/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

typedef struct _SLL_ENTRY {
  int Data;
  struct _SLL_ENTRY *Flink;
} SLL_ENTRY, *PSLL_ENTRY;

PSLL_ENTRY newEntry() { return malloc(sizeof(SLL_ENTRY)); }

void fill(PSLL_ENTRY head) {
  while (nondet()) {
    PSLL_ENTRY entry = newEntry();
    entry->Flink = head->Flink;
    head->Flink = entry;
  }
}

void walk(PSLL_ENTRY head) {
  PSLL_ENTRY entry = head->Flink;
  while (entry != head) {
    entry = entry->Flink;
  }
}

void drain(PSLL_ENTRY head) {
  PSLL_ENTRY entry = head->Flink;
  while (entry != head) {
    PSLL_ENTRY next = entry->Flink;
    free(entry);
    entry = next;
  }
}

void main() {
  PSLL_ENTRY head = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY));
  head->Flink = head;
  fill(head);
  walk(head);
  drain(head);
  free(head);
}
