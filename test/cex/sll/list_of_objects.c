/**
   Copyright (c) Microsoft Corporation.  All rights reserved.

   Create and destroy a singly-linked list of pointers to heap objects.
**/

#include "slayer.h"


typedef struct _SLL_ENTRY {
  void* Data;
  struct _SLL_ENTRY *Flink;
} SLL_ENTRY, *PSLL_ENTRY;


void main(void) {
  PSLL_ENTRY head, item;

  head = NULL;
  while (nondet()) {
    item = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY));
    item->Data = (int*)malloc(sizeof(int));
    item->Flink = head;
    head = item;
  }

  while (head) {
    item = head;
    head = item->Flink;
    free(item->Data);
    free(item);
  }
}
