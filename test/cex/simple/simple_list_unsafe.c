/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

typedef struct _SLL_ENTRY {
  int Data;
  struct _SLL_ENTRY *Flink;
} SLL_ENTRY, *PSLL_ENTRY;

int main()
{
  int i;
  SLL_ENTRY* list = NULL;
  for(i = 0; i < 10; i++) {
    SLL_ENTRY* tmp = (SLL_ENTRY*)malloc(sizeof(SLL_ENTRY));
    tmp->Flink = list;
    list = tmp;
  }

  while(nondet()) {
    list = list->Flink;
  }
  return 0;
}
