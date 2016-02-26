/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

typedef struct _SLL_ENTRY {
  int Data;
  struct _SLL_ENTRY *Flink;
} SLL_ENTRY, *PSLL_ENTRY;

int main() {
  int i;
  int j = nondet()%3+5;
  SLL_ENTRY* list = NULL;
  while(nondet() || j) {
    SLL_ENTRY* tmp = (SLL_ENTRY*)malloc(sizeof(SLL_ENTRY));
    tmp->Flink = list;
    list = tmp;
    j--;
  }

  j = nondet()%5+5;
  
  for(i = 0; i < j; i++) {
    list = list->Flink;
  }
  return 0;
}
