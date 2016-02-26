/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

/* JEK: a, b and c should be irrelevant, i.e. sliceable, here
       interesting: a is actually connected to j - can we still slice it? */

typedef struct _SLL_ENTRY {
  int Data;
  struct _SLL_ENTRY *Flink;
} SLL_ENTRY, *PSLL_ENTRY;

int main() {
  int i;
  int j = nondet()%3+5;
  SLL_ENTRY* list = NULL;
  int a;
  int* b;
  SLL_ENTRY* c = NULL;
  while(nondet() || j) {
    SLL_ENTRY* tmp = (SLL_ENTRY*)malloc(sizeof(SLL_ENTRY));
    tmp->Flink = list;
    list = tmp;
    j--;
  }
  
  for(a = 0; a < j; a++) {
    SLL_ENTRY* tmp = (SLL_ENTRY*)malloc(sizeof(SLL_ENTRY));
    tmp->Flink = c;
    c = tmp;
  }

  j = nondet()%5+5;
  
  while( c != NULL) {
    SLL_ENTRY* tmp = c ;
    c = c->Flink ;
    free(tmp);
  }
    
  for(i = 0; i < j; i++) {
    list = list->Flink;
  }
  return 0;
}
