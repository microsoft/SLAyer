/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

/* Result: POSSIBLY UNSAFE *\
\* Expected Result: SAFE   */


typedef struct _SLL_ENTRY {
  int Data;
  struct _SLL_ENTRY *Flink;
} SLL_ENTRY, *PSLL_ENTRY;


PSLL_ENTRY cons(int a, SLL_ENTRY* d) {
  PSLL_ENTRY x = (SLL_ENTRY*)malloc(sizeof(SLL_ENTRY));
  x->Data = a;
  x->Flink = d;
  return x;
}

int main()
{
  int i;
  int *target = NULL;
  SLL_ENTRY* list = NULL;

  for(i = 0; i < 5; ++i) {
    int input = nondet();
    if (input != 3) {
      list = cons(input, list);
    }
  }
  
  while(list) {
	SLL_ENTRY* tmp;
    if(list->Data != 3) {
      target = (int*)malloc(sizeof(int));
    }

    *target = 15;

    if(target) {
      free(target);
      target = NULL;
    }
	tmp = list;
	list = list->Flink;
	free(tmp);
  }
}
