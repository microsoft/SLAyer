/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

PSLL_ENTRY create(int length) {
  int i;
  PSLL_ENTRY head, tmp;

  head = NULL;
  for(i = 0; i < length; i++) {
    tmp = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY));
    tmp->Flink = head;
    head = tmp;
  }

  return head;
}

void traverse(PSLL_ENTRY x) {
  if(x != NULL) {
    traverse(x->Flink);
  }
}

void main(void) {
  int length;
  PSLL_ENTRY head;

  length = length % 100;
  head = create(length);

  traverse(head);
/*   traverse(head); */

  return;
}
