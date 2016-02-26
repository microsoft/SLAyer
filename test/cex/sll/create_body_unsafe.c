/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

PSLL_ENTRY create(int length) {
  int i;
  PSLL_ENTRY head, tmp;

  head = NULL;
  tmp = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY));
  tmp->Flink = head;
  head = head->Flink;
  printf_s("created link\n") ;

  return head;
}

void main(void) {
  PSLL_ENTRY x;

  x = create(1);

  free(x);
}
