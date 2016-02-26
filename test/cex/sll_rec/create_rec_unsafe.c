/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

PSLL_ENTRY create(int length) {
  PSLL_ENTRY head, tmp;
 
  if (0 <= length) {
    tmp = create(length - 1);
    //    head = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY));
    head->Flink = tmp;
  } else {
    head = NULL;
  }
  return head;
}

void main(void) {
  int length;
  PSLL_ENTRY head, tmp;
 
  length = length % 100;
  tmp = head = create(length);

  print_list(head);
}
