/* Copyright (c) Microsoft Corporation.  All rights reserved. */

/* like create_rec.c but using length <= 0 instead of 0 <= length */

#include "sll.h"

PSLL_ENTRY create(int length) {
  PSLL_ENTRY head, tmp;
 
  if (length <= 0) {
    head = NULL;
  } else {
    tmp = create(length - 1);
    head = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY));
    head->Flink = tmp->Flink;
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
