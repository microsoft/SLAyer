/* Copyright (c) Microsoft Corporation.  All rights reserved. */

/* like create_rec.c but allocate the new link before making the recursive
   call, so that the frame is unbounded */

#include "sll.h"

PSLL_ENTRY create(int length) {
  PSLL_ENTRY head;
 
  if (0 <= length) {
    head = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY));
    head->Flink->Flink = create(length - 1);
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
