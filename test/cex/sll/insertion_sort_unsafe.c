/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

PSLL_ENTRY insert(PSLL_ENTRY l, PSLL_ENTRY x) {
  PSLL_ENTRY elem, prev;
  elem = l;
  prev = NULL;
  while (elem != NULL) {
    if (elem->Data >= x->Data) {
      x->Flink = elem;
      if (prev == NULL) return x;
      prev->Flink = x;
      return l;
    }
    prev = elem;
    elem = elem->Flink;
  }
  x->Flink = elem;
  if (prev == NULL) return x;
  prev->Flink = x;
  return l;
}

PSLL_ENTRY insertion_sort(PSLL_ENTRY x) {
  PSLL_ENTRY h, ret, cand;
  h = x;
  ret = NULL;
  while (h != NULL) {
    h = h->Flink;
    cand = h;
    cand->Flink = NULL;
    ret = insert(ret, cand);
  }
  return ret;
}

void main() {
  PSLL_ENTRY x;
  x = SLL_create(17);
  x = insertion_sort(x);
  SLL_destroy(x);
}
