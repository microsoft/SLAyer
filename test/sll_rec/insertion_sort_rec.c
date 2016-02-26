/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

PSLL_ENTRY insert(PSLL_ENTRY a, PSLL_ENTRY x) {
  PSLL_ENTRY l = a;

  if (l == NULL) {
    x->Flink = NULL;
    return x;
  } else {
    if (x->Data > l->Data) {
      l->Flink = insert(l->Flink, x);
      return l;
    } else {
      x->Flink = l;
      return x;
    }
  }
  return l;
}

PSLL_ENTRY insertion_sort(PSLL_ENTRY x) {
  PSLL_ENTRY h, ret, cand;
  h = x;
  ret = NULL;
  while (h != NULL) {
    cand = h;
    h = h->Flink;
    cand->Flink = NULL;
    ret = insert(ret, cand);
  }
  return ret;
}

void main() {
  PSLL_ENTRY x = NULL, y = NULL;
  x = cons(4, x);
  x = cons(1, x);
  x = cons(9, x);
  x = cons(5, x);
  x = cons(7, x);
  x = cons(3, x);
  print_list(x); printf_s("\n");
  x = insertion_sort(x);
  print_list(x);
}
