/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

PSLL_ENTRY quick_sort(PSLL_ENTRY first, PSLL_ENTRY last) {
  PSLL_ENTRY hd, prev, tl, low;
  if (first == NULL || first == last) return first;
  hd = first;
  prev = first;
  tl = first->Flink;
  while (tl != last) {
    if (tl->Data >= prev->Data) {
      prev->Flink = tl->Flink;
      tl->Flink = hd;
      hd = tl;
      tl = prev->Flink;
    } else {
      prev = tl;
      tl = tl->Flink;
    }
  }
  tl = first->Flink;
  first->Flink = NULL;
  prev = NULL;
  low = quick_sort(hd, first);
  first->Flink = quick_sort(tl, last);
  return low;
}

void main() {
  PSLL_ENTRY x = NULL, y = NULL, z;
  x = cons(4, x);
  x = cons(1, x);
  x = cons(9, x);
  x = cons(5, x);
  x = cons(7, x);
  x = cons(3, x);
  print_list(x); printf_s("\n");
  z = quick_sort(x, NULL);
  print_list(z);
}
