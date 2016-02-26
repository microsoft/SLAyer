/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

typedef struct _SLL_ENTRY cell, *list, *listseg;

list list_reverse_rec_aux(list x, list y) {
  if(y == NULL) {
    return x;
  } else {
    list tmp = y->Flink;
    y->Flink = x;
    return list_reverse_rec_aux(y, tmp);
  }
}

void list_reverse(list *z) {
  *z = list_reverse_rec_aux(NULL, *z);
}

int main() {
  list x;
  x = cons(1, cons(2, cons(3, NULL)));
  list_reverse(&x);
  print_list(x); printf_s("\n");
}
