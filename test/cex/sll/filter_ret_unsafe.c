/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  version of filter that does not use sub-object pointers, and hence must
  special-case removing the first link and return the filtered list
*/

#include "sll.h"

/* remove all links with Data a from x */
PSLL_ENTRY filter(PSLL_ENTRY x, int a) {
  PSLL_ENTRY y, z;

  y = x;
  z = NULL;
  while(y != NULL) {
    if(y->Data == a) { /* need to remove y */
      if(y == x) { /* first link */
        free(y);
        x = y->Flink;
        y = x;
      } else { /* not first link */
        z->Flink = y->Flink;
        free(y);
        y = z->Flink;
      }
    } else { /* don't need to remove y */
      z = y;
      y = y->Flink;
    }
  }
  return x;
}

void main() {
  PSLL_ENTRY x = NULL;
  x = cons(1, x);
  x = cons(4, x);
  x = cons(1, x);
  x = cons(3, x);
  x = cons(2, x);
  x = cons(1, x);
  print_list(x); printf_s("\n");
  x = filter(x, 1);
  print_list(x);
  SLL_destroy(x);
}
