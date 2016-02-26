/**
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Create and then leak a singly-linked list of fixed length.
**/

#include "sll.h"

void main() {
  PSLL_ENTRY x = NULL;
  x = cons(4, x);
  x = cons(3, x);
  x = cons(2, x);
  x = cons(1, x);
  x = cons(4, x);
  x = cons(3, x);
  x = cons(2, x);
  x = cons(1, x);
}
