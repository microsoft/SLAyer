/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"

void main() {
  PSLL_ENTRY x = NULL;
  PSLL_ENTRY tmp1, tmp2, tmp3, tmp4, tmp5;
  tmp1 = cons(1, x);
  x = tmp1;
  tmp2 = cons(2, x);
  x = tmp2;
  tmp3 = cons(3, x);
  x = tmp3;
  tmp4 = cons(4, x);
  x = tmp4;
  // ls(x,0) doesn't hold here.
  SLL_destroy(x);
}
