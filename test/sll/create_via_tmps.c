/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"


void main(void) {
  PSLL_ENTRY x= NULL;
  int i, len;
  for (i=0; i<len; i++) {
    PSLL_ENTRY tmp;
    tmp = cons(i,x);
    x = tmp;
  }

  SLL_destroy(x);

}
