/**
  Copyright (c) Microsoft Corporation.  All rights reserved.

  y is an alias of &x.
 **/

#include "slayer.h"

void main() {
  int x;
  int *y = &x;
  *y = 0;
  FAIL_IF(x != 0) ;
  return;
}
