/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

void main() {
  int x = 0, y = 0;
  if(x==0) {
    FAIL_IF(y != x);
    return;
  }
  FAIL ;
  return;
}
