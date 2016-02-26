/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Global variable init.
*/

#include "slayer.h"

int x = 0;

void main() {
  int y = x;
  FAIL_IF (y!=0) ;
  return;
}
