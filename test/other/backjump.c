/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Structured gotos.
 */

#include "slayer.h"

void main() {
  int x, y;
 L: y = 0;
  if(y==0) return;
  FAIL ;
  x = 0;
  goto L;
}
