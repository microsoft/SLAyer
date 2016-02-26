/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

void main() {
  int x, y;

  x++;
  if(x / 2) {
  L0:
    if(x <= 0) goto L3;
    x--;
    goto L1;
  } else {
  L1:
    y++;
    goto L0;
  }
 L3:

  return;
}
