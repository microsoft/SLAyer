/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Access local/global through fun call.
*/

#include "slayer.h"

int g;

void f(int a, int *z) {
  int y = a;

  FAIL_IF( y!=*z );
  *z = g;
  y = 13;
  return;
}

void main() {
  int x = 0;
  f(0, &x);
  FAIL_IF( x!=g );
  return;
}
