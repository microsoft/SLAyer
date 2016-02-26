/*  Copyright (c) Microsoft Corporation.  All rights reserved.  */

#include <assert.h>

int f();

static int g() { return 2; }

void main() {
  int x;
  int y;

  x = f();
  y = g();

  assert(x == 0);		/* succeeds if f from extern.c called */
  assert(y == 2);		/* succeeds if g from main.c called */
}
