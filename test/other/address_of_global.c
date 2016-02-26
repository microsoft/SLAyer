/**
  Copyright (c) Microsoft Corporation.  All rights reserved.

  y is an alias of the global x.
 **/

int x;

void main() {
  int *y = &x;
  *y = 0;
  assert(x==0);
  return;
}
