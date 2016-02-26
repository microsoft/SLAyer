/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  We have a fun call convention to have unique actuals.
*/

void f(int a, int b) {
  int y = a + b;
  return;
}

void main() {
  int x = 0;
  int y = 1;

  f(x,x); // Should become f(x1,x2)
  f(x,y); // Should become f(x3,y1)
  return;
}
