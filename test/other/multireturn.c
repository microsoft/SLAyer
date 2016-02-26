/* Copyright (c) Microsoft Corporation.  All rights reserved. */

int f() {
  int x, y;
  x = 0;
  if(x==y) return 0;
  x = 4;
  y = x;
  return 1;
}

int main() {
  f();
}
