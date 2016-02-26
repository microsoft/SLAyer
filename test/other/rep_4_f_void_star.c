/*  Copyright (c) Microsoft Corporation.  All rights reserved.  */

void * f(int size) {
  return malloc(size);
}

void main(void) {
  void * x, * x1, * x2;

  x = f(8);
  x1 = f(8);
  x1 = f(8);
  x1 = f(8);

  free(x);
}
