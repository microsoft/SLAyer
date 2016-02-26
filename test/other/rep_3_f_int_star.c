/*  Copyright (c) Microsoft Corporation.  All rights reserved.  */

int * f() {
  return malloc(sizeof(int));
}

void main(void) {
  int * x, * x1, * x2;

  x = f();
  x1 = f();
  x1 = f();

  free(x);
}
