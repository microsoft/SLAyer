/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

int a;

int* f(int* x) {
  return x;
}

void main() {
  int* x;
  x = f(&a);
  assert(x == &a);
}
