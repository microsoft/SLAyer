/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

int a;

void main() {
  int x;
  x = &a;
  assert((int*)x == &a);
  return;
}
