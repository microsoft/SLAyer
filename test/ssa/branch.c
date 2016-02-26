/* Copyright (c) Microsoft Corporation.  All rights reserved. */

// simple branch

#include "slayer.h"

void main() {
  int x;

  x = 0;
  if (nondet()) {
    x = x + 1;
  } else {
    x = x + 2;
  }
  x = x + 3;

}
