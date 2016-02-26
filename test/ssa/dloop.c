/* Copyright (c) Microsoft Corporation.  All rights reserved. */

// Two loops using [head] variable

#include "slayer.h"

void main() {
  int i, j;
  int head = 0;
  int length = nondet();

  for (i=0; i<length; i++) {
    head++;
  }
  j = 0;
  while (j < length) {
    head--;
  }
}
