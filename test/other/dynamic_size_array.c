/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

void main() {
  int n;
  int* a;
  int i;

  n = n % 16;
  a = malloc(n * sizeof(int));

  for(i=0; i<n; i++) {
    a[i] = i;
  }

  free(a);

  return;
}
