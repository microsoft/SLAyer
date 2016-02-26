/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

/* Result: POSSIBLY UNSAFE *\
\* Expected Result: UNSAFE */

/* JEK: a and b are still irrelevant, i.e. sliceable, here*/

int main()
{
  int* x;
  int a = 2;
  int* b;
  b = malloc(sizeof(int)*a);

  if (nondet()) {
      x = malloc(sizeof(int)*a);
    }

  *x = 3;
  return 0;
}
