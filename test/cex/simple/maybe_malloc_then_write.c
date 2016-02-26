/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

/* Result: POSSIBLY UNSAFE *\
\* Expected Result: UNSAFE */

/* JEK: a and b are still irrelevant, i.e. sliceable, here*/

void main()
{
  int* x;
  int a;

  if (nondet()) {
    x = malloc(sizeof(int));
  }

  while (nondet()) {
    *x = a;
  }

}
