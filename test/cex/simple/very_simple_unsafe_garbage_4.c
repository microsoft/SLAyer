/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

/* Result: POSSIBLY UNSAFE *\
\* Expected Result: UNSAFE */

/* JEK: a and b are still irrelevant, i.e. sliceable, here
    - easy, because *x=a; not in error-path ... */

int main()
{
  int* x;
  int a = 2;
  int* b;
  b = malloc(sizeof(int)*a);

  x = malloc(sizeof(int));
  *x = a;
 
  free(x);
  *x = 3;
  return 0;
}
