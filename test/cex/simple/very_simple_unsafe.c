/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

/* Result: POSSIBLY UNSAFE *\
\* Expected Result: UNSAFE */


int main()
{
  int* x;
  *x = 3;
  return 0;
}
