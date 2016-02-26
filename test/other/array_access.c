/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Check that the A access is valid.
*/

#include "slayer.h"

int A[4] = { 0, 1, 2, 3 };

void main()
{
  int x;
  x = A[0];
  x = A[1];
  x = A[2];
  x = A[3];
  x = A[55]; /* We don't check array bounds */
  x = A[-2];
  x = -2[A];
}
