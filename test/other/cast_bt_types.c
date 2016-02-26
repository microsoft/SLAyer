/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

struct I { int i; };
struct IJ { int i; int j; };

void main()
{
  int *x;
  struct I *pi;
  struct IJ *pij;

  x = malloc(sizeof(int));
  // x->(int) []

  pi = (struct I*)x;
  pi->i = 32;
  // x->(I) [i:32]

  pij = (struct IJ*)x;
  pij->i = 10;
  pij->j = 100; // UNSAFE!
  // x->(IJ) [i:10;j:100]

  x = (int*)pij;
  free(x);

}
