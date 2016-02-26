/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  A while loop whose test loads from two memory locations.
*/

#include "slayer.h"

void main()
{

  int *p;
  int *q;
  p = (int*) malloc(sizeof(int));
  q = (int*) malloc(sizeof(int));
  *p = 10;
  *q = 0;

  while (*p != *q) (*q)++;

  if (*p != *q) FAIL;
}
