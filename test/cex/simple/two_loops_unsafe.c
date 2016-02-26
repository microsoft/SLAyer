/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

/* Result: POSSIBLY UNSAFE *\
\* Expected Result: UNSAFE */

int main()
{
  int *i = NULL;
  int *k = NULL;
  int j;

  for(j = 0; j < 45; j++) {
    if(nondet()) {
      i = (int*)malloc(sizeof(int));
    }
  }

  for(j = 0; j < 17; j++) {
    if(nondet()) {
      k = i;
    }
  }

  *k = 1;
  free(k);
}
