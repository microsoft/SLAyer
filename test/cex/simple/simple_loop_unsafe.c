/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

/* Result: POSSIBLY UNSAFE *\
\* Expected Result: UNSAFE */

int main()
{
  int *i = NULL;
  int j;

  for(j = 0; j < 5; j++) {
    if(nondet()) {
      i = (int*)malloc(sizeof(int));
    }
  }

  *i = 1;
  free(i);
}
