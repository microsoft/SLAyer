/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

/*JEK: a and b, nd all their commands, should be irrelevent, i.e. sliceable here */

/* Result: POSSIBLY UNSAFE *\
\* Expected Result: UNSAFE */

int main()
{
  int a = 7;
  int *b = NULL;
  int *i = NULL;
  int j;

  for(j = 0; j < 5; j++) {
    if(nondet()) {
      i = (int*)malloc(sizeof(int));
      a ++ ;
    }
  }

  b = (int*)malloc(a*(sizeof(int)));
  *i = 1;
  free(b);
  free(i);
}
