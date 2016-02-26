/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  deref_via_call.c : assign into a local, but by passing it's address down a
  chain of function calls.

  This program should be safe.
*/

#include <slayer.h>

int g;

void set_to_global_addr(int*** ppi)
{
  **ppi = &g;
}

void set_to_global_addr_wrapper(int** pi)
{
  int** l_pi = pi;
  int*** ppi;
  ppi = &l_pi;
  set_to_global_addr(ppi);
}

void main ()
{
  int **p = (int*) malloc (sizeof(int));
  *p = NULL;
  set_to_global_addr_wrapper(p);

  assert (*p != NULL);

  free(p);
}
