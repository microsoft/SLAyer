/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  deref_via_call.c : assign into a local, but by passing it's address down a
  chain of function calls.

  This program should be safe.
*/

#include <slayer.h>

int a,b,c;

void add10(int* ppi)
{
  *ppi = *ppi + 10;
}

int add10_wrapper(int pi)
{
  add10(&pi);
  return pi;
}

void main ()
{
  add10_wrapper(1);
}
