/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  compare is inlineable.
  Check that:
  1. args passed correctly;
  2. result returned correctly;
  3. we continue to the right place.
*/

#include "slayer.h"

int a, b, c, d, e, f;


void* compare (void* x, void* y, void* z)
{
  void* result;

  if (x==y) goto xy_eq;
  else if (y==z) goto yz_eq;
  else goto no_eq;

 xy_eq: result=&a; return result;

 yz_eq: result=&b; return result;

 no_eq: result=&c; return result;
}

void main()
{
  int *x, *y, *z;
  void* result;

  x=&d; y=&d; z=&f;
  result = compare (x,y,z);
  if (result != &a) FAIL;

  x=&d; y=&e; z=&e;
  result = compare (x,y,z);
  if (result != &b) FAIL;

  x=&d; y=&e; z=&f;
  result = compare (x,y,z);
  if (result != &c) FAIL;
}
