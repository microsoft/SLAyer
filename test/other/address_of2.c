/**
  Copyright (c) Microsoft Corporation.  All rights reserved.

  p is an alias of x.
 **/

#include "slayer.h"

/*
   f's x should be heapified (it has it's address taken). It needs to be
   alloc/free-ed at the start/end of f.

   This program is a test-case for a bug # in SlamTranslator in which the
   to-be-heapified set wasn't implemented as a set. x was being malloc/free-ed
   as many times as it occured in the body of f.

*/
void f() {
  int x;
  int *p;

  p = &x;
  p = &x; // Just another mention of &x.
  *p = 0;
  FAIL_IF(x!=0);

  return;
}

void main()
{
  f();
}
