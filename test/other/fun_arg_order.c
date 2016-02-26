/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  fun arg passing convention.
*/

#include "slayer.h"

void f(int one, int two, int three)
{
  FAIL_IF (! ((one==1) && (two==2) && (three==3)) ) ;
  return;
}

void main() {

  f(1,2,3);
  return;
}
