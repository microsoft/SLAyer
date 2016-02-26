/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"
#include <assert.h>

#pragma warning(disable:4700) 	/* Using uninitialized memory */
#pragma warning(disable:6001) 	/* Using uninitialized memory */

void main() {
  int i;
  assume(i > 0);
  assert(i > 0);
  i--;
  assert(i);			/* UNSAFE */
  _SLAyer_unreachable();
  _SLAyer_error();
}
