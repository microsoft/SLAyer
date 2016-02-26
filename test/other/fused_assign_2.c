/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

/*
  Check that fused assignment works.
 */
int main() {
  int v;
  int *a;

  a = &v;

  *a = 5;

  *a -= 1;

  assert(v==4);
  
}
