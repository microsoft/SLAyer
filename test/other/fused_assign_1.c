/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

/*
  Check that fused assignment works.
 */
int main() {
  int v;
  
  v = 5;

  v -= 1;

  assert(v==4);
  
}
