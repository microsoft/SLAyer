/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

typedef struct _cell {
  int car;
} cell;

/*
  Check that copy assignment is implemented when assigning a 
  local struct into another local struct. 
 */
int main() {
  cell v, x;

  v.car = 5;

  x = v;

  v.car = 6;
  
  assert(x.car==5);
  
}
