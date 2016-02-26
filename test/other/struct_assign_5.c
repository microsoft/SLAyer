/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

typedef struct _inner_cell {
  int car;
} inner_cell;

typedef struct _cell {
  inner_cell car;
} cell;

/*
  Check that copy assignment is implemented when assigning an
  allocated struct into a local struct. 
 */
int main() {
  cell v, x;

  x.car.car = 5;

  v = x;

  x.car.car = 6;
  
  assert(v.car.car==5);
  
 
}
