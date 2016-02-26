/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

typedef struct _cell {
  int car;
} cell;

/*
  Check that copy assignment is implemented when assigning an
  allocated struct into a local struct. 
 */
int main() {
  cell v, *x;
  x = malloc(sizeof(cell));

  x->car = 5;

  v = *x;

  x->car = 6;
  
  assert(v.car==5);

  free(x);  
}
