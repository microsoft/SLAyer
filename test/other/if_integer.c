/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Update x->car if n.
*/

#include "slayer.h"

typedef struct cell cell;
struct cell {
  int car;
  cell* cdr;
};

int main() {
  int n;
  cell *x;

  n = 5;
  x = (cell*)malloc(sizeof(cell));
  x->car = 0;

  if (n) {
    x->car = x->car + 1;
    x->cdr = 0;
  }

  assert( x->car == 1 );

  free(x);

  return 0;
}
