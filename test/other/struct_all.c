/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

typedef struct _cell {
  int car;
  struct _cell* cdr;
} cell;

int main() {
  int y, *z;
  cell v, *x, *w, **u;
  y = 3;
  z = (int*)malloc(sizeof(int));
  *z = 5;
  y = *z;
  free(z);
  z = &y;
  *z = 0;
  if(y!=*z) FAIL;
  v.car = 53;
  v.cdr = 0;
  z = &v.car;
  x = (cell*)malloc(sizeof(cell));
  (*x).car = 42;
  (*x).cdr = 0;
  y = (*x).car;
  w = (*x).cdr;
  z = &(*x).car;
  u = &(*x).cdr;
  free(x);
  free(malloc(sizeof(int)));
}
