/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

typedef struct _cell {
  int car;
  struct _cell* cdr;
  struct foo {
    int bar;
  } baz;
} cell;

void main() {
  cell* x = (cell*)malloc(sizeof(cell));
  cell y;
  (*x).car = 1;
  y.car = 2;
  y.baz.bar = 42;
  (*x).baz.bar = 21;
  return;
}
