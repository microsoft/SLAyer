/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

typedef struct cell cell;
struct cell {
  int car;
  cell* cdr;
};

int main() {
  cell *x = (cell*)malloc(sizeof(cell));
  x->car = 42;
  x->cdr = 0;
  free(x);
}
