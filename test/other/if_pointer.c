/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Update x->car if x.
*/

#include "slayer.h"

typedef struct cell cell;
struct cell {
  int car;
  cell* cdr;
};

int main() {
  cell *x ;

  if (x) {
    x->car = x->car * 2 ;
    x->cdr = 0;
  }
  else {
    x = (cell*)malloc(sizeof(cell));
    x->car = 0;
  }

  return 0;
}
