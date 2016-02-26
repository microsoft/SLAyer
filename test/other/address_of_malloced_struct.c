/**
  Copyright (c) Microsoft Corporation.  All rights reserved.

  y is an alias of an s->a.
 **/

#include "slayer.h"

typedef struct _ls {int car; struct ls* cdr;} ls;

void main() {
  ls* x;
  int* y;
  x = (ls*)malloc(sizeof(ls));
  x->car = 42;
  y = 0;
  y = &(x->car);
  FAIL_IF( *y != 42 ) ;
  free(x);
  return;
}
