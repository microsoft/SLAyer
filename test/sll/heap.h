/*  Copyright (c) Microsoft Corporation.  All rights reserved.  */

#include <stdlib.h>

/* Note: be able to compile under non-SLAyer circumstances. */
#ifndef SLAyer
#include <stdio.h>
#else
int printf (const char *fmt,...) { return 0; }
int printf_s (const char *fmt,...) { return 0; }
#endif

typedef struct cell {
  int car;
  struct cell* cdr;
} cell;

cell* new() {
  cell* x = (cell*)malloc(sizeof(cell));
  assert(x != NULL);
  return x;
}

void print_cell(cell* x) {
  printf("%i", x->car);
}


