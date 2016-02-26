/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

int main()
{
  int *a;
  int *b;
  int *c;
  int *d;

  if(nondet()) {
    a = (int*)malloc(sizeof(int));
  }

  if(nondet()) {
    b = a;
  }

  if(nondet()) {
    c = b;
  }

  if(nondet()) {
    d = c;
  }

  *d = 17;
  free(b);
}
