/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include <slayer.h>

typedef struct _T {
  int a;
  int b;
  int c;
} T;

// pointer to a T*->int* function.
typedef int* (*FP) (T*);

int* add_b (T* x) {
  return &(x->b);
}

int* add_c (T* x) {
  return &(x->c);
}

void main() {
  T t;
  int* x;
  int* y;
  FP fp;

  fp = &add_b;
  x = &(t.a);
  y = (*fp)(x); // Even if we write fp(x), cl will coerce this to (*fp)(x).
  assert(x != y);

  fp = &add_c;
  x = &(t.a);
  y = (*fp)(x);
  assert(x != y);
}
