/* Copyright (c) Microsoft Corporation. All rights reserved. */

typedef struct _S {
  int a;
  int b;
} S;

void f(S x) {
  x.b = 1;
}

main() {
  S s;
  s.b = 0;
  f(s);
  assert(s.b == 0);
}
