/* Copyright (c) Microsoft Corporation. All rights reserved. */

typedef struct _S {
  int a;
  int b;
} S;

S f() {
  S t;
  return t;
}

main() {
  S s;
  s = f();
}
