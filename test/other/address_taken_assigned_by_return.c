/* Copyright (c) Microsoft Corporation. All rights reserved. */

int f() {
  return 1;
}

main() {
  int s;
  int * ps;
  ps = & s;
  s = f();
  assert(*ps == 1);
}
