/* Copyright (c) Microsoft Corporation.  All rights reserved. */

struct s {
  int *a;
  int *b;
};

void main() {
  struct s x;
  int **ptr;
  int y;

  x.b = &y;
  ptr = &x.a;
  ptr++;
  *ptr = 0;    // UNSAFE unless we know there is no padding between a and b members of s
  *x.b = 0;    // UNSAFE if we do know there is no padding between a and b members of s
}
