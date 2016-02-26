/* Copyright (c) Microsoft Corporation.  All rights reserved. */

typedef struct _S {
  int x;
  int arr[30];
} S, *PS;

void
main() {
  S s;
  s = s;
}
