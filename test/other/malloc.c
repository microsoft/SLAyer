/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

int main() {
  int *x = (int*)malloc(sizeof(int));
  int *y = (int*)malloc(sizeof(int));
  *x = 42;
  *y = 13;
}
