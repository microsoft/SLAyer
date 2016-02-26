/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

int main() {
  int *x = (int*)malloc(sizeof(int));
  free(x);
  free(x);
  x = (int*)malloc(sizeof(int));
}
