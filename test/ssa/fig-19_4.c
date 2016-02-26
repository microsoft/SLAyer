/* Copyright (c) Microsoft Corporation.  All rights reserved. */

// Figure 19.4 from "modern compiler implementation in ML" (Appel)

int main() {
  int i,j,k;

  i = 1;
  j = 1;
  k = 0;

  while (k < 100) {
    if (j < 20) {
      j = i;
      k = k + 1;
    } else {
      j = k;
      k = k + 2;
    }
  }

  return j;
}
