/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Frontend elides stores to a constant address.
*/

int main() {
  int x;
  x = *(int*)0;  // x = [0], but fe translates to x=0;
  *(int*)0 = x; // [0] = x , but fe elides completely.
}
