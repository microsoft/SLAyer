/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Statement inside an Expr.
*/

void main() {
  int x = 0;
  if(x++, x) { x = 42; }
  return;
}
