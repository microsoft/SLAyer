/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  C's bool representation.
*/

#include "slayer.h"

int main() {
  int four, eq_four;
  four = 4;
  eq_four = (four == 4);
  FAIL_IF( eq_four != 1 );
}
