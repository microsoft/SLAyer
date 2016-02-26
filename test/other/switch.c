/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

*/
#include "slayer.h"

void main()
{
  int y = 0;
  int x;
  switch (x) {
  case 0:
    y = y + 0;
    break;
  case 1:
    y = y + 1;
    break;
  case 2:
    y = y + 2;
    // fall-through
  default:
    y = y * 10;
    break;
  }

  assert( x == 0 ? y == 0 :
          x == 1 ? y == 1 :
          x == 2 ? y == 20 : y == 0 );

}
