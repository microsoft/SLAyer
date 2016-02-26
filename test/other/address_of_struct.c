/**
  Copyright (c) Microsoft Corporation.  All rights reserved.

  pa is an alias of an s.a.
 **/

#include "slayer.h"

struct s {
  int a;
};

void main() {
  int *pa;
  struct s x;

  pa = &(x.a);
  *pa = 0;

  FAIL_IF(x.a != 0);
  return;

}
