/*   Copyright (c) Microsoft Corporation.  All rights reserved.  */
#include "sll.h"

void main() {
  PSLL_ENTRY x = SLL_create(nondet());
  print_list(x);
  SLL_destroy(x);
}
