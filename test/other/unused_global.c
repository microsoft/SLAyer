/********************************************************************
 * Copyright (c) Microsoft Corporation. All rights reserved.

This test highlights a bug in the Ounused optimisation: structs that
are declared globally are optimised away, even if they are used in the
main program.

Due to this bug, running
  slayer -Ounused true *.c -- -no-builtins
results in UNSAFE, whereas running
  slayer -Ounused false *.c -- -no-builtins
results in SAFE. This is the correct result, and in any case, an
optimisation shouldn't change the results anyway.

 ******************************************************************/
#include "slayer_intrinsics.h"

typedef struct _T {
  int x ;
  char *y;
} T, *PT;

T foo = {1, "foo"};
PT pf = &foo;
T bar = {1, "bar"};
PT pb = &bar; 


int main()
{
  // Taking the address locally is OK. 
/*   PT pfl = &foo; */
/*   PT pbl = &bar;  */
//  if (pfl == pbl) { 
  // But not globally. 
  if (pf == pb) { 
    _SLAyer_error(); // Should never get here
  }
  return 0;
}
