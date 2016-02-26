/********************************************************************
 * Copyright (c) Microsoft Corporation. All rights reserved.

This test highlights a bug in the Ounused optimisation: structs that
are declared globally are optimised away, even if they are used in the
main program.

 ******************************************************************/
#include "slayer_intrinsics.h"

typedef struct _T {
  int x ;
  char *y;
} T, *PT, **PPT;

T foo = {1, "foo"};
PT pf = &foo;
T bar = {1, "bar"};
PT pb = &bar; 
T foobar = {1, "foobar"};  //Removed by Ounused analysis (Correct)
PT pb2 = &bar; //Not removed by current Ounused analysis (Sound, but not optimal)
PPT ppb = &pb2; //Not removed by current Ounused analysis (Correct)

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
