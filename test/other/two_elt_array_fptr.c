/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  A table of function pointers.

  slam seems to do a sound abstraction for this program: PA tells it that
  fun_tbl[] = {f,g}, so whenever fun_tbl[_]() is called, a non-det choice is
  made between f() and g().
*/

#include <slayer.h>

int f (int x)
{
  return x+1;
}

int g (int x)
{
  return x+10;
}

// pointer to a int->int function.
typedef int (*FP) (int);


void main()
{
  int x, y;
  FP fun_tbl[2];

  fun_tbl[0] = f;
  fun_tbl[1] = g;

  x = 0;
  y = fun_tbl[0](x);
  if (y == 0) FAIL;

  x = 0;
  y = fun_tbl[1](x);
  if (y == 0) FAIL;

  return;
}
