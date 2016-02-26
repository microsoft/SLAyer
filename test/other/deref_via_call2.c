/* Copyright (c) Microsoft Corporation.  All rights reserved. */

/*
  deref_via_call2.c : assign into a local, but by passing it's address down a
  chain of function calls.

  This program should be safe: it should never assign *0=0.

  This was a test case to check frontend_slam's local declaration correctness:
  parent functions used to declare their child functions locals.

  This is the same as deref_via_call.c except that the address of the formal
  paramater of f is not taken.
*/

#include <slayer.h>

int a,b,c;

int uninit_g_x ;
int* uninit_g_py ;
int uninit_g_z ;

void g(int*** g_ppi)
{
  **g_ppi = &a;
}

void f(int** _f_pi)
{
  int** f_pi = _f_pi;
  int*** f_ppi;
  f_ppi = &f_pi;
  g(f_ppi);
}

void main ()
{
  int** p = (int*) malloc (sizeof(int));
  *p = &b;
  f(p);
  *p = &c;
  f(p);
  *p = NULL;
  f(p);

  assert(*p != NULL);
  return;
}
