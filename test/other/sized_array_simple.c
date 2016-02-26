/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Declare a fixed size array.
*/

void f(int x)
{
  int i;
  int a[10];
  for (i=0; i<10; i++)
    a[i] = x + i;
}


void main()
{
  int x = 42;
  f(x);
}
