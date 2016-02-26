/* Copyright (c) Microsoft Corporation.  All rights reserved. */

// frec not inlineable: calls itself.
void frec (int x)
{
/*   int x = y; */
  if (x==0) return;
  else frec(x-1);
}

void main()
{
  frec(99);
}
