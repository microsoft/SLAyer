/* Copyright (c) Microsoft Corporation.  All rights reserved. */

typedef struct _T {
  unsigned char a : 1;
  unsigned char   : 2;		/* 2 bits of padding */
  unsigned char b : 2;
  unsigned char   : 0;		/* pad to next char/byte boundary */
  unsigned char c : 3;
  unsigned long   : 0;		/* pad to next long/word boundary */
  unsigned char d : 4;
} T, *PT;

void main()
{
  T t;
  t.a = 0;
  t.b = 3;
  t.c = 2;
  t.d = 4;
  assert( t.a != t.d );
  assert( t.a != t.c );
  assert( t.a != t.b );
}
