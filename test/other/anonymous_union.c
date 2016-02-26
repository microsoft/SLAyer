/* Copyright (c) Microsoft Corporation.  All rights reserved. */

typedef struct _T {
    union {
        int a;
        struct {
            char b;
            char c;
            short d;
        };
    };
    void* e;
} T, *PT;


void main()
{
  T t;
  PT pt = &t;
  assert( pt != NULL );
}
