/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Assign into struct fields.
*/

#include "slayer.h"

typedef struct _val {
   void * foo;
} val ;

int main ()
{
    val * y = (val *)malloc(sizeof(val));
    val * z = (val *)malloc(sizeof(val));
    int x;
    y->foo = &x;
    z->foo = y->foo;
    FAIL_IF(&x != z->foo);
}
