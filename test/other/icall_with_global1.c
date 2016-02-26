/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include <slayer.h>

int x;

int y;

// pointer to two function types.
typedef int (*FP) ();
typedef void (*FP2) ();

void add_b () {
  x ++;
}

int add_c () {
  return y + 1;
}

FP fp;
FP2 fp2;

//Access {fp, y}
//Modifies nothing.
void f() {
  int z;
  z = (*fp)();
}

//Accesses {fp2, x}
//Writes x (if modified globals are not heapified) 
void g() {
  (*fp2)();
}

//Accesses (fp2,fp,x,y,add_c,add_b)
//Writes fp,fp2,x (without heapification of globals)
void main() {
  fp = &add_c;
  fp2 = &add_b;
  
  f();
  g();
}
