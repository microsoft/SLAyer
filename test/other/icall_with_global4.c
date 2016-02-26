/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include <slayer.h>

int x;

int y;

// pointer to two function types.
typedef void (*FP) (int*);
typedef void (*FP2) (int*);

void add_b (int * f) {
  x ++;
}

void add_c (int * f) {
  y ++;
}

FP2 fp;
FP fp2;

//Access {fp, y}
//   with type approximation or (flow insensitive) may alias also {x}
//Writes  { y} (without heapification)
//    with type approximation or (flow insensitive) may alias also {x}
void f() {
  int z;
  (*fp)(&z);
}

//Accesses {fp, x} 
//    with type approximation or (flow insensitive) may alias also {y}
//Writes x (if modified globals are not heapified) 
//    with type approximation or (flow insensitive) may alias also {y}
void g() {
  int z;
  (*fp)(&z);
}

//Accesses (fp2,fp,x,y,add_c,add_b)
//Writes fp,fp2,x (without heapification of globals)
void main() {
  fp = &add_c;
  fp2 = &add_b;
  
  f();

  fp = &add_b;
  fp2 = &add_c;

  g();
}
