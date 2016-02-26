/* Copyright (c) Microsoft Corporation.  All rights reserved. */

// non-empty circular lists

/* implements a stack and a queue simultaneously using a circular list:
 *
 *   rear |-> top_front * lseg(top_front, rear)
 *
 * - the rear of the queue is the node pointed to by rear
 * - the top of the stack and the front of the queue
 *   are both the node pointed to by the tl of x
 * - we keep a pointer r to the rear, and existentially quantify top_front
 */
#include "slayer.h"
#include "heap.h"

// utility functions ///////////////////////////////////////////////////////////

void print_clist(cell* x) /* x|->-,y * ls(y,x) */ {
  cell* z = x;
  printf_s("(");
  do {
    print_cell(z);
    z = z->cdr;
  } while(z != x ? printf_s(", "),1 : 0);
  printf_s(",...)");
}  /* x|->-,y * ls(y,x) */


// operations //////////////////////////////////////////////////////////////////

/* push */
void insert_after(cell* x, int n) /* x|->-,y * ls(y,x) */ {
  cell* t = new();  /* x|->-,y * t|->-,- * ls(y,x) */
  t->car = n;
  t->cdr = x->cdr;  /* x|->-,y * t|->n,y * ls(y,x) */
  x->cdr = t;
}  /* x|->-,t * t|->n,y * ls(y,x) */

void rotate(cell* *x) /* x|->-,y * ls(y,x) */ {
  *x = (*x)->cdr;  /* ls(x,x') * x'|->-,x */
} /* x|->-,z * ls(z,x) */

/* enqueue */
void insert_before(cell* *x, int n) /* x|->-,y * ls(y,x) */ {
  insert_after(*x, n);  /* x|->-,t * t|->n,y * ls(y,x) */
  *x = (*x)->cdr;  /* spec of rotate(x); too weak to use here */
} /* x|->n,y * ls(y,x) */

/* pop / dequeue */
/* this is the spec & proof for the possibly-empty list version */
void delete_next(cell* x) /* x|->y * ls(y,x) */ {
  cell* t = x->cdr;  /* t=y | x=y ? x|->y : x|->y * y|->z * ls(z,x) */
  cell* u = t->cdr;  /* t=y | x=y ? x|->y : u=z | x|->y * y|->z * ls(z,x) */
  x->cdr = u;  /* x=y ? t|->u : x|->z * t|->z * ls(z,x)) */
  free(t);
} /* x=y ? emp : x|->z * ls(z,x) */


// test harness ////////////////////////////////////////////////////////////////

int main() {
  cell* x;
  x = new();
  x->car = 0;
  x->cdr = x;
  printf_s("clist:\t\t"); print_clist(x); printf_s("\n");
  insert_after(x, 1);
  insert_after(x, 2);
  printf_s("push 1, 2:\t"); print_clist(x); printf_s("\n");
  insert_before(&x, 3);
  insert_before(&x, 4);
  printf_s("enqueue 3, 4:\t"); print_clist(x); printf_s("\n");
  delete_next(x);
  printf_s("pop / dequeue:\t"); print_clist(x); printf_s("\n");
  rotate(&x);
  printf_s("rotate:\t\t"); print_clist(x); printf_s("\n");
}
