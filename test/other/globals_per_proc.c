/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Run these with 'inline' set to false.
*/

int x,y,z,u,v;

// f uses {x}
void f()
{
 int a;
 int b;
 int c;
 // assert: x->_. (y->_ should have been framed away.)
 a = x;
 c = &b;
}

// g uses {y,x}
void g()
{
 int b = y;
 // assert: y->_ * x->_. (z->_ should have been framed away.)
 f();
}

// h uses {z,y,x}
void h()
{
 int c=z;
 // assert: z->_ * y->_ * x->_
 g();
}

void main()
{
 h();
}
