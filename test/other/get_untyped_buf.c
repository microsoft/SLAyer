/*
Copyright (c) 2013  Microsoft Corporation All Rights Reserved

This is, I think, how kernel functions like WdfRequestRetrieveInputBuffer work.

SLAyer currently casts the &t to a void* when calling get_buf (that's ok), but
then doesn't reconstruct the type when t is used as a T after get_buf. We need
a way to deal with this type of code.
*/

typedef struct _T {
  int X;
  int Y;
  int Z;
} T;

/* Point p to a (untyped) buffer. */
void get_buf(void* *p)
{
  void *new_buf = (void*)malloc(128);
  *p = new_buf;
}

void main()
{
  int x;
  T *t;
  get_buf(&t);
  x = t->Z;
  free(t);
}
