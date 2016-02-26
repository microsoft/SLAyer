/**
   Copyright (c) Microsoft Corporation.  All rights reserved.

   Test case for PS #603. The load to x below is like
   (&fdoAttributes)->ContextTypeInfo = (&_WDF_DEVICE_EXTENSION_TYPE_INFO)->UniqueType;
   from wdf_device_create.c
   The other loads are variations of the x one.
**/


struct _RECT {
  int Length;
  int Width;
};

typedef struct _RECT RECT;

RECT r;

void main ()
{
  int x,y,z ;
  RECT *pr;

  // #1 addr of r directly
  x = (&r)->Length ;

  // #2 addr or r via an intermediate
  pr = &r;
  y = pr->Length;

  // #3 access r.Length
  z = r.Length ;

 End: return;
}
