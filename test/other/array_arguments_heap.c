/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  This is how t1394_EvtIoDeviceControl passes Arguments to t1394_AsyncWrite.
*/
#include "slayer.h"

struct T {
  int Id ;
  int Arguments[2] ;
};

void init (int iArguments[2])
{
  struct T *p ;
  p = (struct T *)malloc(sizeof(struct T));
  p->Id = 0;
  p->Arguments[0] = iArguments[0] ;
  p->Arguments[1] = iArguments[1] ;
  free(p);
}

void main()
{

  struct T *t;
  t = (struct T *)malloc(sizeof(struct T));
  t->Id = -1;
  t->Arguments[0] = 0 ;
  t->Arguments[1] = 1 ;
  init(t->Arguments);

  { int x; x = t->Arguments[0]; }
  free(t);
}
