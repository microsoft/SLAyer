/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  This is a simpler version of how t1394_EvtIoDeviceControl passes Arguments to
  t1394_AsyncWrite.
*/

#include "slayer.h"

struct T {
  int Id ;
  int Arguments[2] ;
};

void init (int iArguments[2])
//void init (int *iArguments)
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
  int Arguments[2] = { 10, 100 };
  init( Arguments );

  { int x; x = Arguments[0]; }

}

