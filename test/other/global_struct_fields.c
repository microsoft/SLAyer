/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Assignment into struct fields.
*/

#include "slayer.h"

struct Globals
{
  int* AssocClassList;
  int* NumAssocClass;
};


struct Globals g;

void cpy (int* src, int* dest)
{
  FAIL_IF (src == dest) ;
  return;
}

main()
{
  int dummy;
  g.AssocClassList = NULL;
  g.NumAssocClass = &dummy;
  cpy(g.AssocClassList, g.NumAssocClass);
}
