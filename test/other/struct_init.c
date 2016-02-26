/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Declare and initialize a struct.
*/

#include "slayer.h"

int a, b;

struct pair
{
  void* fst;
  void* snd;
};

// XX initialization
struct pair XX = {&a, &b};

void main()
{
  // xx initialization
  struct pair xx = {&b, &a};

  if (! ((XX.fst == &a) && (XX.snd == &b))) { FAIL; }

  if (! ((xx.fst == &b) && (xx.snd == &a))) { FAIL; }

}

