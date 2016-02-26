/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

struct list
{
  int data;
  struct list * next;
};

struct list* l;
struct list* m;

// {l,c0,c1,c2} should be in f's footprint.
// m shouldn't be.
void f()
{
  struct list *x = l;
}

void main()
{
  struct list *c0, *c1, *c2;

  m =  (struct list*) malloc(sizeof(struct list));

  c2 = (struct list*) malloc(sizeof(struct list));
  c2->data = 2;
  c2->next = (struct list *)NULL;

  c1 = (struct list*) malloc(sizeof(struct list));
  c1->data = 1;
  c1->next = c2;

  c0 = (struct list*) malloc(sizeof(struct list));
  c0->data = 0;
  c0->next = c1;

  // loc:     c0      c1      c2
  // content: (0,c1)  (1,c2)  (2,NULL)
  l = c0;
  f();
}
