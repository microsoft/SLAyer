/**
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Common definitions for singly-linked lists.
**/

#include <stdlib.h>

/* In non-SLAyer case, allow tests to be compiled. */
#ifndef SLAyer
#include <stdio.h>
#else
int printf (const char *fmt,...) { return 0; }
#endif


typedef struct _SLL_ENTRY {
  int Data;
  struct _SLL_ENTRY *Flink;
} SLL_ENTRY, *PSLL_ENTRY;


/* Construction */

PSLL_ENTRY cons(int a, SLL_ENTRY* d) {
  PSLL_ENTRY x = (SLL_ENTRY*)malloc(sizeof(SLL_ENTRY));
  x->Data = a;
  x->Flink = d;
  return x;
}

PSLL_ENTRY SLL_create_seg(int length, PSLL_ENTRY head) {
  int i;
  PSLL_ENTRY tmp;

  for(i = 0; i < length; i++) {
    tmp = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY));
    tmp->Flink = head;
    head = tmp;
  }

  return head;
}

PSLL_ENTRY SLL_create(int length) {
  int i;
  PSLL_ENTRY head, tmp;

  head = NULL;
  for(i = 0; i < length; i++) {
    tmp = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY));
    tmp->Flink = head;
    head = tmp;
  }

  return head;
}

/*
PSLL_ENTRY create(int length) {
  return create_seg(length, NULL);
}
*/

/* Destruction */

void SLL_destroy_seg(PSLL_ENTRY x, PSLL_ENTRY y) {
  PSLL_ENTRY t;

  while(x != y) {
    t = x;
    x = x->Flink;
    free(t);
  }
}

void SLL_destroy(PSLL_ENTRY x) {
  SLL_destroy_seg(x, NULL);
}

/*
void SLL_destroy(PSLL_ENTRY x) {
  PSLL_ENTRY t;

  while(x != NULL) {
    t = x;
    x = x->Flink;
    free(t);
  }
}
*/


/* Printing */

void print_link(PSLL_ENTRY x) {
  printf("%p -> {D:%i, F:%p}", x, x->Data, x->Flink);
/*   printf("%i", x->Data); */
}

void print_listseg(PSLL_ENTRY a, SLL_ENTRY* y) {
  PSLL_ENTRY x = a;

  printf("(");
  if(x != y) {
    print_link(x);
    x = x->Flink;
    while(x != y) {
      printf(",\n ");
      print_link(x);
      x = x->Flink;
    };
  };
  printf(")\n");
}

/* void print_list(PSLL_ENTRY x) { print_listseg(x, 0); } */

void print_list(PSLL_ENTRY a) {
  PSLL_ENTRY x = a;

  printf("(");
  if(x != NULL) {
    print_link(x);
    x = x->Flink;
    while(x != NULL) {
      printf(", ");
      print_link(x);
      x = x->Flink;
    };
  };
  printf(")");
}
