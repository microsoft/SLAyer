/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "sll.h"


typedef struct _LIST_ENTRY {
  struct _LIST_ENTRY *Flink;
  struct _LIST_ENTRY *Blink;
} LIST_ENTRY, *PLIST_ENTRY;

PLIST_ENTRY create(int length) {
/*   int i;  */
  PLIST_ENTRY head, tmp;

  head = NULL;
/*   for(i = 0; i < length; i++) { */
    tmp = (PLIST_ENTRY)malloc(sizeof(LIST_ENTRY));
    tmp->Flink = head;
    head = tmp;
/*   } */
  return head;
}

void traverse(PLIST_ENTRY head) {
  PLIST_ENTRY tmp = head;

  while(tmp != NULL) {
    tmp = tmp->Flink ;
  }
}

void destroy(PLIST_ENTRY head) {
  PLIST_ENTRY t, c = head;

/*   while(c != NULL) { */
    t = c;
    c = c->Flink;
    free(t);
}

void main(void) {
  int length = 10;
  PLIST_ENTRY head;
  head = create(length);
  traverse(head);
  destroy(head);
  return;
}
