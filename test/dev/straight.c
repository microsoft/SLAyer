/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include <slayer.h>

typedef struct _LIST_ENTRY {
  struct _LIST_ENTRY *Flink;
  struct _LIST_ENTRY *Blink;
} LIST_ENTRY, *PLIST_ENTRY;

void main(void) {
  int length = 10;
  PLIST_ENTRY head, tmp;

  head = NULL;
  tmp = (PLIST_ENTRY)malloc(sizeof(LIST_ENTRY));
  tmp->Flink = head;
  head = tmp;
  tmp = tmp->Flink;

  return;
}
