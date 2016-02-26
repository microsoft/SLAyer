/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "csll.h"

/* PSLL_ENTRY create_seg(int length, PSLL_ENTRY head) { */
/*   int i; */
/*   PSLL_ENTRY tmp; */

/*   for(i = 0; i < length; i++) { */
/*     tmp = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY)); */
/*     tmp->Flink = head; */
/*     head = tmp; */
/*   } */

/*   return head; */
/* } */


PSLL_ENTRY find(PSLL_ENTRY head, int fo) {
  PSLL_ENTRY prev, entry, tmp;

/*   for (prev = head, entry = head->Flink; */
/*        entry != head; */
/*        prev = entry, entry = entry->Flink) { */

  prev = head;
  entry = head->Flink;
  while(entry != head) {
    if (entry->Data == fo) {
      /* remove entry */
      tmp = entry->Flink;
      prev->Flink = tmp;
      /* don't free entry, it gets leaked here */

      if (nondet()) {
	return entry;
      } else {
	entry->Flink = entry;
      }
    } else {
      prev = entry;
    }
    entry = entry->Flink;
  }

  return NULL;
}

void main() {
  PSLL_ENTRY head, tail, mark;
  int i;
  PSLL_ENTRY tmp;

  tail = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY));

/*   head = create_seg(4, tail); */
  head = tail;
  for(i = 0; i < 4; i++) {
    tmp = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY));
    tmp->Flink = head;
    head = tmp;
  }

/*   mark = cons(42, head); */

/* /\*   head = create_seg(2, mark); *\/ */
/*   head = mark; */
/*   for(i = 0; i < 2; i++) { */
/*     tmp = (PSLL_ENTRY)malloc(sizeof(SLL_ENTRY)); */
/*     tmp->Flink = head; */
/*     head = tmp; */
/*   } */

  tail->Flink = head;

/*   print_csll(head); */

  find(head, 42);

/*   print_csll(head); */
}
