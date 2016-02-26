// common definitions for cyclic singly-linked lists

#include "../sll/sll.h"


/* assumes cyclic list is non-empty */
void CSLL_destroy(PSLL_ENTRY head) {
  PSLL_ENTRY curr, next;
  curr = head->Flink;
  while( curr != head ) {
    next = curr->Flink;
    free(curr);
    curr = next;
  }
  free(head);
}


/* assumes cyclic list is non-empty */
void CSLL_print(PSLL_ENTRY head) {
  PSLL_ENTRY curr;

  printf("(");
  print_link(head);
  curr = head->Flink;
  do {
    printf(",\n ");
    print_link(curr);
    curr = curr->Flink;
  } while (curr != head);
  printf(")\n");
}
