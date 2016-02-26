
#include "SLAyer.h"

int *x;
int *y;

int *f()
{
  free(y);
  *x += 1;
  return x;
}

void main()
{
  x = (int*)malloc(sizeof(int));
  y = (int*)malloc(sizeof(int));
  *x = 0;

  *(f()) += 1;
  // Should only evaluate *(f()) once.
  // Integers are too abstracted by SLAyer to prove this, 
  // added free(y) to track if it is called twice.
  //assert(*x == 2);

  free(x);
}
