
#define TRUE (1==1)
#define FALSE (1==0)

int *p;
int *q;
int freed_p = FALSE;
int freed_q = FALSE;

void delete(int* obj)
{
  if (obj == p) { freed_p = TRUE; }
  if (obj == q) { freed_q = TRUE; }
  free(obj);
}

void main()
{
  int x ;

  p = malloc(sizeof(int));
  *p = 2;

  q = malloc(sizeof(int));
  *q = 2;

  if (x) {
    delete(p);
  } else {
    delete(q);
  }

  if (freed_p == FALSE) { free(p); }
  if (freed_q == FALSE) { free(q); }

}
