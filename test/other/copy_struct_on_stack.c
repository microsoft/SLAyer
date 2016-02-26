/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  The fe needs to move s1, s2 to the heap and change the s1=s2 copy
  to field-wise copy s1.x=s2.x;... .
*/

struct S {
  int x;
  int y;
  char z;
};

void main()
{
  struct S s1 = {1,2,'3'};
  struct S s2 = {4,5,'6'};

  s1 = s2;
}
