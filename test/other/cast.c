/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include <assert.h>

void main() {
  int i;
  float* fp1;
  float* fp2;
  float f1;
  float f2;
  float f3;
  float f4;

  (int*)(float*)fp1 = &i;
  /* compiles to:
	lea	eax, DWORD PTR _i$[ebp]
	mov	DWORD PTR _fp1$[ebp], eax
  */
  fp2 = (float*)(int*)(&i);
  /* compiles to:
	lea	ecx, DWORD PTR _i$[ebp]
	mov	DWORD PTR _fp2$[ebp], ecx
  */
  assert(fp1 == fp2); /* SAFE */


  (int)(float)f1 = i;
  /* compiles to:
	mov	ecx, DWORD PTR _i$[ebp]
	mov	DWORD PTR _f1$[ebp], ecx
  */
  f2 = (float)(int)i;
  /* compiles to:
	cvtsi2ss xmm0, DWORD PTR _i$[ebp]
	movss	DWORD PTR _f2$[ebp], xmm0
  */


  (int*)(int)(float)f3 = &i;
  /* compiles to:
	lea	edx, DWORD PTR _i$[ebp]
	mov	DWORD PTR _f3$[ebp], edx
  */
  f4 = (float)(int)(int*)(&i);
  /* compiles to:
	lea	eax, DWORD PTR _i$[ebp]
	cvtsi2ss xmm0, eax
	movss	DWORD PTR _f4$[ebp], xmm0
  */

  assert(f1 == f2 || f3 == f4); /* UNSAFE */
}
