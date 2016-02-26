/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

/* Returns: POSSIBLY UNSAFE *\
\* Exptected Return: Unsafe */

int main()
{
	int v = nondet() % 10;
	int j;
	int* i;
	int a;
	int* b;
	int c = 0;
	if(v) i = (int*)malloc(sizeof(int));


	for(j = 0; j < 4; j++) {
		v += (nondet() % 4);
	}

	for(a = 0; a <4; a++) {
	  c += 2;
	}
	if (v) {
	  b = (int*)malloc(sizeof(int)*c);
	} 
	if(v) {
		*i = 0;
		free(i);
	}
	return 0;
}
