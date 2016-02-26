/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "slayer.h"

/* Returns: POSSIBLY UNSAFE *\
\* Exptected Return: Unsafe */

int main()
{
	int v = nondet() % 10;
	int j;
	int* i;
	if(v) i = (int*)malloc(sizeof(int));


	for(j = 0; j < 4; j++) {
		v += (nondet() % 4);
	}

	if(v) {
		*i = 0;
		free(i);
	}
	return 0;
}
