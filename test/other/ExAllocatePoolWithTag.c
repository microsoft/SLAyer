/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  OS Model's definition of ExAllocatePoolWithTag so as to
  Alloc in terms of type rather than size.

  This deals with code in the form of
  ExAllocatePoolWithTag(PoolType,sizeof(Type),Tag).  But not when no sizeof,
  or sizeof plus additional operators are used, like
  ExAllocatePoolWithTag(NonPagedPool, Ro_SetLocalHostProps3->nLength,
  POOLTAG_KMDF_VDEV) in kmdf_vdev_api.c. Actually, cl wraps the right cast
  around a call to ExAllocatePoolWithTag to not require a macro to do this.
*/


typedef enum _POOL_TYPE {
  NonPagedPool,
/*     PagedPool, */
/*     NonPagedPoolMustSucceed, */
/*     DontUseThisType, */
/*     NonPagedPoolCacheAligned, */
/*     PagedPoolCacheAligned, */
/*     NonPagedPoolCacheAlignedMustS, */
/*     MaxPoolType, */
/*     NonPagedPoolSession = 32, */
/*     PagedPoolSession = NonPagedPoolSession + 1, */
/*     NonPagedPoolMustSucceedSession = PagedPoolSession + 1, */
/*     DontUseThisTypeSession = NonPagedPoolMustSucceedSession + 1, */
/*     NonPagedPoolCacheAlignedSession = DontUseThisTypeSession + 1, */
/*     PagedPoolCacheAlignedSession = NonPagedPoolCacheAlignedSession + 1, */
/*     NonPagedPoolCacheAlignedMustSSession = PagedPoolCacheAlignedSession + 1, */
} POOL_TYPE;

/* Redef to get the type rather than size. */
#define sizeof(T) T

#define sdv_ExAllocatePoolWithTag(PoolType,TypeName,Tag) \
  (TypeName*)malloc(32)



void main()
{
  int* p;
  p = sdv_ExAllocatePoolWithTag(NonPagedPool, sizeof(int), 4);

  // assert that p->(int)r.
}
