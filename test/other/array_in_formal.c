/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Flattening arrays to scalars is not OK if the array is in a formal.
  Source of bug: kmdf 1394.
*/

// Un-comment to exhibit work-around.
//#define WORK_AROUND


// Basic types
#define ULONG unsigned long
#define ULONG_PTR unsigned long
#define LONG_PTR long
#define PAGE_SIZE 32
#define UCHAR unsigned char

// 1394 data structures
typedef struct _ALLOCATE_ADDRESS_RANGE {
  // ....
  ULONG          nLength;
  UCHAR          Data[1];
} ALLOCATE_ADDRESS_RANGE, *PALLOCATE_ADDRESS_RANGE;

typedef struct _CONTEXT_BUNDLE
{
  PALLOCATE_ADDRESS_RANGE Context0;
} CONTEXT_BUNDLE;



// Stub
int ADDRESS_AND_SIZE_TO_SPAN_PAGES(void* Va, ULONG Size)
{
  int x ;
  return x;
}


kmdf1394_AllocateAddressRange (
			       PALLOCATE_ADDRESS_RANGE
#ifdef WORK_AROUND
			       F_AAR
#else
			       AAR
#endif
			       )
{
  void* pAsyncAddressData ;
  int nPages;
  CONTEXT_BUNDLE ContextBundle;
#ifdef WORK_AROUND
  PALLOCATE_ADDRESS_RANGE AAR = F_AAR;
#endif

  ContextBundle.Context0 = AAR;

  // Access to nLength is fine.
  pAsyncAddressData = malloc(AAR->nLength);

  // Access to Data causes SLAyer fe to think the formal AAR is a
  // local whose address is taken.
  nPages = ADDRESS_AND_SIZE_TO_SPAN_PAGES(
					  AAR->Data,
					  AAR->nLength);

  free(pAsyncAddressData);
}



void main()
{
  ALLOCATE_ADDRESS_RANGE MainAAR;
  kmdf1394_AllocateAddressRange(&MainAAR);
}

