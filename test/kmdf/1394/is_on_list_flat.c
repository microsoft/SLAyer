/*****************************************************************************
  Copyright (c) Microsoft Corporation.  All rights reserved.

  1394 Feature: t1394_IsOnList.
  Source: util.c, line 60.
  Expected Result: SAFE.
 *****************************************************************************/

#include "harness.h"
#include "1394.h"

BOOLEAN
t1394_IsOnList(
    PLIST_ENTRY        Entry,
    PLIST_ENTRY        List
    )
{
    PLIST_ENTRY TempEntry;

    for(
        TempEntry = List->Flink;
        TempEntry != List;
        TempEntry = TempEntry->Flink
        )
    {
        if (TempEntry == Entry)
        {
	  /* TRACE(TL_TRACE, ("Entry 0x%x found on list 0x%x\n", Entry, List)); */
	  return TRUE;
        }
    }

    /* TRACE(TL_TRACE, ("Entry 0x%x not found on list 0x%x\n", Entry, List)); */
    return FALSE;
}


/*
 * Harness.
 */
int main ()
{
  int result;
  int length, i;
  PLIST_ENTRY entry, ll, tmp;

  // Create a list of some size; 'entry' is one of these elements.
  ll = (PLIST_ENTRY) malloc (sizeof(LIST_ENTRY)) ;
  InitializeListHead(ll);
  entry = ll;

  for (i=0; i<length; i++)
    {
      tmp = (PLIST_ENTRY) malloc (sizeof(LIST_ENTRY)) ;
      if (nondet ()) entry = tmp;
      InsertHeadList(ll,tmp);
    }

  result = t1394_IsOnList(entry,ll) ;

  return (TRUE == result);
}
