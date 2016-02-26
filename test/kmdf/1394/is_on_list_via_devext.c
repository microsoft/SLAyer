/*****************************************************************************
  Copyright (c) Microsoft Corporation.  All rights reserved.

  1394 Feature: t1394_IsOnList.
  Source: util.c, line 60.
  Expected Result: SAFE, MAY LEAK.
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
NTSTATUS SLAyer_harness_init()
{
  PWDFDEVICE_INIT DInit;
  WDF_OBJECT_ATTRIBUTES DAttrib;
  WDFDEVICE Device;
  NTSTATUS status;

  // malloc SL_Device_one
  WDF_OBJECT_ATTRIBUTES_INIT_CONTEXT_TYPE(&DAttrib,DEVICE_EXTENSION);
  status = WdfDeviceCreate(&DInit,&DAttrib,&Device);
  return status;
}

void SLAyer_harness_teardown()
{
  free(SL_Device_one->Context);
  free(SL_Device_one);
}

int main ()
{
  PDEVICE_EXTENSION devExt;
  int result;
  int length, i;
  PISOCH_DETACH_DATA entry;
  NTSTATUS ntStatus;

  // OS Model init
  ntStatus = SLAyer_harness_init();

  if (NT_SUCCESS(ntStatus)) {
    devExt = GetDeviceContext(SL_Device_one);

    InitializeListHead(& (devExt->IsochDetachData));

    // Create a list of some size;
    // 'entry' is one of these elements.
    for (i=0; i<length; i++)  {
      PISOCH_DETACH_DATA tmp =
        (PISOCH_DETACH_DATA) malloc (sizeof(ISOCH_DETACH_DATA)) ;
      tmp->DeviceExtension = devExt;
      if (nondet ()) entry = tmp;
      InsertHeadList(&(devExt->IsochDetachData),&(tmp->IsochDetachList)) ;
    }

    result = t1394_IsOnList(&(entry->IsochDetachList),&devExt->IsochDetachData);
  }

  return (NT_SUCCESS(ntStatus));
  //return ntStatus ;
}
