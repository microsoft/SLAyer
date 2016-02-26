/*****************************************************************************
 Copyright (c) Microsoft Corporation.  All rights reserved.

 1394 Feature: allocate_address_range_insert_head_list,
                free_address_range_{remove_entry,insert_head}_list.
 Source: asyncapi.c, lines 199, 278, 304.
 Expected Result: SAFE, MAY LEAK.
*****************************************************************************/

#include "harness.h"
#include "1394.h"


NTSTATUS
t1394_FreeAddressRange(
    PDEVICE_EXTENSION deviceExtension,
    HANDLE            hAddressRange
    )
{
    NTSTATUS                ntStatus         = STATUS_SUCCESS;
    PASYNC_ADDRESS_DATA     AsyncAddressData = NULL;
    PLIST_ENTRY             listHead;
    PLIST_ENTRY             thisEntry;

    // have to find our struct...
    listHead = &deviceExtension->AsyncAddressData;

    for(thisEntry = listHead->Flink;
	thisEntry != listHead;
	AsyncAddressData = NULL, thisEntry = thisEntry->Flink)
      {
        AsyncAddressData = CONTAINING_RECORD(thisEntry, ASYNC_ADDRESS_DATA,
					     AsyncAddressList);

        if (AsyncAddressData->hAddressRange == hAddressRange) {
	  RemoveEntryList(&AsyncAddressData->AsyncAddressList);
	  break;
        }
      }

    // never found an entry...
    if (!AsyncAddressData) {
        ntStatus = STATUS_INVALID_PARAMETER;
        goto Exit_FreeAddressRange;
    }

    // got it, lets free it...
    // need to free up everything associated with this allocate...
    if (AsyncAddressData->pMdl) {
        IoFreeMdl(AsyncAddressData->pMdl);
    }
    if (AsyncAddressData->Buffer) {
        ExFreePool(AsyncAddressData->Buffer);
    }

    if (AsyncAddressData->AddressRange) {
        ExFreePool(AsyncAddressData->AddressRange);
    }

    if (AsyncAddressData) {
        ExFreePool(AsyncAddressData);
    }

 Exit_FreeAddressRange:
    return(ntStatus);
} // t1394_FreeAddressRange




/******************************************************************************

  main

 ******************************************************************************/
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

NTSTATUS
main()
{
  PDEVICE_EXTENSION  deviceExtension;// = &SL_Context;
  PLIST_ENTRY       listEntry;
  PADDRESS_RANGE    hAddressRange;
  NTSTATUS          ntStatus;
  ULONG             nLength;
  unsigned int      i, count;

  // OS Model init
  ntStatus = SLAyer_harness_init();

  // We set up dE->AAL to a steady state.
  if (NT_SUCCESS(ntStatus)) {
    // Setup.
    deviceExtension = GetDeviceContext(SL_Device_one);
    InitializeListHead(&(deviceExtension->AsyncAddressData));

    for ( i=0; i<count; i++) {
      PASYNC_ADDRESS_DATA pAsyncAddressData;

      // Use _SLAyer_malloc rather than ExAllocate to definitely alloc.
      pAsyncAddressData = _SLAyer_malloc(sizeof(ASYNC_ADDRESS_DATA));

      pAsyncAddressData->Buffer = _SLAyer_malloc(nLength);

      pAsyncAddressData->AddressRange = _SLAyer_malloc(sizeof(ADDRESS_RANGE));

      pAsyncAddressData->pMdl =
        IoAllocateMdl(pAsyncAddressData->Buffer,
                      nLength,
                      FALSE,
                      FALSE,
                      NULL);

      InitializeListHead(&(pAsyncAddressData->AsyncAddressList));

      InsertHeadList(&deviceExtension->AsyncAddressData,
                     &pAsyncAddressData->AsyncAddressList);
    }


    // 2. Delete some AsyncAddressDatas.
    ntStatus = t1394_FreeAddressRange(deviceExtension, (HANDLE) hAddressRange);

    /*
     */
    // Note: We currently LEAK; should teardown here.
    /*
      while (!IsListEmpty(&deviceExtension->AsyncAddressData)) {

      PASYNC_ADDRESS_DATA     AsyncAddressData;

      // get struct off list
      listEntry = RemoveHeadList(&deviceExtension->AsyncAddressData);

      AsyncAddressData = CONTAINING_RECORD(listEntry, ASYNC_ADDRESS_DATA,
      AsyncAddressList);

      // need to free up everything associated with this allocate...
      if (AsyncAddressData->pMdl)
      IoFreeMdl(AsyncAddressData->pMdl);

      if (AsyncAddressData->Buffer)
      ExFreePool(AsyncAddressData->Buffer);

      if (AsyncAddressData->AddressRange)
      ExFreePool(AsyncAddressData->AddressRange);

      if (AsyncAddressData)
      ExFreePool(AsyncAddressData);
      }
    */

    SLAyer_harness_teardown();
  }

  return ntStatus;
}


