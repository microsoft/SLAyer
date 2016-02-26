/*****************************************************************************
  Copyright (c) Microsoft Corporation.  All rights reserved.

  1394 Feature: free_resources_remove_entry_list.
  Expected Result: SAFE.
 *****************************************************************************/

#include "harness.h"
#include "1394.h"


/* Source: WDK/src_5239/kmdf/1394/isochapi.c, line 679. */
NTSTATUS
t1394_IsochFreeResources(
    PDEVICE_EXTENSION   deviceExtension, /* added for SLAyer */
/*     IN WDFDEVICE   Device, */
/*     IN WDFREQUEST  Request, */
    /* IN */ HANDLE      hResource
    )
{
    NTSTATUS              ntStatus            = STATUS_SUCCESS;
/*     PDEVICE_EXTENSION     deviceExtension     = GetDeviceContext(Device); */
    PIRB                  pIrb                = NULL;
    PISOCH_RESOURCE_DATA  IsochResourceData   = NULL;
    PLIST_ENTRY           listHead;
    PLIST_ENTRY           thisEntry;

/*     ENTER("t1394_IsochFreeResources"); */

/*     TRACE(TL_TRACE, ("hResource = 0x%x\n", hResource)); */

    pIrb = ExAllocatePoolWithTag(NonPagedPool, sizeof(IRB), POOLTAG_1394);

    if (!pIrb) {

/*         TRACE(TL_ERROR, ("Failed to allocate pIrb!\n")); */
        ntStatus = STATUS_INSUFFICIENT_RESOURCES;
        goto Exit_IsochFreeResources;
    } // if

    // remove this one from our list...

	WdfSpinLockAcquire(deviceExtension->IsochResourceSpinLock);

    listHead = &deviceExtension->IsochResourceData;

    for(thisEntry = listHead->Flink;
                    thisEntry != listHead;
                    IsochResourceData = NULL, thisEntry = thisEntry->Flink)
    {
        IsochResourceData = CONTAINING_RECORD(thisEntry,
                                              ISOCH_RESOURCE_DATA,
                                              IsochResourceList);

        if (IsochResourceData->hResource == hResource) {
/*             TRACE(TL_TRACE, ("Removing hResource = 0x%x\n", hResource)); */
            RemoveEntryList(&IsochResourceData->IsochResourceList);
            ExFreePool(IsochResourceData);
            break;
        }
    }


	WdfSpinLockRelease(deviceExtension->IsochResourceSpinLock);

    RtlZeroMemory (pIrb, sizeof (IRB));
/*     pIrb->FunctionNumber = REQUEST_ISOCH_FREE_RESOURCES; */
    pIrb->Flags = 0;
/*     pIrb->u.IsochFreeResources.hResource = hResource; */

/*     ntStatus = t1394_SubmitIrpSynch(deviceExtension->StackIoTarget, Request, pIrb); */

    if (!NT_SUCCESS(ntStatus)) {

/*         TRACE(TL_ERROR, ("SubmitIrpSync failed = 0x%x\n", ntStatus)); */
    }

Exit_IsochFreeResources:

    if (pIrb)
    {
        ExFreePool(pIrb);
    }

/*     EXIT("t1394_IsochFreeResources", ntStatus); */
    return(ntStatus);
} // t1394_IsochFreeResources



NTSTATUS SLAyer_harness_init()
{
  PWDFDEVICE_INIT DInit;
  WDF_OBJECT_ATTRIBUTES DAttrib;
  WDFDEVICE Device;
  NTSTATUS status;

  // malloc SL_Device
  WDF_OBJECT_ATTRIBUTES_INIT_CONTEXT_TYPE(&DAttrib,DEVICE_EXTENSION);
  status = WdfDeviceCreate(&DInit,&DAttrib,&Device);
  return status;
}

void SLAyer_harness_teardown()
{
  free(SL_Device_one->Context);
  free(SL_Device_one);
}

int main()
{
  PDEVICE_EXTENSION    devExt ;
  NTSTATUS             ntStatus        = STATUS_SUCCESS;
  PISOCH_RESOURCE_DATA pird;
  PISOCH_RESOURCE_DATA obj_to_del      = NULL;
  int                  choice;
  unsigned int         count, i; // count=*

  // OS Model init
  ntStatus = SLAyer_harness_init();

  if (NT_SUCCESS(ntStatus)) {
    devExt = GetDeviceContext(SL_Device_one);

    InitializeListHead(&(devExt->IsochResourceData));

    // Add some IsochResourceData off devExt.
    // Set obj_to_del to one of these.
    for (i=0; i<count; i++) {
      pird = (PISOCH_RESOURCE_DATA)malloc(sizeof(ISOCH_RESOURCE_DATA));
      if (choice) obj_to_del = pird;
      InsertHeadList(&(devExt->IsochResourceData),&(pird->IsochResourceList));
    }
    if (!obj_to_del) obj_to_del = pird;

    ntStatus = t1394_IsochFreeResources(devExt, obj_to_del);

    // Now set obj_to_del to something new.
    obj_to_del = (PISOCH_RESOURCE_DATA)malloc(sizeof(ISOCH_RESOURCE_DATA));
    InitializeListHead(&(obj_to_del->IsochResourceList));

    ntStatus = t1394_IsochFreeResources(devExt, obj_to_del);

    // OS Model teardown
    SLAyer_harness_teardown();
  }

  return (NT_SUCCESS(ntStatus));
}

