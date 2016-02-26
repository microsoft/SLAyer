/* Copyright (c) Microsoft Corporation.  All rights reserved. */

/*****************************************************************************
  1394 Feature: cleanup_isochresourcedata_remove_head_list_unsafe.
  Expected Result: POSSIBLY UNSAFE.
 *****************************************************************************/

#include "harness.h"
#include "1394.h"

#define WHILE while
#define TRUE (1==1)

NTSTATUS t1394_SubmitIrpSynch(WDFIOTARGET IoTarget, WDFREQUEST Request, PIRB Irb)
{
  int x;
  if (x) {
    return STATUS_SUCCESS;
  } else {
    return STATUS_UNSUCCESSFUL;
  }
}

/* Source: WDK/src_5239/kmdf/1394/pnp.c, line 634. */
VOID
t1394_EvtDeviceSelfManagedIoCleanup(
/*     IN  WDFDEVICE Device */
    PDEVICE_EXTENSION   deviceExtension /* added for SLAyer */
    )
{
     PLIST_ENTRY         listEntry;

    //
    // Remove any isoch resource data
    //
    WHILE (TRUE) {

        WdfSpinLockAcquire(deviceExtension->IsochResourceSpinLock);

        if (!IsListEmpty(&deviceExtension->IsochResourceData)) {

            PISOCH_RESOURCE_DATA    IsochResourceData = NULL;

/* SLAyer: memory unsafety in original code. Reported as Windows 8 Bug #59410. Fixed by Patrick Maninger 9/Aug/2010.  */
	    listEntry = RemoveHeadList(&deviceExtension->CromData);

            IsochResourceData = CONTAINING_RECORD(listEntry,
						  ISOCH_RESOURCE_DATA,
						  IsochResourceList);

            WdfSpinLockRelease(deviceExtension->IsochResourceSpinLock);

/*             TRACE(TL_TRACE, ("Surprise Removal: IsochResourceData = 0x%x\n", */
/*                              IsochResourceData)); */

            if (IsochResourceData) {

                PIRB          pIrb;
                WDFREQUEST    request;
	         NTSTATUS status;

/*                 TRACE(TL_TRACE, ("Surprise Removal: Freeing hResource = 0x%x\n", */
/*                                  IsochResourceData->hResource)); */

                status = WdfRequestCreate(
                    WDF_NO_OBJECT_ATTRIBUTES,
                    deviceExtension->StackIoTarget,
                    &request);

                if (!NT_SUCCESS(status)) {
/*                     TRACE(TL_ERROR, ("Failed to allocate request %x\n", status)); */
			free(IsochResourceData); /* SLAyer: added */
                }
                else {

                    pIrb = ExAllocatePoolWithTag(NonPagedPool, sizeof(IRB), POOLTAG_1394);

                    if (!pIrb) {

                        WdfObjectDelete(request);

/*                         TRACE(TL_ERROR, ("Failed to allocate pIrb!\n")); */
			free(IsochResourceData); /* SLAyer: added */
                    }
                    else {

                        RtlZeroMemory (pIrb, sizeof (IRB));
                        pIrb->FunctionNumber = REQUEST_ISOCH_FREE_RESOURCES;
                        pIrb->Flags = 0;
                        pIrb->u.IsochFreeResources.hResource = IsochResourceData->hResource;

                        status = t1394_SubmitIrpSynch(deviceExtension->StackIoTarget, request, pIrb);
			free(IsochResourceData); /* SLAyer: added */

                        if (!NT_SUCCESS(status)) {

/*                             TRACE(TL_ERROR, ("SubmitIrpSync failed = 0x%x\n", status)); */
                        }

                        ExFreePool(pIrb);
                        WdfObjectDelete(request);
                    }
                }
            }
        }
        else {


            WdfSpinLockRelease(deviceExtension->IsochResourceSpinLock);
            break;
        }
    }

/*     EXIT("t1394_PnpRemoveDevice", STATUS_SUCCESS); */

}


NTSTATUS
main()
{
  DEVICE_EXTENSION devExt;
  unsigned int count, i; // count=*

  InitializeListHead(&(devExt.IsochResourceData));

  for (i=0; i<count; i++) {
      PISOCH_RESOURCE_DATA pdata = (PISOCH_RESOURCE_DATA)malloc(sizeof(ISOCH_RESOURCE_DATA));
      InsertHeadList(&(devExt.IsochResourceData),&(pdata->IsochResourceList));
  }

  // 2. Delete all Isochresourcedatas.
  t1394_EvtDeviceSelfManagedIoCleanup(&devExt);

  return STATUS_SUCCESS;
}


