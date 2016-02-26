/*****************************************************************************
  Copyright (c) Microsoft Corporation.  All rights reserved.

  1394 Feature: cleanup_isochresourcedata_remove_head_list.
  Expected Result: SAFE.
 *****************************************************************************/

#include "harness.h"
#include "1394.h"
#include "common.h"

NTSTATUS
t1394_SubmitIrpSynch(
    IN WDFIOTARGET       IoTarget,
    IN WDFREQUEST        Request,
    IN PIRB              Irb
    )
{
    NTSTATUS            ntStatus /* = STATUS_SUCCESS */;

/*     WDF_REQUEST_SEND_OPTIONS    option; */
/*     WDF_MEMORY_DESCRIPTOR descriptor; */

/*     UNREFERENCED_PARAMETER(Request); */

/*     ENTER("t1394_SubmitIrpSynch"); */

/*     ASSERT(KeGetCurrentIrql() < DISPATCH_LEVEL); */
/*     ASSERT(Irb); */

/*     WDF_REQUEST_SEND_OPTIONS_INIT(&option, WDF_REQUEST_SEND_OPTION_SYNCHRONOUS); */

/*     TRACE(TL_TRACE, ("t1394_SubmitIrpSynch: Irp is pending...\n")); */

/*     WDF_MEMORY_DESCRIPTOR_INIT_BUFFER(&descriptor, Irb, sizeof (IRB)); */

/*     ntStatus = WdfIoTargetSendInternalIoctlOthersSynchronously(IoTarget, NULL, IOCTL_1394_CLASS, &descriptor, NULL, NULL, &option, NULL); */
/*     if (!NT_SUCCESS(ntStatus)) { */
/*             TRACE(TL_ERROR, ("WdfIoTargetSendInternalIoctlSynchronouslyOthers Failed with status %x\n",ntStatus)); */
/*     } */

/*     EXIT("t1394_SubmitIrpSynch", ntStatus); */
    return(ntStatus);
} // t1394_SubmitIrpSynch

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

/*             listEntry = RemoveHeadList(&deviceExtension->CromData); */
/* SLAyer: above seems buggy, replaced by below */
            listEntry = RemoveHeadList(&deviceExtension->IsochResourceData);

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
  if (NULL != SL_Device_one->Context) { free(SL_Device_one->Context); }
  free(SL_Device_one);
}

NTSTATUS
main()
{
  NTSTATUS         status;
  PDEVICE_EXTENSION devExt;
  unsigned int     count, i;

  status = SLAyer_harness_init();

  if (NT_SUCCESS(status)) {
    devExt = GetDeviceContext(SL_Device_one);

    InitializeListHead(&(devExt->IsochResourceData));

    for (i=0; i<count; i++) {
      PISOCH_RESOURCE_DATA pdata = (PISOCH_RESOURCE_DATA)malloc(sizeof(ISOCH_RESOURCE_DATA));
      InsertHeadList(&(devExt->IsochResourceData),&(pdata->IsochResourceList));
    }

    // 2. Delete all Isochresourcedatas.
    t1394_EvtDeviceSelfManagedIoCleanup(devExt);

    SLAyer_harness_teardown();
  }



  return STATUS_SUCCESS;
}


