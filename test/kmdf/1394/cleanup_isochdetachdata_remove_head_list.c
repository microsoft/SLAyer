/*****************************************************************************
  Copyright (c) Microsoft Corporation.  All rights reserved.

  1394 Feature: cleanup_isochdetachdata_remove_head_list.
  Expected Result: SAFE.
 *****************************************************************************/

#include "harness.h"
#include "1394.h"
#include "common.h"


/* Source: WDK/src_5239/kmdf/1394/isochapi.c, line 1344. */
NTSTATUS
t1394_IsochDetachCompletionRoutine(
    /* IN */ PDEVICE_OBJECT       Device,
    /* IN */ PIRP                 Irp,
    /* IN */ PISOCH_DETACH_DATA   IsochDetachData
    )
{
    NTSTATUS        ntStatus = STATUS_SUCCESS;
    ULONG           i;

/*     UNREFERENCED_PARAMETER(Device); */
/*     UNREFERENCED_PARAMETER(Irp); */
/*     UNREFERENCED_PARAMETER(ntStatus); */

/*     ENTER("t1394_IsochDetachCompletionRoutine"); */

    if (!IsochDetachData) {

/*         TRACE(TL_WARNING, ("Invalid IsochDetachData\n")); */
        goto Exit_IsochDetachCompletionRoutine;
    }

/*     if (IsochDetachData->DetachIrb) */
/*     { */
/*         ExFreePool(IsochDetachData->DetachIrb); */
/*     } */
/*     TRACE(TL_TRACE, ("Now lets complete the Irp.\n")); */

/*     if (IsochDetachData->AttachIrb) */
/*         ExFreePool(IsochDetachData->AttachIrb); */

/*     for (i=0; i<IsochDetachData->numIsochDescriptors; i++) */
/*     { */

/*         if (IsochDetachData->IsochDescriptor[i].Mdl) */
/*         { */
/*             IoFreeMdl(IsochDetachData->IsochDescriptor[i].Mdl); */
/*         } */
/*     } */

/*     if (IsochDetachData->IsochDescriptor) */
/*     { */
/*         ExFreePool(IsochDetachData->IsochDescriptor); */
/*     } */

    // only set this if its a success...
/*     if (NT_SUCCESS(IsochDetachData->AttachStatus)) */
/*     { */
/*         //IsochDetachData->Irp->IoStatus.Information = IsochDetachData->outputBufferLength; */
/*     } */

    //IsochDetachData->Irp->IoStatus.Status = IsochDetachData->AttachStatus;

    //
    // Complete original Irp and free the one we allocated in
    // IsochAttachBuffers
    //
/*     WdfRequestCompleteWithInformation(IsochDetachData->Request, IsochDetachData->AttachStatus, IsochDetachData->outputBufferLength); */
/*     IoFreeIrp (IsochDetachData->newIrp); */

    // all done with IsochDetachData, lets deallocate it...
    if (IsochDetachData)
    {
        ExFreePool(IsochDetachData);
    }

Exit_IsochDetachCompletionRoutine:

/*     EXIT("t1394_IsochDetachCompletionRoutine", ntStatus); */
    return(STATUS_MORE_PROCESSING_REQUIRED);
} // t1394_IsochDetachCompletionRoutine


/* Source: WDK/src_5239/kmdf/1394/isochapi.c, line 1238. */
void
t1394_IsochCleanup(
    /* IN */ PISOCH_DETACH_DATA   IsochDetachData
    )
{
/*     ULONG               i; */
/*     PDEVICE_EXTENSION   DeviceExtension; */

/*     ENTER("t1394_IsochCleanup"); */

/*     DeviceExtension = IsochDetachData->DeviceExtension; */

    //
    // see if we need to detach this buffer
    //
    if ((!IsochDetachData) /* || (!DeviceExtension) */)
    {
        goto Exit_IsochDetachBuffers;
    }

/*     if (IsochDetachData->bDetach) { */

/*         PIRB                pIrb; */
/*         NTSTATUS            ntStatus; */
/*         PIO_STACK_LOCATION  NextIrpStack; */

/*         pIrb = ExAllocatePoolWithTag(NonPagedPool, sizeof(IRB), POOLTAG_1394); */

/*         if (!pIrb) { */

/*             TRACE(TL_ERROR, ("Failed to allocate pIrb!\n")); */
/*             TRACE(TL_WARNING, ("Can't detach buffer!\n")); */
/*             ntStatus = STATUS_INSUFFICIENT_RESOURCES; */
/*             goto Exit_IsochDetachBuffers; */
/*         } // if */

        // save the irb in our detach data context
/*         IsochDetachData->DetachIrb = pIrb; */

/*         RtlZeroMemory (pIrb, sizeof (IRB)); */
/*         pIrb->FunctionNumber = REQUEST_ISOCH_DETACH_BUFFERS; */
/*         pIrb->Flags = 0; */
/*         pIrb->u.IsochDetachBuffers.hResource = IsochDetachData->hResource; */
/*         pIrb->u.IsochDetachBuffers.nNumberOfDescriptors = IsochDetachData->numIsochDescriptors; */
/*         pIrb->u.IsochDetachBuffers.pIsochDescriptor = IsochDetachData->IsochDescriptor; */

/*         NextIrpStack = IoGetNextIrpStackLocation(IsochDetachData->newIrp); */
/*         NextIrpStack->MajorFunction = IRP_MJ_INTERNAL_DEVICE_CONTROL; */
/*         NextIrpStack->Parameters.DeviceIoControl.IoControlCode = IOCTL_1394_CLASS; */
/*         NextIrpStack->Parameters.Others.Argument1 = pIrb; */

/*         IoSetCompletionRoutine( IsochDetachData->newIrp, */
/*                                 t1394_IsochDetachCompletionRoutine, */
/*                                 IsochDetachData, */
/*                                 TRUE, */
/*                                 TRUE, */
/*                                 TRUE */
/*                                 ); */

/*         IoCallDriver(DeviceExtension->StackDeviceObject, IsochDetachData->newIrp); */
/*     } */
/*     else { */

/*         TRACE(TL_TRACE, ("Complete Irp.\n")); */

/*         if (IsochDetachData->AttachIrb) */
/*         { */
/*             ExFreePool(IsochDetachData->AttachIrb); */
/*         } */

/*         for (i=0; i<IsochDetachData->numIsochDescriptors; i++) */
/*         { */
/*             if (IsochDetachData->IsochDescriptor[i].Mdl) */
/*             { */
/*                 IoFreeMdl(IsochDetachData->IsochDescriptor[i].Mdl); */
/*             } */
/*         } */

/*         ExFreePool(IsochDetachData->IsochDescriptor); */

        //IsochDetachData->Irp->IoStatus.Status = IsochDetachData->AttachStatus;

        // only set this if its a success...
/*         if (NT_SUCCESS(IsochDetachData->AttachStatus)) */
/*         { */
/* 	    //IsochDetachData->Irp->IoStatus.Information = IsochDetachData->outputBufferLength; */
/*         } */

        //
        // Complete original Irp and free the one we allocated in
        // IsochAttachBuffers
        //
/*         WdfRequestCompleteWithInformation(IsochDetachData->Request, */
/*                                           IsochDetachData->AttachStatus, */
/*                                           IsochDetachData->outputBufferLength); */
/*         IoFreeIrp (IsochDetachData->newIrp); */

        // all done with IsochDetachData, lets deallocate it...
        ExFreePool(IsochDetachData);
/*     } */

Exit_IsochDetachBuffers:

/*     EXIT("t1394_IsochCleanup", 0) */;
} // t1394_IsochCleanup


/* Source: WDK/src_5239/kmdf/1394/pnp.c, line 593. */
VOID
t1394_EvtDeviceSelfManagedIoCleanup(
/*     IN  WDFDEVICE Device */
    PDEVICE_EXTENSION   deviceExtension /* added for SLAyer */
    )
{
     PLIST_ENTRY         listEntry;

    //
    // Free up any attached isoch buffers
    // Note: There are known bugs in this code path
    //
    WHILE (TRUE) {

        WdfSpinLockAcquire(deviceExtension->IsochSpinLock);

        if (!IsListEmpty(&deviceExtension->IsochDetachData)) {

            PISOCH_DETACH_DATA  IsochDetachData;

/*             IsochDetachData = (PISOCH_DETACH_DATA) */
/*                 RemoveHeadList(&deviceExtension->IsochDetachData); */

/* SLAyer: This cast assumes IsochDetachList is the first field, replaced by: */
            listEntry = RemoveHeadList(&deviceExtension->IsochDetachData);

            IsochDetachData = CONTAINING_RECORD (
                listEntry,
                ISOCH_DETACH_DATA,
                IsochDetachList);
/* SLAyer: end replace */

/*             TRACE(TL_TRACE, ("Surprise Removal: IsochDetachData = 0x%x\n", */
/*                              IsochDetachData)); */

/*             KeCancelTimer(&IsochDetachData->Timer); */

            WdfSpinLockRelease(deviceExtension->IsochSpinLock);

/*             TRACE(TL_TRACE, ("Surprise Removal: IsochDetachData->Irp = 0x%x\n", */
/*                              IsochDetachData->Request)); */

            // need to save the status of the attach
            // we'll clean up in the same spot for success's and timeout's
/*             IsochDetachData->AttachStatus = STATUS_SUCCESS; */

            // detach no matter what...
/*             IsochDetachData->bDetach = TRUE; */

            t1394_IsochCleanup(IsochDetachData);
        }
        else {

            WdfSpinLockRelease(deviceExtension->IsochSpinLock);
            break;
        }
    }

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
  free(SL_Device_one->Context);
  free(SL_Device_one);
}

int main()
{
  NTSTATUS status;
  PDEVICE_EXTENSION devExt ;
  unsigned int      count, i;

  // OS Model init
  status = SLAyer_harness_init();

  if (NT_SUCCESS(status)) {
    devExt = GetDeviceContext(SL_Device_one);

    InitializeListHead(&(devExt->IsochDetachData));

    for (i=0; i<count; i++) {
      PISOCH_DETACH_DATA pdata =
        (PISOCH_DETACH_DATA)malloc(sizeof(ISOCH_DETACH_DATA));
      pdata->DeviceExtension = devExt;
      InsertHeadList(&(devExt->IsochDetachData), &(pdata->IsochDetachList));
    }

    t1394_EvtDeviceSelfManagedIoCleanup(devExt);

    // OS Model teardown
    SLAyer_harness_teardown();
  }

  return (NT_SUCCESS(status));
}

