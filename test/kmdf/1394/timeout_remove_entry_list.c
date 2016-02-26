/*****************************************************************************
  Copyright (c) Microsoft Corporation.  All rights reserved.

  1394 Feature: timeout_remove_entry_list.
  Expected Result: SAFE, MAY LEAK.
 *****************************************************************************/

#include "harness.h"
#include "1394.h"


/* Source: WDK/src_5239/kmdf/1394/util.c, line 60. */
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
/*             TRACE(TL_TRACE, ("Entry 0x%x found on list 0x%x\n", Entry, List)); */
            return TRUE;
        }
    }

/*     TRACE(TL_TRACE, ("Entry 0x%x not found on list 0x%x\n", Entry, List)); */
    return FALSE;
}


/* Source: WDK/src_5239/kmdf/1394/isochapi.c, line 1238. */
void
t1394_IsochCleanup(
    /* IN */ PISOCH_DETACH_DATA   IsochDetachData
    )
{
    ULONG               i;
    PDEVICE_EXTENSION   DeviceExtension;

/*     ENTER("t1394_IsochCleanup"); */

/*     DeviceExtension = IsochDetachData->DeviceExtension; */

    //
    // see if we need to detach this buffer
    //
    if ((!IsochDetachData) || (!DeviceExtension))
    {
        goto Exit_IsochDetachBuffers;
    }

/*     if (IsochDetachData->bDetach) */ {

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
    }
/*     else */ {

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
        {
            //IsochDetachData->Irp->IoStatus.Information = IsochDetachData->outputBufferLength;
        }

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
    }

Exit_IsochDetachBuffers:

/*     EXIT("t1394_IsochCleanup", 0) */;
} // t1394_IsochCleanup


/* Source: WDK/src_5239/kmdf/1394/isochapi.c, line 1178. */
void
t1394_IsochTimeout(
    PDEVICE_EXTENSION   DeviceExtension, /* added for SLAyer */
/*     IN PKDPC                Dpc, */
    /* IN */ PISOCH_DETACH_DATA   IsochDetachData/* , */
/*     IN PVOID                SystemArgument1, */
/*     IN PVOID                SystemArgument2 */
    )
{
/*     PDEVICE_EXTENSION   DeviceExtension; */

/*     UNREFERENCED_PARAMETER(Dpc); */
/*     UNREFERENCED_PARAMETER(SystemArgument1); */
/*     UNREFERENCED_PARAMETER(SystemArgument2); */

/*     ENTER("t1394_IsochTimeout"); */
/*     TRACE(TL_WARNING, ("Isoch Timeout!\n")); */

    //
    // ISSUE: the device extension we are referencing comes from the IsochDetachData
    // but it is possible this memory has been freed before we enter this function.
    // The only way to check is to validate against our DeviceExtension->IsochDetachList
    // but if the IsochDetachData has been freed then that won't be accessible
    //
/*     DeviceExtension = IsochDetachData->DeviceExtension; */
    if (DeviceExtension)
    {
        // make sure nobody else has already handled this request yet

		WdfSpinLockAcquire(DeviceExtension->IsochSpinLock);
        if (t1394_IsOnList(&IsochDetachData->IsochDetachList, &DeviceExtension->IsochDetachData))
        {
            RemoveEntryList(&IsochDetachData->IsochDetachList);

			WdfSpinLockRelease(DeviceExtension->IsochSpinLock);


/*             if(KeCancelTimer(&IsochDetachData->Timer)) */
            {

/*                 TRACE(TL_TRACE, ("IsochTimeout: IsochDetachData = 0x%x\n", IsochDetachData)); */
/*                 TRACE(TL_TRACE, ("IsochTimeout: IsochDetachData->Irp = 0x%x\n", IsochDetachData->Request)); */
/*                 TRACE(TL_TRACE, ("IsochTimeout: IsochDetachData->newIrp = 0x%x\n", IsochDetachData->newIrp)); */

                // need to save the status of the attach
                // we'll clean up in the same spot for success's and timeout's
/*                 IsochDetachData->AttachStatus = STATUS_TIMEOUT; */
                t1394_IsochCleanup(IsochDetachData);
            }
        }
        else
        {

			WdfSpinLockRelease(DeviceExtension->IsochSpinLock);

        }
    }

/*     EXIT("t1394_IsochTimeout", 0); */
} // t1394_IsochTimeout


/*
  Harness
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

int main()
{
  PDEVICE_EXTENSION  deviceExtension ;
  PISOCH_DETACH_DATA pidd;
  PISOCH_DETACH_DATA obj_to_del      = NULL;
  int                choice;
  unsigned int       count, i;
  NTSTATUS           status;

  // OS Model init
  status = SLAyer_harness_init();

  if (NT_SUCCESS(status)) {
    deviceExtension = GetDeviceContext(SL_Device_one);

    InitializeListHead(&(deviceExtension->IsochDetachData));

    // Add some IsochDetachData off deviceExtension.
    // Set obj_to_del to one of these.
    for (i=0; i<count; i++) {
      pidd = (PISOCH_DETACH_DATA)malloc(sizeof(ISOCH_DETACH_DATA));
      pidd->DeviceExtension = deviceExtension;
      if (choice) obj_to_del = pidd;
      InsertHeadList(&(deviceExtension->IsochDetachData),
                     &(pidd->IsochDetachList));
    }
    if (!obj_to_del) obj_to_del = pidd;

    t1394_IsochTimeout(deviceExtension, obj_to_del);

    // Now set obj_to_del to a new IsochDetachData.
    obj_to_del = (PISOCH_DETACH_DATA)malloc(sizeof(ISOCH_DETACH_DATA));
    obj_to_del->DeviceExtension = deviceExtension;
    InitializeListHead(&(obj_to_del->IsochDetachList));

    t1394_IsochTimeout(deviceExtension, obj_to_del);

    // OS Model teardown
    SLAyer_harness_teardown();
  }

  return (NT_SUCCESS(status));;

}
