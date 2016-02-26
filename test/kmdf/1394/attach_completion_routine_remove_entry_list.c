/*****************************************************************************
  Copyright (c) Microsoft Corporation.  All rights reserved.

  1394 Feature: attach_completion_routine_remove_entry_list.
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


/* Source: WDK/src_5239/kmdf/1394/isochapi.c, line 1416. */
NTSTATUS
t1394_IsochAttachCompletionRoutine(
    PDEVICE_EXTENSION   DeviceExtension, /* added for SLAyer */
/*     IN PDEVICE_OBJECT       Device, */
/*     IN PIRP                 Irp, */
    /* IN */ PISOCH_DETACH_DATA   IsochDetachData
    )
{
/*     PDEVICE_EXTENSION   DeviceExtension; */
    NTSTATUS            ntStatus = STATUS_SUCCESS;
    ULONG               i;

/*     UNREFERENCED_PARAMETER(Device); */
/*     UNREFERENCED_PARAMETER(Irp); */

/*     ENTER("t1394_IsochAttachCompletionRoutine"); */

    if (!IsochDetachData)
    {
/*         TRACE(TL_WARNING, ("IsochAttachCompletionRoutine: IsochDetachData = 0x%x\n",  */
/*                            IsochDetachData)); */
/*         IoFreeIrp (Irp); */
        goto Exit_IsochAttachCompletionRoutine;
    }

/*     if (!NT_SUCCESS(Irp->IoStatus.Status)) */
    {
        // make sure this irp is still on the device extension list, meaning no one else
        // has already handled this yet
/*         DeviceExtension = IsochDetachData->DeviceExtension; */

		WdfSpinLockAcquire(DeviceExtension->IsochSpinLock);
        if (t1394_IsOnList(&IsochDetachData->IsochDetachList, &DeviceExtension->IsochDetachData))
        {
            RemoveEntryList(&IsochDetachData->IsochDetachList);
/*             KeCancelTimer(&IsochDetachData->Timer); */

			WdfSpinLockRelease(DeviceExtension->IsochSpinLock);
        }
        else
        {
            // just bomb out here

			WdfSpinLockRelease(DeviceExtension->IsochSpinLock);
            goto Exit_IsochAttachCompletionRoutine;
        }
/*         TRACE(TL_ERROR, ("Isoch Attach Failed! = 0x%x\n", Irp->IoStatus.Status)); */
/*         ntStatus = Irp->IoStatus.Status; */

        if (!IsochDetachData)
        {
            goto Exit_IsochAttachCompletionRoutine;
        }

/*         DeviceExtension = IsochDetachData->DeviceExtension; */

/*         TRACE(TL_TRACE, ("IsochAttachCompletionRoutine: IsochDetachData = 0x%x\n", IsochDetachData)); */
/*         TRACE(TL_TRACE, ("IsochAttachCompletionRoutine: IsochDetachData->Request = 0x%x\n", IsochDetachData->Request)); */
/*         TRACE(TL_TRACE, ("IsochAttachCompletionRoutine: IsochDetachData->newIrp = 0x%x\n", IsochDetachData->newIrp)); */
/*         TRACE(TL_TRACE, ("Now lets complete Irp.\n")); */

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

        //
        // Complete original Irp and free the one we allocated in
        // IsochAttachBuffers
        //
        //IsochDetachData->Irp->IoStatus = Irp->IoStatus;
/*         WdfRequestCompleteWithInformation(IsochDetachData->Request, Irp->IoStatus.Status, 0); */
/*         IoFreeIrp (IsochDetachData->newIrp); */

        // all done with IsochDetachData, lets deallocate it...
        ExFreePool(IsochDetachData);
    }

Exit_IsochAttachCompletionRoutine:

/*     EXIT("t1394_IsochAttachCompletionRoutine", ntStatus); */
    return(STATUS_MORE_PROCESSING_REQUIRED);
} // t1394_IsochAttachCompletionRoutine


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
  NTSTATUS           ntStatus;
  PDEVICE_EXTENSION  deviceExtension;
  PISOCH_DETACH_DATA obj_to_del      = NULL;
  int                choice;
  unsigned int       count, i;
  PISOCH_DETACH_DATA pidd;

  // OS Model init
  ntStatus = SLAyer_harness_init();

  if (NT_SUCCESS(ntStatus)) {
    deviceExtension = GetDeviceContext(SL_Device_one);

    // Setup
    InitializeListHead(&(deviceExtension->IsochDetachData));

    // Add some IsochDetachData off deviceExtension.
    // Set obj_to_del to one of these.
    for (i=0; i<count; i++) {
      pidd = (PISOCH_DETACH_DATA)malloc(sizeof(ISOCH_DETACH_DATA));
      if (choice) obj_to_del = pidd;
      pidd->DeviceExtension = deviceExtension;
      InsertHeadList(&(deviceExtension->IsochDetachData),&(pidd->IsochDetachList));
    }
    if (!obj_to_del) obj_to_del = pidd;

    // assert: obj_to_del should now point to one of devicExtension->IsochResourceData.
    ntStatus = t1394_IsochAttachCompletionRoutine(deviceExtension, obj_to_del);
    // assert: devExt.IsochDetachData > devExt.IsochDeatchData'

    // Now set obj_to_del to something new.
    obj_to_del = (PISOCH_DETACH_DATA)malloc(sizeof(ISOCH_DETACH_DATA));
    obj_to_del->DeviceExtension = deviceExtension;
    InitializeListHead(&(obj_to_del->IsochDetachList));

    t1394_IsochAttachCompletionRoutine(deviceExtension, obj_to_del);
    // assert: devExt = devExt'

    SLAyer_harness_teardown();
  }

  return (NT_SUCCESS(ntStatus));

}
