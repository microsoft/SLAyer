/******************************************************************************
  Copyright (c) Microsoft Corporation.  All rights reserved.

  t1394_SetLocalHostProperties procedure.

 ******************************************************************************/

#include "harness.h"
#include "1394.h"

NTSTATUS t1394_SubmitIrpSynch(WDFIOTARGET IoTarget, WDFREQUEST Request, PIRB Irb)
{
  int x;
  if (x) {
    return STATUS_SUCCESS;
  } else {
    return STATUS_UNSUCCESSFUL;
  }
}

NTSTATUS
t1394_SetLocalHostProperties(
    /*IN*/ WDFDEVICE   Device,
    /*IN*/ WDFREQUEST             Request,
    /*IN*/ ULONG            nLevel,
    /*IN*/ PVOID            Information
    )
{
    NTSTATUS                ntStatus                = STATUS_SUCCESS;
    PDEVICE_EXTENSION       deviceExtension         = GetDeviceContext(Device);
    PIRB                    pIrb                    = NULL;
    PSET_LOCAL_HOST_PROPS3  R0_SetLocalHostProps3   = NULL;
    PCROM_DATA              CromData                = NULL;
    PLIST_ENTRY listHead, thisEntry;

    //ENTER("t1394_SetLocalHostProperties");

    // allocate irb
    pIrb = ExAllocatePoolWithTag(NonPagedPool, sizeof(IRB), POOLTAG_1394);
    if (!pIrb) {

        // TRACE(TL_ERROR, ("Failed to allocate pIrb!\n"));
        ntStatus = STATUS_INSUFFICIENT_RESOURCES;
        goto Exit_SetLocalHostProperties;
    } // if



    RtlZeroMemory (pIrb, sizeof (IRB));
    pIrb->FunctionNumber = REQUEST_SET_LOCAL_HOST_PROPERTIES;
    pIrb->Flags = 0;
    pIrb->u.SetLocalHostProperties.nLevel = nLevel;

    // TRACE(TL_TRACE, ("nLevel = 0x%x\n", nLevel));
    // TRACE(TL_TRACE, ("Information = 0x%x\n", Information));

    if (nLevel == SET_LOCAL_HOST_PROPERTIES_GAP_COUNT) {

        PSET_LOCAL_HOST_PROPS2  SetLocalHostProps2;

        SetLocalHostProps2 = (PSET_LOCAL_HOST_PROPS2)Information;

        // TRACE(TL_TRACE, ("GapCountLowerBound = 0x%x\n", SetLocalHostProps2->GapCountLowerBound));

        pIrb->u.SetLocalHostProperties.Information = Information;
    }
    else if (nLevel == SET_LOCAL_HOST_PROPERTIES_MODIFY_CROM) {

        PSET_LOCAL_HOST_PROPS3  SetLocalHostProps3;

        SetLocalHostProps3 = (PSET_LOCAL_HOST_PROPS3)Information;

        // TRACE(TL_TRACE, ("fulFlags = 0x%x\n", SetLocalHostProps3->fulFlags));
        // TRACE(TL_TRACE, ("hCromData = 0x%x\n", SetLocalHostProps3->hCromData));
        // TRACE(TL_TRACE, ("nLength = 0x%x\n", SetLocalHostProps3->nLength));

        // since we need to create a mdl, we'll create another setlocalhostprops3
        // and pass that down to the bus driver
        R0_SetLocalHostProps3 = ExAllocatePoolWithTag(NonPagedPool,
                                                      sizeof(SET_LOCAL_HOST_PROPS3),
                                                      POOLTAG_1394);

        if (!R0_SetLocalHostProps3) {

            // TRACE(TL_ERROR, ("Failed to allocate R0_SetLocalHostProps3!\n"));
            if (pIrb)
                ExFreePool(pIrb);

            ntStatus = STATUS_INSUFFICIENT_RESOURCES;
            goto Exit_SetLocalHostProperties;
        } // if

        // TRACE(TL_TRACE, ("R0_SetLocalHostProps3 = 0x%x\n", R0_SetLocalHostProps3));

        // copy over the contents...
        RtlCopyMemory( R0_SetLocalHostProps3,
                       SetLocalHostProps3,
                       sizeof(SET_LOCAL_HOST_PROPS3)
                       );

        // branch, depending if we are adding or removing
        if (R0_SetLocalHostProps3->fulFlags == SLHP_FLAG_ADD_CROM_DATA) {

            // we are adding an entry. let's get our crom data struct...
            CromData = ExAllocatePoolWithTag(NonPagedPool,
                                             sizeof(CROM_DATA),
                                             POOLTAG_1394);

            if (!CromData) {

                // TRACE(TL_ERROR, ("Failed to allocate CromData!\n"));
                if (pIrb)
                    ExFreePool(pIrb);

                if (R0_SetLocalHostProps3)
                    ExFreePool(R0_SetLocalHostProps3);

                ntStatus = STATUS_INSUFFICIENT_RESOURCES;
                goto Exit_SetLocalHostProperties;
            }

            // let's allocate our buffer...
            CromData->Buffer = ExAllocatePoolWithTag(NonPagedPool,
                                                     R0_SetLocalHostProps3->nLength,
                                                     POOLTAG_1394);

            // TRACE(TL_TRACE, ("CromData->Buffer = 0x%x\n", CromData->Buffer));

            if (!CromData->Buffer) {

                // TRACE(TL_ERROR, ("Failed to allocate CromData->Buffer!\n"));
                if (pIrb)
                    ExFreePool(pIrb);

                if (R0_SetLocalHostProps3)
                    ExFreePool(R0_SetLocalHostProps3);

                if (CromData)
                    ExFreePool(CromData);

                ntStatus = STATUS_INSUFFICIENT_RESOURCES;
                goto Exit_SetLocalHostProperties;
            }

            // copy over contents (mdl == ring 3 buffer)
            RtlCopyMemory(CromData->Buffer,
                          &SetLocalHostProps3->Mdl,
                          SetLocalHostProps3->nLength);

            R0_SetLocalHostProps3->Mdl = IoAllocateMdl (CromData->Buffer,
                                                        R0_SetLocalHostProps3->nLength,
                                                        FALSE,
                                                        FALSE,
                                                        NULL);
            if(R0_SetLocalHostProps3->Mdl == NULL) {

                // TRACE(TL_ERROR, ("Failed to allocate mdl for CromData->Buffer!\n"));
                if (pIrb)
                    ExFreePool(pIrb);

                if (R0_SetLocalHostProps3)
                    ExFreePool(R0_SetLocalHostProps3);

                if (CromData)
                    ExFreePool(CromData);

                ntStatus = STATUS_INSUFFICIENT_RESOURCES;
                goto Exit_SetLocalHostProperties;

            }
            MmBuildMdlForNonPagedPool(R0_SetLocalHostProps3->Mdl);

            // TRACE(TL_TRACE, ("Mdl = 0x%x\n", R0_SetLocalHostProps3->Mdl));
        }
        else if (SetLocalHostProps3->fulFlags == SLHP_FLAG_REMOVE_CROM_DATA) {

            // TRACE(TL_TRACE, ("hCromData = 0x%x\n", R0_SetLocalHostProps3->hCromData));
        }

        pIrb->u.SetLocalHostProperties.Information = (PVOID)R0_SetLocalHostProps3;
    }

    ntStatus = t1394_SubmitIrpSynch(deviceExtension->StackIoTarget, Request, pIrb);

    if (!NT_SUCCESS(ntStatus)) {

        if (nLevel == SET_LOCAL_HOST_PROPERTIES_MODIFY_CROM) {

            if (R0_SetLocalHostProps3 &&
                R0_SetLocalHostProps3->fulFlags == SLHP_FLAG_ADD_CROM_DATA) {

                if (R0_SetLocalHostProps3->Mdl)
                    IoFreeMdl(R0_SetLocalHostProps3->Mdl);

                if (CromData) {
                    if (CromData->Buffer) {
                        ExFreePool(CromData->Buffer);
                    }
                    ExFreePool(CromData);
                }
            }

            if (R0_SetLocalHostProps3)
                ExFreePool(R0_SetLocalHostProps3);
        }

        // TRACE(TL_ERROR, ("SubmitIrpSync failed = 0x%x\n", ntStatus));
    }
    else {

        if (nLevel == SET_LOCAL_HOST_PROPERTIES_MODIFY_CROM) {
            //
            // branch, depending if we are adding or removing
            //
            if (R0_SetLocalHostProps3 &&
                R0_SetLocalHostProps3->fulFlags == SLHP_FLAG_ADD_CROM_DATA) {

                PSET_LOCAL_HOST_PROPS3  SetLocalHostProps3;

                SetLocalHostProps3 = Information;
                SetLocalHostProps3->hCromData = R0_SetLocalHostProps3->hCromData;

                // TRACE(TL_TRACE, ("hCromData = 0x%x\n", SetLocalHostProps3->hCromData));

                if (CromData) {

                    CromData->hCromData = SetLocalHostProps3->hCromData;
                    CromData->pMdl = R0_SetLocalHostProps3->Mdl;

                    // need to add to our list...

                    WdfSpinLockAcquire(deviceExtension->CromSpinLock);
                    InsertHeadList(&deviceExtension->CromData, &CromData->CromList);

                    WdfSpinLockRelease(deviceExtension->CromSpinLock);
                }
            }
            else if (R0_SetLocalHostProps3 &&
                     R0_SetLocalHostProps3->fulFlags == SLHP_FLAG_REMOVE_CROM_DATA) {

                // have to find our struct...

                WdfSpinLockAcquire(deviceExtension->CromSpinLock);

                listHead = &deviceExtension->CromData;

                for(thisEntry = listHead->Flink;
                                thisEntry != listHead;
                                CromData = NULL, thisEntry = thisEntry->Flink)
                {
                    CromData = CONTAINING_RECORD(thisEntry, CROM_DATA, CromList);
                    if (CromData->hCromData == R0_SetLocalHostProps3->hCromData) {
                        RemoveEntryList(&CromData->CromList);
                        break;
                    }
                }

                WdfSpinLockRelease(deviceExtension->CromSpinLock);

                if (CromData) {

                    if (CromData->Buffer)
                        ExFreePool(CromData->Buffer);

                    if (CromData->pMdl)
                        IoFreeMdl(CromData->pMdl);

                    ExFreePool(CromData);
                }
            }

            if (R0_SetLocalHostProps3)
                ExFreePool(R0_SetLocalHostProps3);
        }
    }


Exit_SetLocalHostProperties:

    if (pIrb)
    {
        ExFreePool(pIrb);
    }


    //EXIT("t1394_SetLocalHostProperties", ntStatus);
    return(ntStatus);
} // t1394_SetLocalHostProperties


/******************************************************************************

  WDF_IO_QUEUE_IO_DEVICE_CONTROL

  The IOCTL_SET_LOCAL_HOST_INFORMATION part of EvtIoDeviceControl.
  Tranform the Request into a SetLocalHostInformation, and pass it down to
  the local t1394_SetLocalHostProperties procedure.

 ******************************************************************************/
VOID
t1394_EvtIoDeviceControl(
   /*IN*/WDFQUEUE     Queue,
   /*IN*/WDFREQUEST Request,
   /*IN*/size_t      OutputBufferLength,
   /*IN*/size_t      InputBufferLength,
   /*IN*/ULONG      IoControlCode
    )
{
    NTSTATUS            ntStatus = STATUS_SUCCESS;
    PDEVICE_EXTENSION   deviceExtension;
    PVOID               ioBuffer = NULL;
    WDFDEVICE           device;
    size_t              bufLength;

    //ENTER("t1394_EvtIoDeviceControl");
    // TRACE(TL_TRACE, ("Request = 0x%p\n", Request));

    device = WdfIoQueueGetDevice(Queue);
    deviceExtension = GetDeviceContext(device);

    //
    // Since all the IOCTLs handled here are buffered, WdfRequestRetrieveOutputBuffer &
    // WdfRequestRetrieveInputBuffer return the same buffer pointer.
    // So make sure you read all the information you need from
    // the buffer before you write to it. Also requiredLength of the buffer vary from
    // ioctl to ioctl, so we will pretend that we need zero length buffer and do the lenght
    // check later in the specific ioct case.
    //
    ntStatus = WdfRequestRetrieveInputBuffer(Request, 0, &ioBuffer, &bufLength);
    if( !NT_SUCCESS(ntStatus) || ioBuffer == NULL) {
        // TRACE(TL_ERROR, ("WdfRequestRetrieveInputBuffer failed 0x%x\n", ntStatus));
        WdfRequestComplete(Request, ntStatus);
        return;
    }


    switch (IoControlCode) {

        case IOCTL_SET_LOCAL_HOST_INFORMATION:
            {
                PSET_LOCAL_HOST_INFORMATION     SetLocalHostInformation;

                // TRACE(TL_TRACE, ("IOCTL_SET_LOCAL_HOST_INFORMATION\n"));

                if (InputBufferLength < sizeof(SET_LOCAL_HOST_INFORMATION)) {

                    ntStatus = STATUS_BUFFER_TOO_SMALL;
                }
                else {

                    SetLocalHostInformation = (PSET_LOCAL_HOST_INFORMATION)ioBuffer;

                    if (InputBufferLength < (sizeof(SET_LOCAL_HOST_INFORMATION) +
                                             SetLocalHostInformation->ulBufferSize)) {

                        ntStatus = STATUS_BUFFER_TOO_SMALL;
                    }
                    else {

                        ntStatus = t1394_SetLocalHostProperties( device,
                                                                     Request,
                                                                     SetLocalHostInformation->nLevel,
                                                                     (PVOID)&SetLocalHostInformation->Information
                                                                     );

                        if (NT_SUCCESS(ntStatus))
                            WdfRequestSetInformation(Request,  OutputBufferLength);
                    }
                }
            }
            break; // IOCTL_SET_LOCAL_HOST_INFORMATION

        default:
            // TRACE(TL_ERROR, ("Invalid ioControlCode = 0x%x\n", IoControlCode));
            ntStatus = STATUS_INVALID_PARAMETER;
            break; // default

    } // switch


    // only complete if the device is there
    if (ntStatus != STATUS_PENDING) {

        WdfRequestComplete(Request, ntStatus);
    }

    //EXIT("t1394_IoControl", ntStatus);
    return ;
} // t1394_EvtIoDeviceControl


/******************************************************************************

  WDF_DEVICE_SEFL_MANAGED_IO_CLEANUP
  (Only the CromData cleanup part of it)

 ******************************************************************************/
VOID
t1394_EvtDeviceSelfManagedIoCleanup(
   /*IN*/ WDFDEVICE Device
    )
/*++

Routine Description:

    EvtDeviceSelfManagedIoCleanup is called by the Framework when the device is
    being torn down, either in response to IRP_MN_REMOVE_DEVICE or
    IRP_MN_SURPRISE_REMOVE_DEVICE.  It will be called only once.  Its job is to
    stop all outstanding I/O in the driver that the Framework is not managing.

Arguments:

    Device - Handle to a framework device object.

Return Value:

    None

--*/
{
    PDEVICE_EXTENSION   deviceExtension;
     PLIST_ENTRY         listEntry;

    //ENTER("t1394_PnpRemoveDevice");

    deviceExtension = GetDeviceContext(Device);

    // TRACE(TL_WARNING, ("Removing 1394VDEV.SYS.\n"));

    // lets free up any crom data structs we've allocated...

    WdfSpinLockAcquire(deviceExtension->CromSpinLock);

    while (!IsListEmpty(&deviceExtension->CromData)) {

        PCROM_DATA      CromData;

        // get struct off list

        listEntry = RemoveHeadList(&deviceExtension->CromData);
        CromData = CONTAINING_RECORD(listEntry, CROM_DATA, CromList);

        // need to free up everything associated with this allocate...
        if (CromData)
        {
	  if (CromData->Buffer) {
                ExFreePool(CromData->Buffer);
	  }
	  if (CromData->pMdl) {
	    IoFreeMdl(CromData->pMdl);
	  }
	  // we already checked CromData
	  ExFreePool(CromData);
        }
    }


    WdfSpinLockRelease(deviceExtension->CromSpinLock);

/*     // lets free up any allocated addresses and deallocate all */
/*     // memory associated with them... */

/*     WdfSpinLockAcquire(deviceExtension->AsyncSpinLock); */

/*     while (!IsListEmpty(&deviceExtension->AsyncAddressData)) { */

/*         PASYNC_ADDRESS_DATA     AsyncAddressData; */

/*         // get struct off list */
/*         listEntry = RemoveHeadList(&deviceExtension->AsyncAddressData); */

/*         AsyncAddressData = CONTAINING_RECORD(listEntry, ASYNC_ADDRESS_DATA, */
/*                                                                             AsyncAddressList); */

/*         // need to free up everything associated with this allocate... */
/*         if (AsyncAddressData->pMdl) */
/*             IoFreeMdl(AsyncAddressData->pMdl); */

/*         if (AsyncAddressData->Buffer) */
/*             ExFreePool(AsyncAddressData->Buffer); */

/*         if (AsyncAddressData->AddressRange) */
/*             ExFreePool(AsyncAddressData->AddressRange); */

/*         if (AsyncAddressData) */
/*             ExFreePool(AsyncAddressData); */
/*     } */


/*     WdfSpinLockRelease(deviceExtension->AsyncSpinLock); */

/*     // */
/*     // Free up any attached isoch buffers */
/*     // Note: There are known bugs in this code path */
/*     // */
/*     WHILE (TRUE) { */

/*         WdfSpinLockAcquire(deviceExtension->IsochSpinLock); */

/*         if (!IsListEmpty(&deviceExtension->IsochDetachData)) { */

/*             PISOCH_DETACH_DATA  IsochDetachData; */

/*             IsochDetachData = (PISOCH_DETACH_DATA) */
/*                 RemoveHeadList(&deviceExtension->IsochDetachData); */

/*             // TRACE(TL_TRACE, ("Surprise Removal: IsochDetachData = 0x%x\n", */
/*                              IsochDetachData)); */

/*             KeCancelTimer(&IsochDetachData->Timer); */

/*             WdfSpinLockRelease(deviceExtension->IsochSpinLock); */

/*             // TRACE(TL_TRACE, ("Surprise Removal: IsochDetachData->Irp = 0x%x\n", */
/*                              IsochDetachData->Request)); */

/*             // need to save the status of the attach */
/*             // we'll clean up in the same spot for success's and timeout's */
/*             IsochDetachData->AttachStatus = STATUS_SUCCESS; */

/*             // detach no matter what... */
/*             IsochDetachData->bDetach = TRUE; */

/*             t1394_IsochCleanup(IsochDetachData); */
/*         } */
/*         else { */

/*             WdfSpinLockRelease(deviceExtension->IsochSpinLock); */
/*             break; */
/*         } */
/*     } */

/*     // */
/*     // Remove any isoch resource data */
/*     // */
/*     WHILE (TRUE) { */

/*         WdfSpinLockAcquire(deviceExtension->IsochResourceSpinLock); */

/*         if (!IsListEmpty(&deviceExtension->IsochResourceData)) { */

/*             PISOCH_RESOURCE_DATA    IsochResourceData = NULL; */

/*             listEntry = RemoveHeadList(&deviceExtension->CromData); */

/*             IsochResourceData = CONTAINING_RECORD(listEntry, */
/*                                                   ISOCH_RESOURCE_DATA, */
/*                                                   IsochResourceList); */

/*             WdfSpinLockRelease(deviceExtension->IsochResourceSpinLock); */

/*             // TRACE(TL_TRACE, ("Surprise Removal: IsochResourceData = 0x%x\n", */
/*                              IsochResourceData)); */

/*             if (IsochResourceData) { */

/*                 PIRB          pIrb; */
/*                 WDFREQUEST    request; */
/* 	         NTSTATUS status; */

/*                 // TRACE(TL_TRACE, ("Surprise Removal: Freeing hResource = 0x%x\n", */
/*                                  IsochResourceData->hResource)); */

/*                 status = WdfRequestCreate( */
/*                     WDF_NO_OBJECT_ATTRIBUTES, */
/*                     deviceExtension->StackIoTarget, */
/*                     &request); */

/*                 if (!NT_SUCCESS(status)) { */
/*                     // TRACE(TL_ERROR, ("Failed to allocate request %x\n", status)); */
/*                 } */
/*                 else { */

/*                     pIrb = ExAllocatePoolWithTag(NonPagedPool, sizeof(IRB), POOLTAG_1394); */

/*                     if (!pIrb) { */

/*                         WdfObjectDelete(request); */

/*                         // TRACE(TL_ERROR, ("Failed to allocate pIrb!\n")); */
/*                     } */
/*                     else { */

/*                         RtlZeroMemory (pIrb, sizeof (IRB)); */
/*                         pIrb->FunctionNumber = REQUEST_ISOCH_FREE_RESOURCES; */
/*                         pIrb->Flags = 0; */
/*                         pIrb->u.IsochFreeResources.hResource = IsochResourceData->hResource; */

/*                         status = t1394_SubmitIrpSynch(deviceExtension->StackIoTarget, request, pIrb); */

/*                         if (!NT_SUCCESS(status)) { */

/*                             // TRACE(TL_ERROR, ("SubmitIrpSync failed = 0x%x\n", status)); */
/*                         } */

/*                         ExFreePool(pIrb); */
/*                         WdfObjectDelete(request); */
/*                     } */
/*                 } */
/*             } */
/*         } */
/*         else { */


/*             WdfSpinLockRelease(deviceExtension->IsochResourceSpinLock); */
/*             break; */
/*         } */
/*     } */

    //EXIT("t1394_PnpRemoveDevice", STATUS_SUCCESS);

} // t1394_PnpRemoveDevice



/******************************************************************************

  WDF_DRIVER_DEVICE_ADD

 ******************************************************************************/


NTSTATUS
t1394_EvtPrepareHardware (
    WDFDEVICE      Device,
    WDFCMRESLIST   Resources,
    WDFCMRESLIST   ResourcesTranslated
    )
/*++

Routine Description:

    EvtDeviceStart event callback performs operations that are necessary
    to make the driver's device operational. The framework calls the driver's
    EvtDeviceStart callback when the PnP manager sends an IRP_MN_START_DEVICE
    request to the driver stack.

Arguments:

    Device - Handle to a framework device object.

Return Value:

    WDF status code

--*/
{
    NTSTATUS            status = STATUS_SUCCESS;
    PDEVICE_EXTENSION   deviceExtension;

/*     UNREFERENCED_PARAMETER(Resources); */
/*     UNREFERENCED_PARAMETER(ResourcesTranslated); */

/*     VERIFY_IS_IRQL_PASSIVE_LEVEL(); */

/*     TRACE(TL_TRACE, ( "--> t1394_EvtPrepareHardware\n")); */

/*     deviceExtension = GetDeviceContext(Device); */

/*     status = t1394_BusResetNotification( Device, */
/*                                          NULL, */
/*                                          REGISTER_NOTIFICATION_ROUTINE ); */

/*     TRACE(TL_TRACE, ( "<-- t1394_EvtPrepareHardware\n")); */

    return status;
}

NTSTATUS
t1394_EvtReleaseHardware(
    /*IN*/  WDFDEVICE Device,
    /*IN*/  WDFCMRESLIST ResourcesTranslated
    )
/*++

Routine Description:

    EvtDeviceReleaseHardware is called by the framework whenever the PnP manager
    is revoking ownership of our resources.  This may be in response to either
    IRP_MN_STOP_DEVICE or IRP_MN_REMOVE_DEVICE.  The callback is made before
    passing down the IRP to the lower driver.

    In this callback, do anything necessary to free those resources.

Arguments:

    Device - Handle to a framework device object.

Return Value:

    NTSTATUS - Failures will be logged, but not acted on.

--*/
{
    NTSTATUS status;

/*     UNREFERENCED_PARAMETER(ResourcesTranslated); */

/*     VERIFY_IS_IRQL_PASSIVE_LEVEL(); */

/*     TRACE(TL_TRACE, ( "--> t1394_EvtReleaseHardware\n")); */

/*     status = t1394_BusResetNotification(Device, */
/*                                         NULL, */
/*                                         DEREGISTER_NOTIFICATION_ROUTINE ); */

/*     TRACE(TL_TRACE, ( "<-- t1394_EvtReleaseHardware\n")); */

    return status;
}

NTSTATUS
t1394_EvtDeviceD0Entry(
    /*IN*/  WDFDEVICE Device,
    /*IN*/  WDF_POWER_DEVICE_STATE PreviousState
    )
/*++

Routine Description:

   EvtDeviceD0Entry event callback must perform any operations that are
   necessary before the specified device is used.  It will be called every
   time the hardware needs to be (re-)initialized.  This includes after
   IRP_MN_START_DEVICE, IRP_MN_CANCEL_STOP_DEVICE, IRP_MN_CANCEL_REMOVE_DEVICE,
   IRP_MN_SET_POWER-D0.

   This function runs at PASSIVE_LEVEL, though it is generally not paged.  A
   driver can optionally make this function pageable if DO_POWER_PAGABLE is set.

   Even if DO_POWER_PAGABLE isn't set, this function still runs at
   PASSIVE_LEVEL.  In this case, though, the function absolutely must not do
   anything that will cause a page fault.

Arguments:

    Device - Handle to a framework device object.

    PreviousState - Device power state which the device was in most recently.
        If the device is being newly started, this will be
        PowerDeviceUnspecified.

Return Value:

    NTSTATUS

--*/
{
    NTSTATUS   status = STATUS_SUCCESS;

/*     UNREFERENCED_PARAMETER(PreviousState); */

/*     TRACE(TL_TRACE, ( */
/*                 "-->t1394_EvtDeviceD0Entry - coming from %s\n", */
/*                 DbgDevicePowerString(PreviousState))); */

/*     // update the generation count */
/*     t1394_UpdateGenerationCount(Device); */

/*     TRACE(TL_TRACE, ( "<--t1394_EvtDeviceD0Entry\n")); */

    return status;
}


NTSTATUS
t1394_EvtDeviceD0Exit(
    /*IN*/  WDFDEVICE Device,
    /*IN*/  WDF_POWER_DEVICE_STATE TargetState
    )
/*++

Routine Description:

   EvtDeviceD0Exit event callback must perform any operations that are
   necessary before the specified device is moved out of the D0 state.  If the
   driver needs to save hardware state before the device is powered down, then
   that should be done here.

   This function runs at PASSIVE_LEVEL, though it is generally not paged.  A
   driver can optionally make this function pageable if DO_POWER_PAGABLE is set.

   Even if DO_POWER_PAGABLE isn't set, this function still runs at
   PASSIVE_LEVEL.  In this case, though, the function absolutely must not do
   anything that will cause a page fault.

Arguments:

    Device - Handle to a framework device object.

    TargetState - Device power state which the device will be put in once this
        callback is complete.

Return Value:

    NTSTATUS

--*/
{

/*     UNREFERENCED_PARAMETER(Device); */
/*     UNREFERENCED_PARAMETER(TargetState); */

/*     TRACE(TL_TRACE, ( */
/*                 "-->t1394_EvtDeviceD0Exit - moving to %s\n", */
/*                 DbgDevicePowerString(TargetState))); */


/*     TRACE(TL_TRACE, ( "<--t1394_EvtDeviceD0Exit\n")); */

    return STATUS_SUCCESS;
}


NTSTATUS
t1394_EvtDeviceAdd(
   /*IN*/WDFDRIVER        Driver,
   /*IN*/PWDFDEVICE_INIT  DeviceInit
    )
/*++
Routine Description:

    EvtDeviceAdd is called by the framework in response to AddDevice
    call from the PnP manager.

Arguments:

    Driver - Handle to a framework driver object created in DriverEntry

    DeviceInit - Pointer to a framework-allocated WDFDEVICE_INIT structure.

Return Value:

    NTSTATUS

--*/
{
    NTSTATUS                        status = STATUS_SUCCESS;
    PDEVICE_EXTENSION               deviceExtension;
    PNODE_DEVICE_EXTENSION          pNodeExt;
    WDF_PNPPOWER_EVENT_CALLBACKS    pnpPowerCallbacks;
    WDF_OBJECT_ATTRIBUTES           fdoAttributes,lockAttributes;
    WDFDEVICE                       device;
    WDF_DEVICE_PNP_CAPABILITIES     pnpCaps;
    WDF_IO_QUEUE_CONFIG             ioQueueConfig;
    WDF_IO_TARGET_OPEN_PARAMS       openParams;

    //UNREFERENCED_PARAMETER(Driver);

    //ENTER("t1394_PnpAddDevice");

    //
    // Zero out the PnpPowerCallbacks structure.
    //
    WDF_PNPPOWER_EVENT_CALLBACKS_INIT(&pnpPowerCallbacks);

    //
    // Set Callbacks for any of the functions we are interested in.
    // If no callback is set, Framework will take the default action
    // by itself.

    //
    // These two callbacks set up and tear down hardware state,
    // specifically that which only has to be done once.
    //

    pnpPowerCallbacks.EvtDevicePrepareHardware = t1394_EvtPrepareHardware;
    pnpPowerCallbacks.EvtDeviceReleaseHardware = t1394_EvtReleaseHardware;

    pnpPowerCallbacks.EvtDeviceSelfManagedIoCleanup =
        t1394_EvtDeviceSelfManagedIoCleanup;

    pnpPowerCallbacks.EvtDeviceD0Entry = t1394_EvtDeviceD0Entry;
    pnpPowerCallbacks.EvtDeviceD0Exit  = t1394_EvtDeviceD0Exit;

    //
    // Register the PnP and power callbacks. Power policy related callbacks
    // will be registered// later in SotwareInit.
    //
    WdfDeviceInitSetPnpPowerEventCallbacks(DeviceInit, &pnpPowerCallbacks);
    if ( !NT_SUCCESS(status)) {
        //TRACE(TL_ERROR, ("WdfDeviceInitSetPnpPowerEventCallbacks failed %x\n",
        //                 status));
        return status;
    }

    WdfDeviceInitSetExclusive(DeviceInit, FALSE);

    //
    // Specify the size and type of device context.
    //
    WDF_OBJECT_ATTRIBUTES_INIT_CONTEXT_TYPE(&fdoAttributes, DEVICE_EXTENSION);

    status = WdfDeviceCreate(&DeviceInit, &fdoAttributes, &device);

    if ( !NT_SUCCESS(status)) {
        //TRACE(TL_ERROR, ("WdfDeviceInitialize failed %x\n", status));
        return status;
    }


    deviceExtension = GetDeviceContext (device);
    deviceExtension->WdfDevice = device;

    //TRACE(TL_TRACE, ("PDO(0x%p) FDO(0x%p), Lower(0x%p) DevExt (0x%p)\n",
    //                 WdfDeviceWdmGetPhysicalDevice (device),
    //                 WdfDeviceWdmGetDeviceObject (device),
    //                 WdfDeviceWdmGetAttachedDevice(device),
    //                 deviceExtension));

    //
    // Tell the Framework that this device will need an interface so that
    // application can interact with it.
    //

    status = WdfDeviceCreateDeviceInterface(
                 device,
#if defined(_1394VDEV_DRIVER_)
                 (LPGUID) &GUID_1394VDEV,
#else
                 (LPGUID) &GUID_1394DIAG,
#endif
                 NULL
             );

    if (!NT_SUCCESS (status)) {
        //TRACE(TL_ERROR, ("WdfDeviceCreateDeviceInterface failed %x\n", status));
        return status;
    }

    //
    // Tell the framework to set the SurpriseRemovalOK in the DeviceCaps so
    // that you don't get the popup in usermode (on Win2K) when you surprise
    // remove the device.
    //
    WDF_DEVICE_PNP_CAPABILITIES_INIT(&pnpCaps);
    pnpCaps.SurpriseRemovalOK = WdfTrue;

    WdfDeviceSetPnpCapabilities(device, &pnpCaps);

    // save the device object we created as our physical device object
    deviceExtension->PhysicalDeviceObject =
        WdfDeviceWdmGetPhysicalDevice (device);

    if (deviceExtension->PhysicalDeviceObject == NULL) {
        //TRACE(TL_ERROR, ("WdfDeviceWdmGetPhysicalDevice: NULL DeviceObject\n"));
        return STATUS_UNSUCCESSFUL;
    }

    //
    // This is our default IoTarget representing the deviceobject
    // we are attached to.
    //
    deviceExtension->StackIoTarget = WdfDeviceGetIoTarget(device);
    deviceExtension->StackDeviceObject = WdfDeviceWdmGetAttachedDevice(device);

    if (deviceExtension->StackDeviceObject == NULL) {
        //TRACE(TL_ERROR, ("WdfDeviceWdmGetAttachedDevice: NULL DeviceObject\n"));
        return STATUS_UNSUCCESSFUL;
    }

    // Patch: this code is not in DDK 7600.16385.1  {
    //
    // Get the port device object from the passed in PhysicalDeviceObject
    // created by the 1394 stack for us.
    // Note: we can't use the top of the stack and get its device extension
    // in case there is a filter driver between us and our PDO.
    //
    //pNodeExt = WdfDeviceWdmGetPhysicalDevice(device)->DeviceExtension;
    //deviceExtension->PortDeviceObject = pNodeExt->PortDeviceObject;
    // Patch: this code is not in DDK 7600.16385.1 }

    //TRACE(TL_TRACE, ("PortDeviceObject = 0x%x\n",
    //                 deviceExtension->PortDeviceObject));

    //
    // Create a automanaged queue for dispatching ioctl requests.
    // All other requests are automatically failed by the framework.
    // By creating an automanaged queue we don't have to worry about
    // PNP/Power synchronization.
    // A default queue gets all the requests that are not
    // configure-fowarded using WdfDeviceConfigureRequestDispatching.
    //
    WDF_IO_QUEUE_CONFIG_INIT_DEFAULT_QUEUE(
        &ioQueueConfig,
        WdfIoQueueDispatchParallel
    );

    ioQueueConfig.EvtIoDeviceControl = t1394_EvtIoDeviceControl;

    status = WdfIoQueueCreate(
                 deviceExtension->WdfDevice,
                 &ioQueueConfig,
                 WDF_NO_OBJECT_ATTRIBUTES,
                 &deviceExtension->IoctlQueue // queue handle
             );

    if (!NT_SUCCESS (status)) {
         //TRACE(TL_ERROR, ("WdfIoQueueCreate failed 0x%x\n", status));
        return status;
    }

    //
    // Create an additional queue to hold bus reset requests.
    //
    WDF_IO_QUEUE_CONFIG_INIT(
        &ioQueueConfig,
        WdfIoQueueDispatchManual
        );

    status = WdfIoQueueCreate (
                   deviceExtension->WdfDevice,
                   &ioQueueConfig,
                   WDF_NO_OBJECT_ATTRIBUTES,
                   &deviceExtension->BusResetRequestsQueue
                   );

    if(!NT_SUCCESS (status)){
        //TRACE(TL_ERROR, ("Error Creating Reset Request Queue 0x%x\n",
        //                 status));
        return status;
    }

    //
    // Create another IoTarget representing PortDeviceObject so that
    // we can send async requests in rawmode directly to the port device.
    //
    WDF_IO_TARGET_OPEN_PARAMS_INIT_EXISTING_DEVICE(&openParams,
                                                pNodeExt->PortDeviceObject);
    status = WdfIoTargetCreate(device,
                                WDF_NO_OBJECT_ATTRIBUTES,
                                &deviceExtension->PortDeviceIoTarget);
    if (!NT_SUCCESS (status)) {
        //TRACE(TL_ERROR, ("WdfIoTargetCreate failed 0x%x\n", status));
        return status;
    }

    status = WdfIoTargetOpen(deviceExtension->PortDeviceIoTarget, &openParams);
    if (!NT_SUCCESS (status)) {
        //TRACE(TL_ERROR, ("WdfIoTargetCreate failed 0x%x\n", status));
        return status;
    }


    WDF_OBJECT_ATTRIBUTES_INIT(&lockAttributes);
    lockAttributes.ParentObject = device;
    // initialize the spinlock/list to store the bus reset irps...

    status = WdfSpinLockCreate(&lockAttributes,&deviceExtension->CromSpinLock );
    if(!NT_SUCCESS(status)){
         //TRACE(TL_ERROR, ("WdfSpinLockCreate CromSpinLock "
         //                 "failed 0x%x\n", status));
        return status;
    }


    WDF_OBJECT_ATTRIBUTES_INIT(&lockAttributes);
    lockAttributes.ParentObject = device;

    status = WdfSpinLockCreate(&lockAttributes,
                               &deviceExtension->AsyncSpinLock );
    if(!NT_SUCCESS(status)){
         //TRACE(TL_ERROR, ("WdfSpinLockCreate AsyncSpinLock "
         //                 "failed 0x%x\n", status));
        return status;
    }

    WDF_OBJECT_ATTRIBUTES_INIT(&lockAttributes);
    lockAttributes.ParentObject = device;

    status = WdfSpinLockCreate(&lockAttributes,
                               &deviceExtension->IsochSpinLock );
    if(!NT_SUCCESS(status)){
         //TRACE(TL_ERROR, ("WdfSpinLockCreate IsochSpinLock "
         //                 "failed 0x%x\n", status));
        return status;
    }

    WDF_OBJECT_ATTRIBUTES_INIT(&lockAttributes);
    lockAttributes.ParentObject = device;

    status = WdfSpinLockCreate(&lockAttributes,
                               &deviceExtension->IsochResourceSpinLock );
    if(!NT_SUCCESS(status)){
         //TRACE(TL_ERROR, ("WdfSpinLockCreate IsochResourceSpinLock "
         //                 "failed 0x%x\n", status));
        return status;
    }
    InitializeListHead(&deviceExtension->CromData);
    InitializeListHead(&deviceExtension->AsyncAddressData);
    InitializeListHead(&deviceExtension->IsochDetachData);
    InitializeListHead(&deviceExtension->IsochResourceData);

    //EXIT("t1394_PnpAddDevice", status);

    return(status);
} // t1394_PnpAddDevice


/******************************************************************************

  DriverEntry

 ******************************************************************************/

NTSTATUS
DriverEntry(
   /*IN*/PDRIVER_OBJECT   DriverObject,
   /*IN*/PUNICODE_STRING  RegistryPath
    )
/*++

Routine Description:

    Installable driver initialization entry point.
    This entry point is called directly by the I/O system.

Arguments:

    DriverObject - pointer to the driver object

    RegistryPath - pointer to a unicode string representing the path,
                   to driver-specific key in the registry.

Return Value:

    STATUS_SUCCESS if successful,
    STATUS_UNSUCCESSFUL otherwise.

--*/
{
    NTSTATUS    ntStatus = STATUS_SUCCESS;
    WDF_DRIVER_CONFIG      config;

    //ENTER("DriverEntry");

    // //TRACE(TL_TRACE, ("1394VDev Sample - Driver Framework Edition \n"));
    // TRACE(TL_TRACE, ("Built %s %s\n", __DATE__, __TIME__));

    //
    // Initialize the Driver Config structure..
    //
    WDF_DRIVER_CONFIG_INIT(
        &config,
        t1394_EvtDeviceAdd
    );

    //
    // Create a WDFDRIVER object.
    //
    ntStatus = WdfDriverCreate(
                 DriverObject,
                 RegistryPath,
                 WDF_NO_OBJECT_ATTRIBUTES,
                 &config,
                 WDF_NO_HANDLE
             );

    if (!NT_SUCCESS(ntStatus)) {
        // TRACE(TL_ERROR, ("WdfDriverCreate failed with status %x\n", ntStatus));
    }

    //EXIT("DriverEntry", ntStatus);
    return(ntStatus);
} // DriverEntry








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

void main()
{

  // This is the OS Model
  PDRIVER_OBJECT   DriverObject;
  WDFDEVICE Device;
  PWDFDEVICE_INIT  DeviceInit;
  WDFQUEUE Queue;
  WDFREQUEST Request;
  ULONG IoControlCode; // symbolic
  int InputBufferLength, OutputBufferLength; // symbolic
  PUNICODE_STRING RegistryPath; // symbolic, genuinely don't care.
  NTSTATUS status;

  // $Super$$main
  status = SLAyer_harness_init() ;

  if (NT_SUCCESS(status)) {
    // OSModel: call DriverEntry
    DriverEntry(DriverObject,RegistryPath);

    // OSModel: Add N devices, for N=1.
    t1394_EvtDeviceAdd(SL_Driver, DeviceInit);

    // OSModel: Make a queue.
    Queue = (WDFQUEUE)malloc(sizeof(SLAyer_WDFOBJECT));
    Queue->typ = SLAyerWdfQueue;
    Queue->typQueue.Device = SL_Device_one;

    // OSModel: Make a request.
    Request = (WDFREQUEST)malloc(sizeof(SLAyer_WDFOBJECT));
    Request->typ = SLAyerWdfRequest;
    Request->typRequest.InputBuffer = (void*)_SLAyer_malloc(1024);

    // OSModel: send request to 1393 Device,
    // which can only do Request IOCTL_SET_LOCAL_HOST_INFORMATION.
    t1394_EvtIoDeviceControl(Queue, Request, OutputBufferLength, InputBufferLength, IoControlCode);

    //  t1394_EvtDeviceSelfManagedIoCleanup(Device);

    SLAyer_harness_teardown();
  }


}

