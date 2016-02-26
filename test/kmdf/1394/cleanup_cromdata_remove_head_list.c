/*****************************************************************************
  Copyright (c) Microsoft Corporation.  All rights reserved.

  1394 Feature: cleanup_cromdata_remove_head_list.
  Expected Result: SAFE.
 *****************************************************************************/

#include "harness.h"
#include "1394.h"

/* Source: WDK/src_5239/kmdf/1394/pnp.c, line 531. */
VOID
t1394_EvtDeviceSelfManagedIoCleanup(
/*     IN  WDFDEVICE Device */
    PDEVICE_EXTENSION   deviceExtension /* added for SLAyer */
    )
{
     PLIST_ENTRY         listEntry;

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
	  if (CromData->Buffer)
                ExFreePool(CromData->Buffer);

            if (CromData->pMdl)
                IoFreeMdl(CromData->pMdl);

            // we already checked CromData
            ExFreePool(CromData);
        }
    }


    WdfSpinLockRelease(deviceExtension->CromSpinLock);

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

NTSTATUS
main()
{
  NTSTATUS status;
  PDEVICE_EXTENSION devExt;
  unsigned int count, i; // count=*

  // OS Model init
  status = SLAyer_harness_init();

  if (NT_SUCCESS(status)) {

    devExt = GetDeviceContext(SL_Device_one);

    InitializeListHead(&(devExt->CromData));

    for (i=0; i<count; i++) {
      PCROM_DATA CromData = (PCROM_DATA)malloc(sizeof(CROM_DATA));
      // Don't bother initializing hCromData, 1394 only sets it in SetLocalHostP Xbroperties
      CromData->Buffer = (PVOID)malloc(sizeof(int));
      CromData->pMdl = (PMDL)malloc(sizeof(MDL));
      InsertHeadList(&(devExt->CromData),&(CromData->CromList));
    }

    // 2. Delete all CromDatas.
    t1394_EvtDeviceSelfManagedIoCleanup(devExt);

    // OS Model teardown
    SLAyer_harness_teardown();
  }

  return STATUS_SUCCESS;
}
