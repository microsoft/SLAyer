/*****************************************************************************
  Copyright (c) Microsoft Corporation.  All rights reserved.

  1394 Feature: cleanup_asyncaddressdata_remove_head_list.
  Expected Result: SAFE.
 *****************************************************************************/

#include "harness.h"
#include "1394.h"


/* Source: WDK/src_5239/kmdf/1394/pnp.c, line 561. */
VOID
t1394_EvtDeviceSelfManagedIoCleanup(
/*     IN  WDFDEVICE Device */
    PDEVICE_EXTENSION   deviceExtension /* added for SLAyer */
    )
{
     PLIST_ENTRY         listEntry;

    // lets free up any allocated addresses and deallocate all
    // memory associated with them...

    WdfSpinLockAcquire(deviceExtension->AsyncSpinLock);

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


    WdfSpinLockRelease(deviceExtension->AsyncSpinLock);

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

  status = SLAyer_harness_init();

  if (NT_SUCCESS(status)) {
    devExt = GetDeviceContext(SL_Device_one);

    InitializeListHead(&(devExt->AsyncAddressData));

    for (i=0; i<count; i++) {
      PASYNC_ADDRESS_DATA pdata =
	(PASYNC_ADDRESS_DATA)malloc(sizeof(ASYNC_ADDRESS_DATA));

      pdata->pMdl = (PMDL)malloc(sizeof(MDL));

      pdata->Buffer = malloc(1);

      pdata->AddressRange = (PADDRESS_RANGE)malloc(sizeof(ADDRESS_RANGE));

      InsertHeadList(&(devExt->AsyncAddressData), &(pdata->AsyncAddressList));
    }

    // 2. Delete all AsyncAddressDatas.
    t1394_EvtDeviceSelfManagedIoCleanup(devExt);

    // OS Model teardown
  SLAyer_harness_teardown();
  }

  return (NT_SUCCESS(status));
}


