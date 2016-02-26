/*****************************************************************************
  Copyright (c) Microsoft Corporation.  All rights reserved.

  1394 Feature: IsochDetachDatas share devExt.
  Source: isochapi.c, lines
  Expected Result: SAFE.
 *****************************************************************************/

#include "harness.h"
#include "1394.h"


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
  PDEVICE_EXTENSION devExt;
  PLIST_ENTRY       listHead, thisEntry;
  BOOLEAN           devExt_same         = TRUE;

  // OS Model init
  status = SLAyer_harness_init();

  if (NT_SUCCESS(status)) {
    devExt = GetDeviceContext(SL_Device_one);

    // Setup: cons some IsochDetachDatas, all pointing to devExt.
    // Use _SLAyer_malloc rather than ExAllocate to definitely alloc.
    InitializeListHead(&(devExt->IsochDetachData));

    while (nondet()) {
      PISOCH_DETACH_DATA IsochDetachDataNew;
      // isochapi, line 408, except we've had to cast the Alloc.
      IsochDetachDataNew = _SLAyer_malloc(sizeof(ISOCH_DETACH_DATA));

      // isochapi, line 444.
      IsochDetachDataNew->DeviceExtension = devExt;

      // isochapi, line 432.
      InsertHeadList(&devExt->IsochDetachData,
                     &IsochDetachDataNew->IsochDetachList);
    }

    // An abstraction of IsochTimeout. There a given IsochDetachData's
    // DeviceExtension is checked for allocated-ness. Here, we check that all
    // DeviceExtensions are the same as devExt.
    listHead = &(devExt->IsochDetachData);
    for (thisEntry = listHead->Flink;
         thisEntry != listHead;
         thisEntry = thisEntry->Flink)
      {
        PISOCH_DETACH_DATA IsochDetachData =
          CONTAINING_RECORD(thisEntry, ISOCH_DETACH_DATA, IsochDetachList) ;
        devExt_same =
          devExt_same && (devExt == IsochDetachData->DeviceExtension);
      }

    assert( devExt_same );


    // Teardown.
    while (!IsListEmpty(&(devExt->IsochDetachData))) {
      PLIST_ENTRY listEntry = RemoveHeadList(&(devExt->IsochDetachData));

      PISOCH_DETACH_DATA IsochDetachData = CONTAINING_RECORD (listEntry,
                                                              ISOCH_DETACH_DATA,
                                                              IsochDetachList);

      free(IsochDetachData);
    }

    // OS Model teardown
    SLAyer_harness_teardown();
  }

  return (NT_SUCCESS(status));
}

