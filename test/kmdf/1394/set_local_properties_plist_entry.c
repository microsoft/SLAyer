/*****************************************************************************
  Copyright (c) Microsoft Corporation.  All rights reserved.

  1394 Feature: 1394api_InsertHeadList, 1394api_RemoveEntryList.
  Source: 1394api.c, lines 954, 974.
  Expected Result: SAFE, MAY LEAK.
 *****************************************************************************/
#include "harness.h"
#include "1394.h"

int d_x = 1;
int d_y = 2;
int d_z = 3;

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
  PDEVICE_EXTENSION deviceExtension;
  CROM_DATA *data_x, *data_y, *data_z;
  PLIST_ENTRY listHead, thisEntry;

  // OS Model init
  status = SLAyer_harness_init();

  if (NT_SUCCESS(status)) {
    // 0. Initialize devExt.
    deviceExtension = GetDeviceContext(SL_Device_one);

    // 1. Add some CromDatas.
    data_x = (PCROM_DATA)malloc(sizeof(CROM_DATA));
    data_x->Buffer = (PVOID)malloc(sizeof(int));
    data_x->pMdl = (PMDL)malloc(sizeof(MDL));
    data_x->hCromData = &d_x;
    data_y = (PCROM_DATA)malloc(sizeof(CROM_DATA));
    data_y->Buffer = (PVOID)malloc(sizeof(int));
    data_y->pMdl = (PMDL)malloc(sizeof(MDL));
    data_y->hCromData = &d_y;
    data_z = (PCROM_DATA)malloc(sizeof(CROM_DATA));
    data_z->Buffer = (PVOID)malloc(sizeof(int));
    data_z->pMdl = (PMDL)malloc(sizeof(MDL));
    data_z->hCromData = &d_z;

    InitializeListHead(&(deviceExtension->CromData));
    InitializeListHead(&(data_x->CromList));
    InitializeListHead(&(data_y->CromList));
    InitializeListHead(&(data_z->CromList));

    InsertHeadList(&deviceExtension->CromData, &data_x->CromList);
    InsertHeadList(&deviceExtension->CromData, &data_y->CromList);
    InsertHeadList(&deviceExtension->CromData, &data_z->CromList);

    // 2. Delete some CromDatas.
    listHead = &deviceExtension->CromData;

    for(thisEntry = listHead->Flink;
        thisEntry != listHead;
        thisEntry = thisEntry->Flink)
      {
        int *filter = &d_y; // Could leave un-initialized for a more generic test.
        CROM_DATA *CromData = CONTAINING_RECORD(thisEntry, CROM_DATA, CromList);
        if (CromData->hCromData == filter) {
          RemoveEntryList(&CromData->CromList);
          break;
        }
      }

    // OS Model teardown
    SLAyer_harness_teardown();
  }

  // Note: Currently we LEAK; should teardown here.
  return (NT_SUCCESS(status));
}
