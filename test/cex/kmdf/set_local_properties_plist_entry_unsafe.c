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

void SLAyer_harness_init()
{
  SL_Device_one = (WDFDEVICE)malloc(sizeof(WDFDEVICE));
  SL_Context_DEVICE_EXTENSION_init();
}

void SLAyer_harness_teardown()
{
  free(SL_Device_one);
}

NTSTATUS
main()
{
  PDEVICE_EXTENSION deviceExtension;
  CROM_DATA *data_x, *data_y, *data_z;
  PLIST_ENTRY listHead, thisEntry;

  // 0. Initialize devExt.
  //  deviceExtension = (PDEVICE_EXTENSION)malloc(sizeof(PDEVICE_EXTENSION));

  // 1. Add some CromDatas.
  data_x = (PCROM_DATA)malloc(sizeof(CROM_DATA));
  //  data_x->Buffer = (PVOID)malloc(sizeof(int));
  data_x->pMdl = (PMDL)malloc(sizeof(MDL));
  data_x->hCromData = &d_x;
  data_y = (PCROM_DATA)malloc(sizeof(int));
  data_y->Buffer = (PVOID)malloc(sizeof(int));
  data_y->pMdl = (PMDL)malloc(1);
  data_y->hCromData = &d_y;
  data_z = (PCROM_DATA)malloc(sizeof(CROM_DATA));
  data_z->Buffer = (PVOID)malloc(sizeof(int));
  data_z->pMdl = (PMDL)malloc(sizeof(MDL));
  data_z->hCromData = &d_z;

/*   SL_triple_CromDatas(deviceExtension,data_x,data_y,data_z) ; */

  InitializeListHead(&(deviceExtension->CromData));
  InitializeListHead(&(data_x->CromList));
  //InitializeListHead(&(data_y->CromList));
  InitializeListHead(&(data_z->CromList));

  InsertHeadList(&deviceExtension->CromData, &data_x->CromList);
  InsertHeadList(&deviceExtension->CromData, &data_y->CromList);
  InsertHeadList(&deviceExtension->CromData, &data_z->CromList);

/*   SL_triple_CromDatasInserted(deviceExtension,data_x,data_y,data_z) ; */

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

  // Note: Teardown. Currently we LEAK.
  return STATUS_SUCCESS;
}
