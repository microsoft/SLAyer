/* Copyright (c) Microsoft Corporation.  All rights reserved. */

/*****************************************************************************
  1394 Feature: 1394api_Add_and_Remove_CromData.
  Source: 1394api.c::t1394_SetLocalHostProperties, lines 823-985, 959-991
  Expected Result: SAFE, LEAKS.
 *****************************************************************************/
#include "harness.h"
#include "1394.h"

int d_x = 1;
int d_y = 2;
int d_z = 3;

#define BUFFER_LENGTH 32

NTSTATUS
main()
{
  PDEVICE_EXTENSION deviceExtension;
  PLIST_ENTRY listHead, thisEntry;
  CROM_DATA *CromData = NULL, *CromDataNew;

  // 0. Setup.

  deviceExtension = (PDEVICE_EXTENSION)malloc(sizeof(DEVICE_EXTENSION));
  InitializeListHead(&(deviceExtension->CromData));

  // 1394api.c, lines 823-985 (without the error-checking).
  CromDataNew = (PCROM_DATA) malloc(sizeof(CROM_DATA));
  CromDataNew->Buffer = (PVOID)malloc(sizeof(BUFFER_LENGTH));
  CromDataNew->pMdl = (PMDL)malloc(sizeof(MDL));
  InsertHeadList(&deviceExtension->CromData, &CromDataNew->CromList);

  CromDataNew = (PCROM_DATA) malloc(sizeof(CROM_DATA));
  CromDataNew->Buffer = (PVOID)malloc(sizeof(BUFFER_LENGTH));
  CromDataNew->pMdl = (PMDL)malloc(sizeof(MDL));
  InsertHeadList(&deviceExtension->CromData, &CromDataNew->CromList);

  CromDataNew = (PCROM_DATA) malloc(sizeof(CROM_DATA));
  CromDataNew->Buffer = (PVOID)malloc(sizeof(BUFFER_LENGTH));
  CromDataNew->pMdl = (PMDL)malloc(sizeof(MDL));
  InsertHeadList(&deviceExtension->CromData, &CromDataNew->CromList);


  // 1394api.c, lines 959-991.
  listHead = &deviceExtension->CromData;

  for(thisEntry = listHead->Flink;
      thisEntry != listHead;
      CromData = NULL, thisEntry = thisEntry->Flink)
    {
      int *filter = &d_y; // Could leave un-initialized for a more generic test.
      CromData = CONTAINING_RECORD(thisEntry, CROM_DATA, CromList);
      if (CromData->hCromData == filter) {
	RemoveEntryList(&CromData->CromList);
	break;
      }
    }
  if (CromData) {
    if (CromData->Buffer) {
      free(CromData->Buffer);
    }
    free(CromData);
  }

  // Teardown ... Not implemented yet, will LEAK.
  //t1394_EvtDeviceSelfManagedIoCleanup(deviceExtension);
  //  free(deviceExtension);

  return STATUS_SUCCESS;
}

