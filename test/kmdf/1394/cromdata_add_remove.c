/*****************************************************************************
  Copyright (c) Microsoft Corporation.  All rights reserved.

  1394 Feature: 1394api_Add_and_Remove_CromData.
  Source: 1394api.c::t1394_SetLocalHostProperties, lines 823-985, 959-991
  Expected Result: SAFE.
*****************************************************************************/
#include "harness.h"
#include "1394.h"

#define BUFFER_LENGTH 32

NTSTATUS
main()
{
  PDEVICE_EXTENSION deviceExtension;
  PLIST_ENTRY listEntry;

  deviceExtension = (PDEVICE_EXTENSION)malloc(sizeof(DEVICE_EXTENSION));
  InitializeListHead(&(deviceExtension->CromData));

  // Add some CromDatas to deviceExtension.
  while (nondet()) {
    PCROM_DATA CromDataNew ;
    CromDataNew = (PCROM_DATA) malloc(sizeof(CROM_DATA));
    CromDataNew->Buffer = (PVOID)malloc(sizeof(BUFFER_LENGTH));
    CromDataNew->pMdl = (PMDL)malloc(sizeof(MDL));
    InsertHeadList(&deviceExtension->CromData, &CromDataNew->CromList);
  }

  // assert dE->CromData is a dll.

  // - Delete all CromDatas.
  // pnp.c:: t1394_EvtDeviceSelfManagedIoCleanup, lines 531-556.
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

  // Teardown ... Not implemented yet, will LEAK.
  //  free(deviceExtension);
  return STATUS_SUCCESS;
}
