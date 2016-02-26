/*****************************************************************************
  Copyright (c) Microsoft Corporation.  All rights reserved.

  1394 Feature: initialize_list_head.
  Source: pnp.c, line 298.
  Expected Result: SAFE.
 *****************************************************************************/
#include "harness.h"
#include "1394.h"

NTSTATUS
main()
{
  PDEVICE_EXTENSION deviceExtension;
  CROM_DATA *data_x, *data_y, *data_z;
  PLIST_ENTRY listHead, thisEntry;

  // 0. Initialize devExt.
  deviceExtension = (PDEVICE_EXTENSION)malloc(sizeof(DEVICE_EXTENSION));

  // If we don't initialize deviceExtension->CromData, the first
  // InsertHeadList goes wrong.
  InitializeListHead(&(deviceExtension->CromData));
/*   SL_triple_de_init(deviceExtension,&(deviceExtension->CromData)); */

  // Note: assert that dE->cromdata is a dll.
  free(deviceExtension);

  return STATUS_SUCCESS;
}




