/*****************************************************************************
  Copyright (c) Microsoft Corporation.  All rights reserved.

  1394 Feature: allocate_resources_insert_head_list.
  Source: isochapi.c, line 223.
  Expected Result: SAFE.
 *****************************************************************************/
#include "harness.h"
#include "1394.h"

NTSTATUS
t1394_IsochAllocateResources(PDEVICE_EXTENSION deviceExtension)
{
  PISOCH_RESOURCE_DATA  IsochResourceData;

  // need to add to our list...
  IsochResourceData = (PISOCH_RESOURCE_DATA)malloc(sizeof(ISOCH_RESOURCE_DATA));

  {
    InsertHeadList(&deviceExtension->IsochResourceData,
		   &IsochResourceData->IsochResourceList);
  }
  return STATUS_SUCCESS;

} // t1394_IsochAllocateResources


int main ()
{
  PDEVICE_EXTENSION deviceExtension;
  NTSTATUS            ntStatus        = STATUS_SUCCESS;
  PLIST_ENTRY prd ;

  // 0. Initialize devExt.
  deviceExtension = (PDEVICE_EXTENSION)malloc(sizeof(DEVICE_EXTENSION));
  InitializeListHead(&(deviceExtension->IsochResourceData));

  ntStatus = t1394_IsochAllocateResources(deviceExtension);
  ntStatus = t1394_IsochAllocateResources(deviceExtension);
  ntStatus = t1394_IsochAllocateResources(deviceExtension);
  ntStatus = t1394_IsochAllocateResources(deviceExtension);

  return ntStatus;
}

