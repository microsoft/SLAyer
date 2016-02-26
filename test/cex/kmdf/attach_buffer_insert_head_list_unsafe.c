/*****************************************************************************
  Copyright (c) Microsoft Corporation.  All rights reserved.

  1394 Feature: attach_buffer_insert_head_list.
  Source: isochapi.c, line 432.
  Expected Result: SAFE.
 *****************************************************************************/

#include "harness.h"
#include "1394.h"

NTSTATUS
t1394_IsochAttachBuffers(PDEVICE_EXTENSION deviceExtension)
{
    NTSTATUS                    ntStatus            = STATUS_SUCCESS;
    PISOCH_DETACH_DATA          pIsochDetachData    = NULL;

    pIsochDetachData = (PISOCH_DETACH_DATA)malloc(sizeof(ISOCH_DETACH_DATA));
    if (!pIsochDetachData)
      {
	ntStatus = STATUS_INSUFFICIENT_RESOURCES;
	goto Exit_IsochAttachBuffers;
      }
    pIsochDetachData->DeviceExtension = deviceExtension;
    InitializeListHead(&pIsochDetachData->IsochDetachList);
    InsertHeadList(&deviceExtension->IsochDetachData,
		   &pIsochDetachData->IsochDetachList);

 Exit_IsochAttachBuffers:
    return(ntStatus);
} // t1394_IsochAttachBuffers


NTSTATUS SLAyer_harness_init()
{
  PWDFDEVICE_INIT DInit;
  WDF_OBJECT_ATTRIBUTES DAttrib;
  WDFDEVICE Device;
  NTSTATUS status;

  // malloc SL_Device
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

  PDEVICE_EXTENSION deviceExtension;
  NTSTATUS          ntStatus;

  // OS Model init
  ntStatus = SLAyer_harness_init();

  if (NT_SUCCESS(ntStatus)) {

    deviceExtension = GetDeviceContext(SL_Device_one);

    // Setup
    InitializeListHead(&(deviceExtension->IsochDetachData));

    while (nondet()) {
      ntStatus = t1394_IsochAttachBuffers(deviceExtension);
      assert(0==1);
    }

  // Note: We currently leak: teardown here.
  }

  return ntStatus;

}
