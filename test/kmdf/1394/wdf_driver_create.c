/*****************************************************************************
  Copyright (c) Microsoft Corporation.  All rights reserved.

  1394 Feature: WdfDriverCreate.
  Source: 1394samp.c, line 75.
  Expected Result: SAFE.
 *****************************************************************************/
#include "harness.h"
#include "1394.h"

/*
 * Stub of function at pnp.c, line 26.
 */
NTSTATUS
t1394_EvtDeviceAdd(
    /* IN */ WDFDRIVER        Driver,
    /* IN */ PWDFDEVICE_INIT  DeviceInit
    )
{
  return STATUS_SUCCESS;
}

void SLAyer_harness_teardown()
{
  if (NULL != SL_Driver->Context) { free(SL_Driver->Context); }
  free(SL_Driver);
}

/*****************************************************************************
  Harness.
 *****************************************************************************/
int main ()
{
  NTSTATUS ntStatus;
  PUNICODE_STRING RegistryPath;
  PDRIVER_OBJECT DriverObject;
  WDF_DRIVER_CONFIG config;

  WDF_DRIVER_CONFIG_INIT(&config,
			 t1394_EvtDeviceAdd
			 );

  ntStatus = WdfDriverCreate(DriverObject,
			     RegistryPath,
			     WDF_NO_OBJECT_ATTRIBUTES,
			     &config,
			     WDF_NO_HANDLE
			     );

  SLAyer_harness_teardown();

  return ntStatus;
}
