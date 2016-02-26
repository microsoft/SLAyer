/*****************************************************************************
  Copyright (c) Microsoft Corporation.  All rights reserved.

  1394 Feature: WDF_DRIVER_CONFIG_INIT.
  Source: %SLAM%/WDK/inc/wdf/kmdf/1.9/, line 123.
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


/*
 * Harness.
 */
int main ()
{
  WDF_DRIVER_CONFIG config;

  WDF_DRIVER_CONFIG_INIT(
			 &config,
			 t1394_EvtDeviceAdd
			 );

}
