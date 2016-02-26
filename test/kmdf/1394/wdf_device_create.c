/*****************************************************************************
  Copyright (c) Microsoft Corporation.  All rights reserved.

  1394 Feature: wdf_device_create.
  Source: 1394samp.c, line 105.
  Expected Result: SAFE.
 *****************************************************************************/
#include "harness.h"
#include <1394.h>

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

// Stub of function at pnp.c, line 308.
NTSTATUS
t1394_EvtPrepareHardware (
    WDFDEVICE      Device,
    WDFCMRESLIST   Resources,
    WDFCMRESLIST   ResourcesTranslated
    )
{
  return STATUS_SUCCESS;
}

// Stub of function at pnp.c, line 354.
NTSTATUS
t1394_EvtReleaseHardware(
    /* IN */  WDFDEVICE Device,
    /* IN */  WDFCMRESLIST ResourcesTranslated
    )
{
  return STATUS_SUCCESS;
}

// Stub of function at pnp.c, line 379.
NTSTATUS
t1394_EvtDeviceD0Entry(
    /* IN */  WDFDEVICE Device,
    /* IN */  WDF_POWER_DEVICE_STATE PreviousState
    )
{
  return STATUS_SUCCESS;
}

// Stub of function at pnp.c, line 450.
NTSTATUS
t1394_EvtDeviceD0Exit(
    /* IN */  WDFDEVICE Device,
    /* IN */  WDF_POWER_DEVICE_STATE TargetState
    )
{
  return STATUS_SUCCESS;
}

// Stub of function at pnp.c, line 499.
VOID
t1394_EvtDeviceSelfManagedIoCleanup(
    /* IN */  WDFDEVICE Device
    )
{
  return;
}


/*****************************************************************************
  Harness.
 *****************************************************************************/
int main ()
{
  WDFDEVICE_INIT  DeviceInit;
  NTSTATUS                        status = STATUS_SUCCESS;
  PDEVICE_EXTENSION               deviceExtension;
  WDF_PNPPOWER_EVENT_CALLBACKS    pnpPowerCallbacks;
  WDF_OBJECT_ATTRIBUTES           fdoAttributes;
  WDFDEVICE                       device;

  // Initialize the PnpPowerCallbacks structure.
  WDF_PNPPOWER_EVENT_CALLBACKS_INIT(&pnpPowerCallbacks);
  pnpPowerCallbacks.EvtDevicePrepareHardware = t1394_EvtPrepareHardware;
  pnpPowerCallbacks.EvtDeviceReleaseHardware = t1394_EvtReleaseHardware;
  pnpPowerCallbacks.EvtDeviceSelfManagedIoCleanup = t1394_EvtDeviceSelfManagedIoCleanup;
  pnpPowerCallbacks.EvtDeviceD0Entry = t1394_EvtDeviceD0Entry;
  pnpPowerCallbacks.EvtDeviceD0Exit  = t1394_EvtDeviceD0Exit;

  // Register the PnP and power callbacks. Power policy related callbacks
  // SI: impl - implies that WDFDEVICE_INIT contains a WF_PNPPOWER_EVENT_CALLBACKS.
  //            And that this InitSet call initializes this part to pnpPowerCallbacks's
  //            content.
  WdfDeviceInitSetPnpPowerEventCallbacks(&DeviceInit, &pnpPowerCallbacks);
  //WdfDeviceInitSetExclusive(DeviceInit, FALSE);

  WDF_OBJECT_ATTRIBUTES_INIT_CONTEXT_TYPE(&fdoAttributes, DEVICE_EXTENSION);

  // Implies that WDFDEVICE must contain:
  //    - DEVICE_INIT
  //    - WDF_OBJECT_ATTRIBUTES
  //          - The ContextTypeInfo contains the meta-data of DEVICE_EXTENSION.
  //            So an additional void* ptr in WDFDEVICE could contain an
  //            instance of DEVICE_EXTENSINON.
  status = WdfDeviceCreate(&DeviceInit, &fdoAttributes, &device);
  if ( !NT_SUCCESS(status)) {
    //TRACE(TL_ERROR, ("WdfDeviceInitialize failed %x\n", status));
  }

  if (NT_SUCCESS(status)) {
    free(SL_Device_one->Context);
    free(SL_Device_one);
  }

  return (NT_SUCCESS(status));
}
