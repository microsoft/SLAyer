/*****************************************************************************
  Copyright (c) Microsoft Corporation.  All rights reserved.

  1394 CoveragePoint: wdf_pnp_power_event_callback_zero,
                      wdf_pnp_power_event_callback_init.
  Source: pnp.c, lines 66-85.
  Expected Result: SAFE.
 *****************************************************************************/
#include "harness.h"
#include "1394.h"

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

int main (int argc, char* argv[])
{
  WDF_PNPPOWER_EVENT_CALLBACKS* callbacks =
    (PWDF_PNPPOWER_EVENT_CALLBACKS)malloc(sizeof(WDF_PNPPOWER_EVENT_CALLBACKS));

  WDF_PNPPOWER_EVENT_CALLBACKS_INIT(callbacks);

  callbacks->EvtDevicePrepareHardware = t1394_EvtPrepareHardware;
  callbacks->EvtDeviceReleaseHardware = t1394_EvtReleaseHardware;

  callbacks->EvtDeviceSelfManagedIoCleanup = t1394_EvtDeviceSelfManagedIoCleanup;

  callbacks->EvtDeviceD0Entry = t1394_EvtDeviceD0Entry;
  callbacks->EvtDeviceD0Exit  = t1394_EvtDeviceD0Exit;

  free(callbacks) ;

  return STATUS_SUCCESS;
}
