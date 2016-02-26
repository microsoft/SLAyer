/*****************************************************************************
  Copyright (c) Microsoft Corporation.  All rights reserved.

  1394 CoveragePoint: wdf_device_init_set_pnp_event_callbacks.
  Source: pnp.c, line 91.
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
  NTSTATUS                        status = STATUS_SUCCESS;
  //PDEVICE_EXTENSION               deviceExtension;
  WDF_OBJECT_ATTRIBUTES           fdoAttributes;

  WDF_OBJECT_ATTRIBUTES_INIT_CONTEXT_TYPE(&fdoAttributes, DEVICE_EXTENSION);

  return STATUS_SUCCESS;
}
