/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  More complicated version of copy_struct_on_stack.c.
*/
#include "slayer.h"

typedef struct _GUID {
    unsigned long  Data1;
    unsigned short Data2;
    unsigned short Data3;
    unsigned char  Data4[ 8 ];
} GUID;
#define DEFINE_GUID(name, l, w1, w2, b1, b2, b3, b4, b5, b6, b7, b8) \
        /*EXTERN_C*/ const GUID /*DECLSPEC_SELECTANY*/ name \
                = { l, w1, w2, { b1, b2,  b3,  b4,  b5,  b6,  b7,  b8 } }

typedef enum _INTERFACE_TYPE {
    InterfaceTypeUndefined = -1,
    Internal,
    Isa,
    Eisa,
    MicroChannel,
    TurboChannel,
    PCIBus,
    VMEBus,
    NuBus,
    PCMCIABus,
    CBus,
    MPIBus,
    MPSABus,
    ProcessorInternal,
    InternalPowerBus,
    PNPISABus,
    PNPBus,
    Vmcs,
    ACPIBus,
    MaximumInterfaceType
}INTERFACE_TYPE, *PINTERFACE_TYPE;

#define ULONG unsigned long

typedef struct _PNP_BUS_INFORMATION {
    GUID BusTypeGuid;
    INTERFACE_TYPE LegacyBusType;
    ULONG BusNumber;
} PNP_BUS_INFORMATION, *PPNP_BUS_INFORMATION;





void main ()
{
    PNP_BUS_INFORMATION        busInfo;
    DEFINE_GUID (GUID_DEVCLASS_TOASTER,
                 0xB85B7C50, 0x6A01, 0x11d2, 0xB8, 0x41, 0x00, 0xC0, 0x4F, 0xAD, 0x51, 0x71);

    busInfo.BusTypeGuid = GUID_DEVCLASS_TOASTER;
    busInfo.LegacyBusType = PNPBus;
    busInfo.BusNumber = 0;

}


