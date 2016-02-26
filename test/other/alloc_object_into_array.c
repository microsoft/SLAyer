/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Store a new object into array[0], array[1], etc. Because of how
  SLAyer treats arrays (compacts into one-elt), we see a LEAK of the
  elt stored in array[0].

  Code excerpted from src7600/wmi/wmisamp/wmisamp.c 
*/
#include "slayer.h"

#define FIELD_OFFSET     _SLAyer_offsetof
#define ALIGN_UP(len,ty) len

#define __in
#define PVOID void*
#define BOOLEAN int
#define UCHAR int 
#define USHORT int 
#define ULONG int
#define ULONGLONG int
#define CHAR int
#define SHORT int
#define LONG int
#define LONGLONG int
#define VOID void

#define NonPagedPool 0

void RtlCopyMemory(void* Dest, void* Src, int Len)
{
  //  *Dest = *Src; 
}

void* ExAllocatePoolWithTag(unsigned int Pool, unsigned int Size, int Tag)
{
  return malloc(Size);
}


#define EC1_COUNT 4

typedef struct _EC1
{
    // boolean data
    BOOLEAN Xboolean;
    #define EC1_Xboolean_SIZE sizeof(BOOLEAN)
    #define EC1_Xboolean_ID 1

    // unsigned character data
    UCHAR Xuint8;
    #define EC1_Xuint8_SIZE sizeof(UCHAR)
    #define EC1_Xuint8_ID 2

    // unsigned short data
    USHORT Xuint16;
    #define EC1_Xuint16_SIZE sizeof(USHORT)
    #define EC1_Xuint16_ID 3

    // unsigned long data
    ULONG Xuint32;
    #define EC1_Xuint32_SIZE sizeof(ULONG)
    #define EC1_Xuint32_ID 4

    // unsigned long long data
    ULONGLONG Xuint64;
    #define EC1_Xuint64_SIZE sizeof(ULONGLONG)
    #define EC1_Xuint64_ID 5

    // signed byte data
    CHAR Xint8;
    #define EC1_Xint8_SIZE sizeof(CHAR)
    #define EC1_Xint8_ID 6

    // singed short data
    SHORT Xint16;
    #define EC1_Xint16_SIZE sizeof(SHORT)
    #define EC1_Xint16_ID 7

    // singed long data
    LONG Xint32;
    #define EC1_Xint32_SIZE sizeof(LONG)
    #define EC1_Xint32_ID 8

    // signed long long data
    LONGLONG Xint64;
    #define EC1_Xint64_SIZE sizeof(LONGLONG)
    #define EC1_Xint64_ID 9

} EC1, *PEC1;

#define EC1_SIZE (FIELD_OFFSET(EC1, Xint64) + EC1_Xint64_SIZE)

//
// Data storage for WMI data blocks.
//
typedef struct _WMI_SAMPLE_DEVICE_DATA {

    ULONG Ec1Count;
    ULONG Ec1Length[EC1_COUNT];
    ULONG Ec1ActualLength[EC1_COUNT];
    PEC1 Ec1[EC1_COUNT];
/*     WDFSPINLOCK Ec1Lock; */
  
} WMI_SAMPLE_DEVICE_DATA, *PWMI_SAMPLE_DEVICE_DATA;


#define WMI_SAMPLE_TAG (LONG)'SimW'

VOID
WmiSampSetEc1(
    __in PWMI_SAMPLE_DEVICE_DATA WmiDeviceData, 
    __in PVOID Buffer,
    __in ULONG Length,
    __in ULONG Index
    )
{
    PEC1 ec1;
    ULONG ec1Length = ALIGN_UP(Length, PVOID);
    PVOID oldBuffer = NULL;

    if (Index >= EC1_COUNT) {
        return;
    }

    ec1 = ExAllocatePoolWithTag(NonPagedPool, ec1Length, WMI_SAMPLE_TAG);
    if (ec1 != NULL) {

        RtlCopyMemory(ec1, Buffer, Length);

        //
        // Acquire the lock to protect access to the EC1 data since multiple
        // threads could be trying to access the common data concurrently.
        //
/*         WdfSpinLockAcquire(WmiDeviceData->Ec1Lock); */

        oldBuffer = WmiDeviceData->Ec1[Index];
        WmiDeviceData->Ec1[Index] = ec1; // SLAyer: doesn't get stored into. 
        WmiDeviceData->Ec1Length[Index] = ec1Length;
        WmiDeviceData->Ec1ActualLength[Index] = Length;

        //
        // Release the lock.
        //
/*         WdfSpinLockRelease(WmiDeviceData->Ec1Lock); */

        // SLAyer: PS #660.
/*         if (oldBuffer != NULL) { */
/*             ExFreePool(oldBuffer); */
/*         } */
    }

    return;
}


void main()
{

  EC1 Ec1;
  PWMI_SAMPLE_DEVICE_DATA DData;

  // Initialize Ec1: 
  // PS #658 Ec1 = {0};
  Ec1.Xboolean = 0 ; 
  Ec1.Xuint8 = 0;
  Ec1.Xuint16 = 0;
  Ec1.Xuint32 = 0;
  Ec1.Xuint64 = 0;
  Ec1.Xint8 = 0;
  Ec1.Xint16 = 0;
  Ec1.Xint32 = 0;
  Ec1.Xint64 = 0;

  DData = (PWMI_SAMPLE_DEVICE_DATA)malloc(sizeof(WMI_SAMPLE_DEVICE_DATA));
  if (DData != NULL) {
    WmiSampSetEc1(DData, &Ec1, EC1_SIZE,0); 

    // SLAyer: if we do this, we go UNSAFE. 
    //free(DData->Ec1[0]);
    free(DData);
  }

  return;
}
    
