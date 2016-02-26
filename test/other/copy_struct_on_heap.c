 /*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Check that the fe changes the
    OutBufferSTBD = pFdo->StdToasterBusData
  copy to field-wise
    OutBufferSTBD->ErrorCount = pFdo->StdToasterBusData.ErrorCount;
    ...
  copies.

  Filed as bug PS#620.
*/

#define ULONG long

typedef struct _ToasterBusInformation
{
  ULONG ErrorCount;
  ULONG DebugPrintLevel;
  ULONG Pad;
} ToasterBusInformation, *PToasterBusInformation;

typedef ToasterBusInformation TOASTER_BUS_WMI_STD_DATA, * PTOASTER_BUS_WMI_STD_DATA;

typedef struct _FDO_DEVICE_DATA
{
  ULONG X ;
  TOASTER_BUS_WMI_STD_DATA   StdToasterBusData;

} FDO_DEVICE_DATA, *PFDO_DEVICE_DATA;



void main()
{
  PFDO_DEVICE_DATA pFdo;
  void *OutBufferX;
  void *OutBufferSTBD;
  ULONG *p ;

  pFdo = (PFDO_DEVICE_DATA)malloc(sizeof(FDO_DEVICE_DATA));
  OutBufferX = malloc(sizeof(FDO_DEVICE_DATA));
  OutBufferSTBD = malloc(sizeof(FDO_DEVICE_DATA));

  // Copying a scalar field.
  * (ULONG*) OutBufferX = pFdo->X;

  // Copy a 'struct' field.
  * (PTOASTER_BUS_WMI_STD_DATA) OutBufferSTBD = pFdo->StdToasterBusData;

  free(OutBufferSTBD);
  free(OutBufferX);
  free(pFdo);
  return;
}
