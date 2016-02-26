/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Code from kmdf_vdev_api, line 194.
*/

#define USHORT unsigned short
#define ULONG unsigned long
#define PVOID void*
#define UCHAR unsigned char


typedef struct _ADDRESS_OFFSET {
    USHORT              Off_High;
    ULONG               Off_Low;
} ADDRESS_OFFSET, *PADDRESS_OFFSET;

typedef struct _GET_LOCAL_HOST_INFO6 {
    ADDRESS_OFFSET          CsrBaseAddress;
    ULONG                   CsrDataLength;
    PVOID                   CsrDataBuffer;
} GET_LOCAL_HOST_INFO6, *PGET_LOCAL_HOST_INFO6;

typedef struct _GET_LOCAL_HOST_INFORMATION {
  ULONG Status;
  ULONG nLevel;
  ULONG ulBufferSize;
  UCHAR Information[1];
} GET_LOCAL_HOST_INFORMATION, *PGET_LOCAL_HOST_INFORMATION;


void f(PGET_LOCAL_HOST_INFORMATION GetLocalHostInfo)
{
  GET_LOCAL_HOST_INFO6    LocalHostInfo6  = {0};
  ((PGET_LOCAL_HOST_INFO6)GetLocalHostInfo->Information)->CsrDataLength = \
    LocalHostInfo6.CsrDataLength;
}

void main()
{
  GET_LOCAL_HOST_INFORMATION GetLocalHostInfo;
  GET_LOCAL_HOST_INFO6 LocalHostInfo6;

  (PGET_LOCAL_HOST_INFO6)GetLocalHostInfo.Information = &LocalHostInfo6;

  f(&GetLocalHostInfo);
}
