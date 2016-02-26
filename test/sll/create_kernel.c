/*****************************************************************************
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Create and then leak a doubly-linked list.
  This is a kernel-style list, in which the payload is adjacent to the links.

  This is a generalization of the harness setup in
  kmdf/1394/address_range_plist_entry.

 *****************************************************************************/

#include "slayer.h"
#include "../kmdf/harness.h"
#include "../kmdf/1394.h"

void main()
{
  PDEVICE_EXTENSION deviceExtension;
  int* addr_range ;
  ASYNC_ADDRESS_DATA *aad ;
  int aad_count, i ;

  deviceExtension = (PDEVICE_EXTENSION)malloc(sizeof(DEVICE_EXTENSION));
  InitializeListHead(&(deviceExtension->AsyncAddressData));

  for (i=0; i<aad_count; i++)
    {
      aad = (PASYNC_ADDRESS_DATA) malloc(sizeof(ASYNC_ADDRESS_DATA));

      aad->pMdl = (PMDL)malloc(sizeof(MDL));

      aad->Buffer = malloc(1);

      aad->AddressRange = (PADDRESS_RANGE)malloc(sizeof(ADDRESS_RANGE));

      InsertHeadList(&(deviceExtension->AsyncAddressData), &(aad->AsyncAddressList));
    }

  return ;

}
