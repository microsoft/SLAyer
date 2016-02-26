/* Copyright (c) Microsoft Corporation.  All rights reserved. */

#include "harness.h"

void
main () {
  WDFSTRING s; 
  NTSTATUS status;
  
  status = WdfStringCreate(NULL,WDF_NO_OBJECT_ATTRIBUTES,&s);
  if (NT_SUCCESS(status)) {
    WdfObjectDelete(s);
  }
}
