/* Copyright (c) Microsoft Corporation.  All rights reserved. */

/*
   This is step5.tmh, line 1758.

extern const __declspec(selectany) GUID WPP_LOCAL_TraceGuids[] = { {0x528b9093,0x3035,0xc568,{0x9e,0xfd,0xdd,0x0f,0xd9,0x14,0x13,0x35}}, };

   Check that the WPP+Local_TraceGuids access is valid.
*/

#include "slayer.h"

struct GUID {
  int A;
  int B;
  int C;
  int D[4];
};

// a global guid
//struct GUID a_guid[] = { { 1, 2, 3,  {100,101,102,103}},  } ;
//                                      /\ write to a_guid[0].D[0],...

struct GUID a_guid[1];

void main()
{
  a_guid[0].A = 1 ;
  a_guid[0].B = 2 ;
  a_guid[0].C = 3 ;
  a_guid[0].D[0] = 100 ;
  a_guid[0].D[1] = 101 ;
  a_guid[0].D[2] = 102 ;
  a_guid[0].D[3] = 103 ;
}
