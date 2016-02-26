/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Test case for PS #659: initialize array of structs.

*/
#include "slayer.h"

typedef void VOID_VOID();

typedef VOID_VOID *PFN_VOID_VOID;

typedef struct _GUID {
    unsigned long  Data1;
    unsigned short Data2;
    unsigned short Data3;
    unsigned char  Data4[ 8 ];
} GUID;
#define DEFINE_GUID(name, l, w1, w2, b1, b2, b3, b4, b5, b6, b7, b8)  \
       /*EXTERN_C*/ const GUID /*DECLSPEC_SELECTANY*/ name            \
               = { l, w1, w2, { b1, b2,  b3,  b4,  b5,  b6,  b7,  b8 } }

#define PFN_WDF_WMI_INSTANCE_QUERY_INSTANCE PFN_VOID_VOID
#define PFN_WDF_WMI_INSTANCE_SET_INSTANCE  PFN_VOID_VOID
#define PFN_WDF_WMI_INSTANCE_SET_ITEM PFN_VOID_VOID
#define PFN_WDF_WMI_INSTANCE_EXECUTE_METHOD PFN_VOID_VOID
#define ULONG long

typedef struct _WMI_SAMPLE_INSTANCE_CONFIG {
    GUID Guid;
    ULONG MinSize;
    PFN_WDF_WMI_INSTANCE_QUERY_INSTANCE EvtWmiInstanceQueryInstance;
    PFN_WDF_WMI_INSTANCE_SET_INSTANCE EvtWmiInstanceSetInstance;
    PFN_WDF_WMI_INSTANCE_SET_ITEM EvtWmiInstanceSetItem;
    PFN_WDF_WMI_INSTANCE_EXECUTE_METHOD EvtWmiInstanceExecuteMethod;

} WMI_SAMPLE_INSTANCE_CONFIG, *PWMI_SAMPLE_INSTANCE_CONFIG;

void f() {}
void g() {}
void h() {}
void i() {}

#define WmiSampleClass1Guid \
    { 0x1,0x1,0x1, { 0x1,0x1,0x1,0x1,0x1,0x1,0x1,0x1 } }
#define WmiSampleClass1_SIZE 0
#define EvtWmiClass1DataQueryInstance f
#define EvtWmiClass1DataSetInstance   g
#define EvtWmiClass1DataSetItem       h
#define EvtWmiClass1ExecuteMethod     i

#define WmiSampleClass2Guid \
    { 0x2,0x2,0x2, { 0x2,0x2,0x2,0x2,0x2,0x2,0x2,0x2 } }
#define WmiSampleClass2_SIZE 0
#define EvtWmiClass2DataQueryInstance f
#define EvtWmiClass2DataSetInstance   g
#define EvtWmiClass2DataSetItem       h
#define EvtWmiClass2ExecuteMethod     i

#define WmiSampleClass5Guid \
    { 0x5,0x5,0x5, { 0x5,0x5,0x5,0x5,0x5,0x5,0x5,0x5 } }
#define WmiSampleClass5_SIZE 0
#define EvtWmiClass5DataQueryInstance f
#define EvtWmiClass5DataSetInstance   g
#define EvtWmiClass5DataSetItem       h
#define EvtWmiClass5ExecuteMethod     i

#define WmiSampleClass6Guid \
    { 0x6,0x6,0x6, { 0x6,0x6,0x6,0x6,0x6,0x6,0x6,0x6 } }
#define WmiSampleClass6_SIZE 0
#define EvtWmiClass6DataQueryInstance f
#define EvtWmiClass6DataSetInstance   g
#define EvtWmiClass6DataSetItem       h
#define EvtWmiClass6ExecuteMethod     i

// Array of ints is fine.
int Numbers[] = { 0, 1, 2, 3, };

// Struct by itself is OK.
WMI_SAMPLE_INSTANCE_CONFIG AConfig =
    {
        WmiSampleClass1Guid,
        WmiSampleClass1_SIZE,
        EvtWmiClass1DataQueryInstance,
        EvtWmiClass1DataSetInstance,
        EvtWmiClass1DataSetItem,
        EvtWmiClass1ExecuteMethod
    }
  ;

// An array of Structs isn't.
WMI_SAMPLE_INSTANCE_CONFIG SampleInstanceConfig[] = {
    {
        WmiSampleClass1Guid,
        WmiSampleClass1_SIZE,
        EvtWmiClass1DataQueryInstance,
        EvtWmiClass1DataSetInstance,
        EvtWmiClass1DataSetItem,
        EvtWmiClass1ExecuteMethod
    },

/*     { */
/*         WmiSampleClass2Guid, */
/*         WmiSampleClass2_SIZE, */
/*         EvtWmiClass2DataQueryInstance, */
/*         EvtWmiClass2DataSetInstance, */
/*         NULL, */
/*         NULL */
/*     }, */

/*     { */
/*         WmiSampleClass5Guid, */
/*         WmiSampleClass5_SIZE, */
/*         EvtWmiClass5DataQueryInstance, */
/*         EvtWmiClass5DataSetInstance, */
/*         NULL, */
/*         NULL */
/*     }, */

/*     { */
/*         WmiSampleClass6Guid, */
/*         WmiSampleClass6_SIZE, */
/*         EvtWmiClass6DataQueryInstance, */
/*         EvtWmiClass6DataSetInstance, */
/*         NULL, */
/*         NULL */
/*     }, */
};

void main()
{
  int size;
  size = AConfig.MinSize;
  SampleInstanceConfig[0].MinSize = size;
}
