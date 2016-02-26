/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  harness.h is a basic kmdf implementation.

  We can assume that the kmdf environment the driver runs in, is
  safe. What we're trying to catch are the driver's unsafe behaviour.

  A large part of the implementation here is just skip. But: malloc is
  implemented as malloc, free as free. The harness malloc functions,
  like ExAllocatePool, WdfIoCreateCreate, are implemented to allow
  malloc to fail. (The don't-fail branch of these functions calls
  _SLAyer_malloc, whose spec is to always succeed.)

  Definitions are listed in the order in which they appear if you
  #include <wdf.h>. Search for "File:" to find this boundary.

  Search for "Patch:" to find additions/deletions/edits to the original
  File. Search for "ToDo" to find missing items. (ToDo: should file PS bugs
  for them.)

  If you're adding/editing defns, please keep to these conventions.
 */
#ifndef _HARNESS_H_
#define _HARNESS_H_

#include "slayer_intrinsics.h"

/*

Comment:

We're defining kmdf functions based on need, as the drivers we run on
turn up more of them. We find functions by running:

  $ export PRE=Rtl && \
    grep $PRE *.[ch] | sed "s#.*$PRE\([^(]*\).*#$PRE\1#"|sort|uniq > $PRE.1394

for every prefix (Ex, Hal, Io, Ke, Mm, Ps, Rtl, Se, Wdf, Wdm, Zw,
...). And then manually editing the results.

*/

/******************************************************************************
 File: OS Model harness.

 Global state, mimicking kmdf. We have these pointers so as to keep a
 handle on these wdf objects.

******************************************************************************/

/* Driver Object.
   We only ever have one Driver object. */

/* Fwd declaration of types used in OS Model state. */
typedef struct _SLAyer_WDFOBJECT *WDFDRIVER;
WDFDRIVER SL_Driver ;

/*
Device Objects

For the sake of bus drivers, we allow two device objects:
SL_Device_one is Fdo, and SL_Device_two is the child pdo. (We used to
have only one, but toaster can attach many child PDOs. The attach is
not the problem, but toaster also detaches.)
*/
int SL_devices_zero = 0; // If we init these constants,
int SL_devices_one  = 1; // then WdfDriverCreate can test against them.
int SL_devices_two  = 2;

int SL_num_of_devices = 0; // start state

typedef struct _SLAyer_WDFOBJECT *WDFDEVICE;
WDFDEVICE SL_Device_one = NULL;
WDFDEVICE SL_Device_two = NULL;
WDFDEVICE SL_Device     = NULL ; /* should be subsumed by SL_Device_one */

/* IoTarget */
typedef struct _SLAyer_WDFOBJECT *WDFIOTARGET;
WDFIOTARGET SL_IoTarget ;

/* Queue */
typedef struct _SLAyer_WDFOBJECT *WDFQUEUE;
WDFQUEUE SL_Queue;

/* Timer */
typedef struct _SLAyer_WDFOBJECT *WDFTIMER;
WDFTIMER SL_Timer ;

/* DeviceInit */
// PS #644 DeviceInit lifetime
typedef struct _WDFDEVICE_INIT **PWDFDEVICE_INIT;
PWDFDEVICE_INIT SL_WdfDeviceInit;

/******************************************************************************
 * File:
 ******************************************************************************/

// body for returning non-det of any T
#define SLAyer_nondetT(T) T x; return x

/******************************************************************************
 * File: CPP-out some ESP
 ******************************************************************************/
// PS #635: switch off analysis_assume
#define __analysis_assume(x)



/******************************************************************************
 * File: guiddef.h
 ******************************************************************************/
typedef struct _GUID {
    unsigned long  Data1;
    unsigned short Data2;
    unsigned short Data3;
    unsigned char  Data4[ 8 ];
} GUID;
#define DEFINE_GUID(name, l, w1, w2, b1, b2, b3, b4, b5, b6, b7, b8)  \
       /*EXTERN_C*/ const GUID /*DECLSPEC_SELECTANY*/ name            \
               = { l, w1, w2, { b1, b2,  b3,  b4,  b5,  b6,  b7,  b8 } }

typedef GUID *LPGUID;

//#define DEFINE_GUID(name, l, w1, w2, b1, b2, b3, b4, b5, b6, b7, b8)  \
//  GUID name ;
//       /*EXTERN_C*/ const GUID /*DECLSPEC_SELECTANY*/ name            \
//               = { l, w1, w2, { b1, b2,  b3,  b4,  b5,  b6,  b7,  b8 } }

#define __MIDL_CONST const
#define REFGUID const GUID * __MIDL_CONST

// SLayer implementation of IsEqualGUID. See PS #646.
unsigned int IsEqualGUID(REFGUID pg1, REFGUID pg2)
{
  return (pg1->Data1 == pg2->Data1) &&
    (pg1->Data2 == pg2->Data2) &&
    (pg1->Data3 == pg2->Data3);
    /* SLAyer: array memory model imprecise, ignore Data4 array.*/
    /*  && (pg1->Data4[*] == pg2->Data4[*])  */
}

// Mimic pragmas, etc.
// SI: I don't think these are in guiddef.h.

#define FORCEINLINE

#define __drv_aliasesMem
#define __drv_functionClass(EVT_WDF_OBJECT_CONTEXT_CLEANUP)
#define __drv_sameIRQL
#define __drv_maxIRQL(DISPATCH_LEVEL)
#define __drv_freesMem(Mem)
//slayer.h now links to assert.h -> crtdefs.h -> sal.h, so these are no longer required
//#define __checkReturn
//#define __in
//#define __inout
//#define __out
//#define __in_opt
#define WDFAPI

#ifndef __LPCGUID_DEFINED__
#define __LPCGUID_DEFINED__
typedef const GUID *LPCGUID;
#endif

#define IN __in
#define OUT __out

#define UNREFERENCED_PARAMETER(x) x=x
#define PAGED_CODE()

/******************************************************************************
 * File: crt
 ******************************************************************************/

// Mimic crt.
//slayer.h now links to stdlib.h, so this are no longer required
//#define NULL 0

typedef unsigned int BOOLEAN;
#define TRUE 1 //(1==1)
#define FALSE 0 //(~(TRUE))

/* SI: ASSERTs from toaster's source. They tend to be about length of
   buffers, which SLAyer can't do anything about. We drop them for
   now, but maybe better to keep them as an assume?  */
#define ASSERT(x)
#define WDFVERIFY(x)

typedef unsigned int NTSTATUS;

// Some of these are definedin ntintsafe.h.
typedef long LONG;
typedef unsigned short USHORT;
typedef USHORT *PUSHORT;
typedef unsigned long ULONG;
typedef ULONG *ULONG_PTR;
typedef ULONG *PULONG;
typedef long *PLONGLONG;
typedef long long LONGLONG;
typedef void VOID;
typedef void *PVOID;
#ifndef _SIZE_T_DEFINED
typedef unsigned int size_t;
#endif
#define SIZE_T size_t
typedef unsigned char UCHAR;
typedef unsigned short WCHAR;
typedef char* PCHAR;
typedef char* PUCHAR;
typedef WCHAR* PWCHAR;
typedef WCHAR *PWCH;
typedef WCHAR *PWSTR;
typedef const WCHAR *PCWSTR;
#define CSHORT int
#define USHORT int
typedef unsigned __int64 UINT64, *PUINT64;

/*****************************************************************************
 * File: shared/WTypesbase.h
 ****************************************************************************/
typedef double LONGLONG;
// The implementation of this is different to the one found in system
// header files. There seems to be at least two different
// implementations. See shared/ntdef.h:line 930 and
// shared/WTypesbase.h:166.
typedef struct _LARGE_INTEGER {
    // struct {
        ULONG LowPart;
        LONG HighPart;
    // } DUMMYSTRUCTNAME;
    // struct {
    //     ULONG LowPart;
    //     LONG HighPart;
    // } u;
    LONGLONG QuadPart;
} LARGE_INTEGER;

/*****************************************************************************
 * File: ntdef.h
 ****************************************************************************/
typedef double ULONGLONG;

// line 989
//typedef VOID PHYSICAL_ADDRESS, *PPHYSICAL_ADDRESS;
#define PHYSICAL_ADDRESS void*
#define PPHYSICAL_ADDRESS void**

/****************************************************************************
 * File: ntstatus.h
 ****************************************************************************/
#define STATUS_SUCCESS                  0x0
#define STATUS_INSUFFICIENT_RESOURCES   0x1
#define STATUS_INVALID_PARAMETER        0x2
#define STATUS_MORE_PROCESSING_REQUIRED 0x3
#define STATUS_BUFFER_TOO_SMALL 0x4
#define STATUS_PENDING 0x5
#define STATUS_UNSUCCESSFUL             0x6
#define STATUS_INVALID_DEVICE_REQUEST   0x7

#define STATUS_INVALID_ADDRESS           ((NTSTATUS)0xC0000141L)

#define STATUS_DEVICE_DATA_ERROR         ((NTSTATUS)0xC000009CL)
#define STATUS_DEVICE_NOT_READY          ((NTSTATUS)0xC00000A3L)
#define STATUS_DEVICE_NOT_CONNECTED      ((NTSTATUS)0xC000009DL)
#define STATUS_DEVICE_OFF_LINE           ((NTSTATUS)0x80000010L)

#define STATUS_NOT_SUPPORTED             ((NTSTATUS)0xC00000BBL)

#define STATUS_OBJECT_NAME_EXISTS        ((NTSTATUS)0x40000000L)

#define STATUS_NO_MORE_ENTRIES           ((NTSTATUS)0x8000001AL)

#define STATUS_NO_SUCH_DEVICE            ((NTSTATUS)0xC000000EL)
#define STATUS_WMI_READ_ONLY             ((NTSTATUS)0xC00002C6L)

// Copied from line 2317
#define STATUS_INVALID_PARAMETER_MIX     ((NTSTATUS)0xC0000030L)

// line 5406
#define STATUS_DEVICE_CONFIGURATION_ERROR ((NTSTATUS)0xC0000182L)

// Copied from line 7053
#define STATUS_WMI_ITEMID_NOT_FOUND      ((NTSTATUS)0xC0000297L)

/******************************************************************************
 * File: ntintsafe.h.
 ******************************************************************************/
#define NT_SUCCESS(Status) (Status == STATUS_SUCCESS)

NTSTATUS RtlSizeTMult(
  /* _In_ */   SIZE_T Multiplicand,
  /* _In_ */   SIZE_T Multiplier,
  /* _Out_  */ SIZE_T *pResult
)
{
  SLAyer_nondetT(NTSTATUS);
}

/******************************************************************************
 * File: ntdef.h.
 ******************************************************************************/

typedef char CCHAR;
typedef CCHAR *PCCHAR;

// Copied from WDK/inc/api/ntdef.h, line 636.
typedef void *HANDLE;
#define DECLARE_HANDLE(name) typedef HANDLE name

// Copied from WDK/inc/api/ntdef.h, line 1287.
typedef struct _UNICODE_STRING {
  unsigned short Length;
  unsigned short MaximumLength;
  PWSTR       Buffer;
} UNICODE_STRING, *PUNICODE_STRING;
typedef const UNICODE_STRING *PCUNICODE_STRING;

// Copied from $SLAM/WDK/inc/api/ntdef.h, line 1358.
typedef struct _LIST_ENTRY {
  struct _LIST_ENTRY *Flink;
  struct _LIST_ENTRY *Blink;
} LIST_ENTRY, *PLIST_ENTRY;

//copied from WDk/inc/shared/ntdef.h, line 1343
#define UNICODE_NULL ((WCHAR)0) // winnt

//copied from WDk/inc/shared/ntdef.h, line 1350
 #define DECLARE_CONST_UNICODE_STRING(_var, _string) \
const WCHAR _var ## _buffer[] = _string; \
__pragma(warning(push)) \
__pragma(warning(disable:4221)) __pragma(warning(disable:4204)) \
const UNICODE_STRING _var = { sizeof(_string) - sizeof(WCHAR), sizeof(_string), (PWCH) _var ## _buffer } \
__pragma(warning(pop))

//copied from WDk/inc/shared/ntdef.h, line 1360
#define DECLARE_UNICODE_STRING_SIZE(_var, _size) \
WCHAR _var ## _buffer[_size]; \
__pragma(warning(push)) \
__pragma(warning(disable:4221)) __pragma(warning(disable:4204)) \
UNICODE_STRING _var = { 0, (_size) * sizeof(WCHAR) , _var ## _buffer } \
__pragma(warning(pop))

// line 1398
typedef struct _SINGLE_LIST_ENTRY {
    struct _SINGLE_LIST_ENTRY *Next;
} SINGLE_LIST_ENTRY, *PSINGLE_LIST_ENTRY;

/****************************************************************************
 * File: ntddk.h
 ****************************************************************************/
// line 5292
#define FILE_DEVICE_NETWORK             0x00000012

 // copied from line 5374
 //
// Define the method codes for how buffers are passed for I/O and FS controls
//

#define METHOD_BUFFERED                 0
#define METHOD_IN_DIRECT                1
#define METHOD_OUT_DIRECT               2
#define METHOD_NEITHER                  3

/******************************************************************************
 * File: winnt.h
 ******************************************************************************/
//copied from line 8560

#define STANDARD_RIGHTS_ALL              (0x001F0000L)

/*****************************************************************************
 * File: wdm.h
 ****************************************************************************/

// line 5904
#define FILE_SHARE_READ                 0x00000001
#define FILE_SHARE_WRITE                0x00000002

// line 5967
#define FILE_OPEN                       0x00000001

// line 5985
#define FILE_NON_DIRECTORY_FILE                 0x00000040

// line 6048
#define FILE_OCTA_ALIGNMENT             0x0000000f

// line 6085
typedef struct _IO_STATUS_BLOCK {
    union {
        NTSTATUS Status;
        PVOID Pointer;
    } DUMMYUNIONNAME;
    ULONG_PTR Information;
} IO_STATUS_BLOCK, *PIO_STATUS_BLOCK;

DECLARE_HANDLE(KAFFINITY);

#define CmResourceTypePort                1   // ResType_IO (0x0002)
#define CmResourceTypeInterrupt           2   // ResType_IRQ (0x0004)
#define CmResourceTypeMemory              3   // ResType_Mem (0x0001)

// Copied from /cygdrive/c/Program Files (x86)/Windows Kits/8.0/Include/km/wdm.h
// line 8853
// /cygdrive/c/Program Files (x86)/Windows Kits/8.0/Include/km/wdm.h
// line 8853
typedef struct _CM_PARTIAL_RESOURCE_DESCRIPTOR {
    UCHAR Type;
    UCHAR ShareDisposition;
    USHORT Flags;
    union {

        //
        // Range of resources, inclusive.  These are physical, bus relative.
        // It is known that Port and Memory below have the exact same layout
        // as Generic.
        //

        struct {
            PHYSICAL_ADDRESS Start;
            ULONG Length;
        } Generic;

        //
        //

        struct {
            PHYSICAL_ADDRESS Start;
            ULONG Length;
        } Port;

        //
        //

        struct {
#if defined(NT_PROCESSOR_GROUPS)
            USHORT Level;
            USHORT Group;
#else
            ULONG Level;
#endif
            ULONG Vector;
            KAFFINITY Affinity;
        } Interrupt;

        //
        // Values for message signaled interrupts are distinct in the
        // raw and translated cases.
        //

        struct {
            union {
               struct {
#if defined(NT_PROCESSOR_GROUPS)
                   USHORT Group;
#else
                   USHORT Reserved;
#endif
                   USHORT MessageCount;
                   ULONG Vector;
                   KAFFINITY Affinity;
               } Raw;

               struct {
#if defined(NT_PROCESSOR_GROUPS)
                   USHORT Level;
                   USHORT Group;
#else
                   ULONG Level;
#endif
                   ULONG Vector;
                   KAFFINITY Affinity;
               } Translated;
            } DUMMYUNIONNAME;
        } MessageInterrupt;

        //
        // Range of memory addresses, inclusive. These are physical, bus
        // relative. The value should be the same as the one passed to
        // HalTranslateBusAddress().
        //

        struct {
            PHYSICAL_ADDRESS Start;    // 64 bit physical addresses.
            ULONG Length;
        } Memory;

        //
        // Physical DMA channel.
        //

        struct {
            ULONG Channel;
            ULONG Port;
            ULONG Reserved1;
        } Dma;

        struct {
            ULONG Channel;
            ULONG RequestLine;
            UCHAR TransferWidth;
            UCHAR Reserved1;
            UCHAR Reserved2;
            UCHAR Reserved3;
        } DmaV3;

        //
        // Device driver private data, usually used to help it figure
        // what the resource assignments decisions that were made.
        //

        struct {
            ULONG Data[3];
        } DevicePrivate;

        //
        // Bus Number information.
        //

        struct {
            ULONG Start;
            ULONG Length;
            ULONG Reserved;
        } BusNumber;

        //
        // Device Specific information defined by the driver.
        // The DataSize field indicates the size of the data in bytes. The
        // data is located immediately after the DeviceSpecificData field in
        // the structure.
        //

        struct {
            ULONG DataSize;
            ULONG Reserved1;
            ULONG Reserved2;
        } DeviceSpecificData;

        // The following structures provide support for memory-mapped
        // IO resources greater than MAXULONG
        struct {
            PHYSICAL_ADDRESS Start;
            ULONG Length40;
        } Memory40;

        struct {
            PHYSICAL_ADDRESS Start;
            ULONG Length48;
        } Memory48;

        struct {
            PHYSICAL_ADDRESS Start;
            ULONG Length64;
        } Memory64;

        struct {
            UCHAR Class;
            UCHAR Type;
            UCHAR Reserved1;
            UCHAR Reserved2;
            ULONG IdLowPart;
            ULONG IdHighPart;
        } Connection;

    } u;
} CM_PARTIAL_RESOURCE_DESCRIPTOR, *PCM_PARTIAL_RESOURCE_DESCRIPTOR;


// Line: 4398
//
// Define the various device type values.  Note that values used by Microsoft
// Corporation are in the range 0-32767, and 32768-65535 are reserved for use
// by customers.
//

#define DEVICE_TYPE ULONG
// Line: 4447
#define FILE_DEVICE_BUS_EXTENDER        0x0000002a

// Line 5129
//
// Define the I/O bus interface types.
//

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

typedef struct _DEVICE_OBJECT {
  short Type;
  struct _DEVOBJ_EXTENSION  * DeviceObjectExtension ;
} DEVICE_OBJECT, *PDEVICE_OBJECT ;

// Copied from $SLAM/WDK/inc/ddk/wdm.h, line 7403.

//
//  Doubly-linked list manipulation routines.
//

//
//  VOID
//  InitializeListHead32(
//      PLIST_ENTRY32 ListHead
//      );
//

#define InitializeListHead32(ListHead) (\
    (ListHead)->Flink = (ListHead)->Blink = PtrToUlong((ListHead)))

//#if !defined(MIDL_PASS) && !defined(SORTPP_PASS)

//#define RTL_STATIC_LIST_HEAD(x) LIST_ENTRY x = { &x, &x }

FORCEINLINE
VOID
InitializeListHead(
    __out PLIST_ENTRY ListHead
    )
{
  ListHead->Flink = ListHead->Blink = ListHead;
}

__checkReturn
BOOLEAN
FORCEINLINE
IsListEmpty(
    __in const LIST_ENTRY * ListHead
    )
{
    return (BOOLEAN)(ListHead->Flink == ListHead);
}

FORCEINLINE
BOOLEAN
RemoveEntryList(
    __in PLIST_ENTRY Entry
    )
{
    PLIST_ENTRY Blink;
    PLIST_ENTRY Flink;

    Flink = Entry->Flink;
    Blink = Entry->Blink;
    Blink->Flink = Flink;
    Flink->Blink = Blink;
    return (BOOLEAN)(Flink == Blink);
}

FORCEINLINE
PLIST_ENTRY
RemoveHeadList(
    __inout PLIST_ENTRY ListHead
    )
{
    PLIST_ENTRY Flink;
    PLIST_ENTRY Entry;

    Entry = ListHead->Flink;
    Flink = Entry->Flink;
    ListHead->Flink = Flink;
    Flink->Blink = ListHead;
    return Entry;
}

FORCEINLINE
PLIST_ENTRY
RemoveTailList(
    __inout PLIST_ENTRY ListHead
    )
{
    PLIST_ENTRY Blink;
    PLIST_ENTRY Entry;

    Entry = ListHead->Blink;
    Blink = Entry->Blink;
    ListHead->Blink = Blink;
    Blink->Flink = ListHead;
    return Entry;
}

FORCEINLINE
VOID
InsertTailList(
    __inout PLIST_ENTRY ListHead,
    __inout __drv_aliasesMem PLIST_ENTRY Entry
    )
{
    PLIST_ENTRY Blink;

    Blink = ListHead->Blink;
    Entry->Flink = ListHead;
    Entry->Blink = Blink;
    Blink->Flink = Entry;
    ListHead->Blink = Entry;
}

FORCEINLINE
VOID
InsertHeadList(
    __inout PLIST_ENTRY ListHead,
    __inout __drv_aliasesMem PLIST_ENTRY Entry
    )
{
    PLIST_ENTRY Flink;

    Flink = ListHead->Flink;
    Entry->Flink = Flink;
    Entry->Blink = ListHead;
    Flink->Blink = Entry;
    ListHead->Flink = Entry;
}

FORCEINLINE
VOID
AppendTailList(
    __inout PLIST_ENTRY ListHead,
    __inout PLIST_ENTRY ListToAppend
    )
{
    PLIST_ENTRY ListEnd = ListHead->Blink;

    ListHead->Blink->Flink = ListToAppend;
    ListHead->Blink = ListToAppend->Blink;
    ListToAppend->Blink->Flink = ListHead;
    ListToAppend->Blink = ListEnd;
}
//copied from WDK/include/km/wdm.h ,line 8032
typedef enum _SYSTEM_POWER_STATE {
    PowerSystemUnspecified = 0,
    PowerSystemWorking     = 1,
    PowerSystemSleeping1   = 2,
    PowerSystemSleeping2   = 3,
    PowerSystemSleeping3   = 4,
    PowerSystemHibernate   = 5,
    PowerSystemShutdown    = 6,
    PowerSystemMaximum     = 7
} SYSTEM_POWER_STATE, *PSYSTEM_POWER_STATE;

//copied from WDK/include/km/wdm.h ,line 8056
typedef enum _DEVICE_POWER_STATE {
    PowerDeviceUnspecified = 0,
    PowerDeviceD0,
    PowerDeviceD1,
    PowerDeviceD2,
    PowerDeviceD3,
    PowerDeviceMaximum
} DEVICE_POWER_STATE, *PDEVICE_POWER_STATE;

DECLARE_HANDLE(KAFFINITY);

// Copied from $SLAM/WDK/inc/ddk/wdm.h, line 16298.
//
// Pool Allocation routines (in pool.c)
//

typedef enum _POOL_TYPE {
    NonPagedPool,
    PagedPool,
    NonPagedPoolMustSucceed,
    DontUseThisType,
    NonPagedPoolCacheAligned,
    PagedPoolCacheAligned,
    NonPagedPoolCacheAlignedMustS,
    MaxPoolType,

    //
    // Note these per session types are carefully chosen so that the appropriate
    // masking still applies as well as MaxPoolType above.
    //

    NonPagedPoolSession = 32,
    PagedPoolSession = NonPagedPoolSession + 1,
    NonPagedPoolMustSucceedSession = PagedPoolSession + 1,
    DontUseThisTypeSession = NonPagedPoolMustSucceedSession + 1,
    NonPagedPoolCacheAlignedSession = DontUseThisTypeSession + 1,
    PagedPoolCacheAlignedSession = NonPagedPoolCacheAlignedSession + 1,
    NonPagedPoolCacheAlignedMustSSession = PagedPoolCacheAlignedSession + 1,
} POOL_TYPE;

#define POOL_COLD_ALLOCATION 256     // Note this cannot encode into the header.

#define POOL_QUOTA_FAIL_INSTEAD_OF_RAISE 8
#define POOL_RAISE_IF_ALLOCATION_FAILURE 16

// SLAyer: implemented "maybe malloc".
// line 14611
#define PAGE_SHIFT 12L

PVOID
ExAllocatePoolWithTag(
/*     __in __drv_strictTypeMatch(__drv_typeExpr) */ POOL_TYPE PoolType,
/*     __in */ size_t NumberOfBytes,
/*     __in */ ULONG Tag
    )
{
  int x;
  if (x) {
    return _SLAyer_malloc(NumberOfBytes) ;
  } else {
    return NULL;
  }
}

//_IRQL_requires_max_(PASSIVE_LEVEL)
//DECLSPEC_IMPORT
PVOID
//NTAPI
MmGetSystemRoutineAddress (
    /*_In_*/ PUNICODE_STRING SystemRoutineName
    )
{
  SLAyer_nondetT(PVOID);
}

VOID
ObDereferenceObject(
    /*_In_*/ PVOID Object
    )
{
  return;
}

// SLAyer: incomplete
NTSTATUS
IoUnregisterPlugPlayNotification(
        _In_ __drv_freesMem(Pool) PVOID NotificationEntry
            )
{
  SLAyer_nondetT(NTSTATUS);
}

/* Reimplementing this myself, as there seems to be no WDF implementation
 * to be found. This might well be CORREECT, see wdm.h line 29203. --KK
 */
ULONG
IoWMIDeviceObjectToProviderId(
    _In_ PDEVICE_OBJECT DeviceObject
    )
{
  return (ULONG)DeviceObject;
}

__drv_maxIRQL(DISPATCH_LEVEL)
//NTKERNELAPI
VOID
//NTAPI
ExFreePool(
    __in __drv_freesMem(Mem) PVOID P
    )
{
  _SLAyer_free(P);
}

//#endif

//#if (NTDDI_VERSION >= NTDDI_WIN2K)

// Patch:
__drv_maxIRQL(DISPATCH_LEVEL)
//NTKERNELAPI
VOID
ExFreePoolWithTag(
    __in __drv_freesMem(Mem) PVOID P,
    __in ULONG Tag
    )
{
  _SLAyer_free(P);
}
//#endif

// SI: stub, shouldn't be used right now.
VOID RtlZeroMemory(VOID* Dest, size_t Len) {}

VOID RtlCopyMemory(
  /*_Out_*/  VOID UNALIGNED *Destination,
  /*_In_*/   const VOID UNALIGNED *Source,
  /*_In_*/   SIZE_T Length
)
{
}

VOID RtlInitUnicodeString(PUNICODE_STRING DestinationString, PCWSTR SourceString) {}

NTSTATUS
RtlUnicodeStringPrintf(PVOID buffer, char* fmt, ULONG SerialNo)
{
  SLAyer_nondetT(NTSTATUS);
}

typedef /* __struct_bcount(Size) */ struct _MDL {
  struct _MDL *Next;
  CSHORT Size;
  CSHORT MdlFlags;
  //    struct _EPROCESS *Process;
    PVOID MappedSystemVa;
    PVOID StartVa;
    ULONG ByteCount;
    ULONG ByteOffset;
} MDL, *PMDL;

// line 19892
typedef enum _MEMORY_CACHING_TYPE_ORIG {
    MmFrameBufferCached = 2
} MEMORY_CACHING_TYPE_ORIG;

typedef enum _MEMORY_CACHING_TYPE {
    MmNonCached = FALSE,
    MmCached = TRUE,
    MmWriteCombined = MmFrameBufferCached,
    MmHardwareCoherentCached,
    MmNonCachedUnordered,       // IA64
    MmUSWCCached,
    MmMaximumCacheType
} MEMORY_CACHING_TYPE;

// line 21279.
//
// I/O Request Packet (IRP) definition
//
// Patch: most fields commented out.
typedef struct /* DECLSPEC_ALIGN(MEMORY_ALLOCATION_ALIGNMENT) */ _IRP {
    CSHORT Type;
    USHORT Size;

    //
    // Define the common fields used to control the IRP.
    //

    //
    // Define a pointer to the Memory Descriptor List (MDL) for this I/O
    // request.  This field is only used if the I/O is "direct I/O".
    //

    PMDL MdlAddress;

    //
    // Flags word - used to remember various flags.
    //

    ULONG Flags;

/*     // */
/*     // The following union is used for one of three purposes: */
/*     // */
/*     //    1. This IRP is an associated IRP.  The field is a pointer to a master */
/*     //       IRP. */
/*     // */
/*     //    2. This is the master IRP.  The field is the count of the number of */
/*     //       IRPs which must complete (associated IRPs) before the master can */
/*     //       complete. */
/*     // */
/*     //    3. This operation is being buffered and the field is the address of */
/*     //       the system space buffer. */
/*     // */

/*     union { */
/*         struct _IRP *MasterIrp; */
/*         __volatile LONG IrpCount; */
/*         PVOID SystemBuffer; */
/*     } AssociatedIrp; */

/*     // */
/*     // Thread list entry - allows queueing the IRP to the thread pending I/O */
/*     // request packet list. */
/*     // */

/*     LIST_ENTRY ThreadListEntry; */

/*     // */
/*     // I/O status - final status of operation. */
/*     // */

/*     IO_STATUS_BLOCK IoStatus; */

/*     // */
/*     // Requestor mode - mode of the original requestor of this operation. */
/*     // */

/*     KPROCESSOR_MODE RequestorMode; */

/*     // */
/*     // Pending returned - TRUE if pending was initially returned as the */
/*     // status for this packet. */
/*     // */

/*     BOOLEAN PendingReturned; */

/*     // */
/*     // Stack state information. */
/*     // */

/*     CHAR StackCount; */
/*     CHAR CurrentLocation; */

/*     // */
/*     // Cancel - packet has been canceled. */
/*     // */

/*     BOOLEAN Cancel; */

/*     // */
/*     // Cancel Irql - Irql at which the cancel spinlock was acquired. */
/*     // */

/*     KIRQL CancelIrql; */

/*     // */
/*     // ApcEnvironment - Used to save the APC environment at the time that the */
/*     // packet was initialized. */
/*     // */

/*     CCHAR ApcEnvironment; */

/*     // */
/*     // Allocation control flags. */
/*     // */

/*     UCHAR AllocationFlags; */

/*     // */
/*     // User parameters. */
/*     // */

/*     PIO_STATUS_BLOCK UserIosb; */
/*     PKEVENT UserEvent; */
/*     union { */
/*         struct { */
/*             union { */
/*                 PIO_APC_ROUTINE UserApcRoutine; */
/*                 PVOID IssuingProcess; */
/*             }; */
/*             PVOID UserApcContext; */
/*         } AsynchronousParameters; */
/*         LARGE_INTEGER AllocationSize; */
/*     } Overlay; */

/*     // */
/*     // CancelRoutine - Used to contain the address of a cancel routine supplied */
/*     // by a device driver when the IRP is in a cancelable state. */
/*     // */

/*     __volatile PDRIVER_CANCEL CancelRoutine; */

/*     // */
/*     // Note that the UserBuffer parameter is outside of the stack so that I/O */
/*     // completion can copy data back into the user's address space without */
/*     // having to know exactly which service was being invoked.  The length */
/*     // of the copy is stored in the second half of the I/O status block. If */
/*     // the UserBuffer field is NULL, then no copy is performed. */
/*     // */

/*     PVOID UserBuffer; */

/*     // */
/*     // Kernel structures */
/*     // */
/*     // The following section contains kernel structures which the IRP needs */
/*     // in order to place various work information in kernel controller system */
/*     // queues.  Because the size and alignment cannot be controlled, they are */
/*     // placed here at the end so they just hang off and do not affect the */
/*     // alignment of other fields in the IRP. */
/*     // */

/*     union { */

/*         struct { */

/*             union { */

/*                 // */
/*                 // DeviceQueueEntry - The device queue entry field is used to */
/*                 // queue the IRP to the device driver device queue. */
/*                 // */

/*                 KDEVICE_QUEUE_ENTRY DeviceQueueEntry; */

/*                 struct { */

/*                     // */
/*                     // The following are available to the driver to use in */
/*                     // whatever manner is desired, while the driver owns the */
/*                     // packet. */
/*                     // */

/*                     PVOID DriverContext[4]; */

/*                 } ; */

/*             } ; */

/*             // */
/*             // Thread - pointer to caller's Thread Control Block. */
/*             // */

/*             PETHREAD Thread; */

/*             // */
/*             // Auxiliary buffer - pointer to any auxiliary buffer that is */
/*             // required to pass information to a driver that is not contained */
/*             // in a normal buffer. */
/*             // */

/*             PCHAR AuxiliaryBuffer; */

/*             // */
/*             // The following unnamed structure must be exactly identical */
/*             // to the unnamed structure used in the minipacket header used */
/*             // for completion queue entries. */
/*             // */

/*             struct { */

/*                 // */
/*                 // List entry - used to queue the packet to completion queue, among */
/*                 // others. */
/*                 // */

/*                 LIST_ENTRY ListEntry; */

/*                 union { */

/*                     // */
/*                     // Current stack location - contains a pointer to the current */
/*                     // IO_STACK_LOCATION structure in the IRP stack.  This field */
/*                     // should never be directly accessed by drivers.  They should */
/*                     // use the standard functions. */
/*                     // */

/*                     struct _IO_STACK_LOCATION *CurrentStackLocation; */

/*                     // */
/*                     // Minipacket type. */
/*                     // */

/*                     ULONG PacketType; */
/*                 }; */
/*             }; */

/*             // */
/*             // Original file object - pointer to the original file object */
/*             // that was used to open the file.  This field is owned by the */
/*             // I/O system and should not be used by any other drivers. */
/*             // */

/*             PFILE_OBJECT OriginalFileObject; */

/*         } Overlay; */

/*         // */
/*         // APC - This APC control block is used for the special kernel APC as */
/*         // well as for the caller's APC, if one was specified in the original */
/*         // argument list.  If so, then the APC is reused for the normal APC for */
/*         // whatever mode the caller was in and the "special" routine that is */
/*         // invoked before the APC gets control simply deallocates the IRP. */
/*         // */

/*         KAPC Apc; */

/*         // */
/*         // CompletionKey - This is the key that is used to distinguish */
/*         // individual I/O operations initiated on a single file handle. */
/*         // */

/*         PVOID CompletionKey; */

/*     } Tail; */

} IRP;

typedef IRP *PIRP;

// PS #640 Consider generalizing return values
// Patch:
PMDL IoAllocateMdl(
  /* __in_opt */     PVOID VirtualAddress,
  /*__in*/         ULONG Length,
  /*__in*/         BOOLEAN SecondaryBuffer,
  /*__in*/         BOOLEAN ChargeQuota,
  /*__inout_opt*/  PIRP Irp
)
{
  int size ;
  return _SLAyer_malloc(size) ;
}

VOID IoFreeMdl (PMDL Mdl)
{
  _SLAyer_free(Mdl);
}

// Line: 28061
//
// Define structure returned in response to IRP_MN_QUERY_BUS_INFORMATION by a
// PDO indicating the type of bus the device exists on.
//

typedef struct _PNP_BUS_INFORMATION {
    GUID BusTypeGuid;
    INTERFACE_TYPE LegacyBusType;
    ULONG BusNumber;
} PNP_BUS_INFORMATION, *PPNP_BUS_INFORMATION;

 //copied from line 5785
 //
// Macro definition for defining IOCTL and FSCTL function control codes.  Note
// that function codes 0-2047 are reserved for Microsoft Corporation, and
// 2048-4095 are reserved for customers.
//

#define CTL_CODE( DeviceType, Function, Method, Access ) (                 \
    ((DeviceType) << 16) | ((Access) << 14) | ((Function) << 2) | (Method) \
)

//copied from line 5851

#define FILE_READ_DATA            ( 0x0001 )    // file & pipe

//copied from line 11243

// SLAyer: implement printf as NOP
#define KdPrint(x)

/* ULONG */
/* __cdecl */
/* DbgPrint ( */
/*     _In_  PCHAR Format, */
/* 	//_In_z_ _Printf_format_string_ PCSTR Format, */
/*     ... */
/*     ) */
/* { */
/* } */
#define DbgPrint(x) 0L

//copied from line 24848
typedef
NTSTATUS
DRIVER_INITIALIZE (
    _In_ struct _DRIVER_OBJECT *DriverObject,
    _In_ PUNICODE_STRING RegistryPath
    );

typedef DRIVER_INITIALIZE *PDRIVER_INITIALIZE;

// Copied from line 14200
#ifndef FIELD_OFFSET
#define FIELD_OFFSET(type, field) ((ULONG)&(((type *)0)->field))
#endif

typedef unsigned __int64    ULONG64, *PULONG64;

// line 16380
#define KI_USER_SHARED_DATA 0xFFFFF78000000000UI64
#define SharedSystemTime (KI_USER_SHARED_DATA + 0x14)
#define KeQuerySystemTime(CurrentCount)                                     \


// line 29191
/* PS #637 --KK */
NTSTATUS
IoWMIWriteEvent(
    IN PVOID WnodeEventItem
    )
{
    NTSTATUS status;
    return status;
}
//
// line 30100
typedef
// _Function_class_(GET_SET_DEVICE_DATA)
// _IRQL_requires_same_
ULONG GET_SET_DEVICE_DATA (
    // _Inout_opt_
    PVOID Context,
    // _In_
    ULONG DataType,
    // _Inout_updates_bytes_(Length)
    PVOID Buffer,
    // _In_
    ULONG Offset,
    // _In_
    ULONG Length
    );
typedef GET_SET_DEVICE_DATA *PGET_SET_DEVICE_DATA;

typedef struct _BUS_INTERFACE_STANDARD {
    /* generic interface header */
    USHORT Size;
    // USHORT Version;
    PVOID Context;
    // PINTERFACE_REFERENCE InterfaceReference;
    // PINTERFACE_DEREFERENCE InterfaceDereference;
    /* standard bus interfaces */
    // PTRANSLATE_BUS_ADDRESS TranslateBusAddress;
    // PGET_DMA_ADAPTER GetDmaAdapter;
    PGET_SET_DEVICE_DATA SetBusData;
    PGET_SET_DEVICE_DATA GetBusData;
} BUS_INTERFACE_STANDARD, *PBUS_INTERFACE_STANDARD;

// Copied from line 30049
typedef enum {
    DevicePropertyDeviceDescription = 0x0 /* | __string_type */ ,
/*
    DevicePropertyHardwareID = 0x1 | __multiString_type,
    DevicePropertyCompatibleIDs = 0x2 | __multiString_type,
    DevicePropertyBootConfiguration = 0x3,
    DevicePropertyBootConfigurationTranslated = 0x4,
    DevicePropertyClassName = 0x5 | __string_type,
    DevicePropertyClassGuid = 0x6 | __string_type,
    DevicePropertyDriverKeyName = 0x7 | __string_type,
    DevicePropertyManufacturer = 0x8 | __string_type,
*/
    DevicePropertyFriendlyName = 0x9 /* | __string_type*/,
/*
    DevicePropertyLocationInformation = 0xa | __string_type,
    DevicePropertyPhysicalDeviceObjectName = 0xb | __string_type,
    DevicePropertyBusTypeGuid = 0xc | __guid_type,
    DevicePropertyLegacyBusType = 0xd,
    DevicePropertyBusNumber = 0xe,
    DevicePropertyEnumeratorName = 0xf | __string_type,
    DevicePropertyAddress = 0x10,
*/
    DevicePropertyUINumber = 0x11,
/*
    DevicePropertyInstallState = 0x12,
    DevicePropertyRemovalPolicy = 0x13,
    DevicePropertyResourceRequirements = 0x14,
    DevicePropertyAllocatedResources = 0x15,
    DevicePropertyContainerID = 0x16 | __string_type
*/
} DEVICE_REGISTRY_PROPERTY;

// copied from line 29298
typedef
//_Function_class_(WMI_NOTIFICATION_CALLBACK)
//_IRQL_requires_same_
VOID FWMI_NOTIFICATION_CALLBACK (
    PVOID Wnode,
    PVOID Context
    );
typedef FWMI_NOTIFICATION_CALLBACK *WMI_NOTIFICATION_CALLBACK;

//copied from line 30667
#define PLUGPLAY_REGKEY_DEVICE  1

// Copied from line 30778
typedef
//_Function_class_(DRIVER_NOTIFICATION_CALLBACK_ROUTINE)
//_IRQL_requires_max_(PASSIVE_LEVEL)
NTSTATUS
DRIVER_NOTIFICATION_CALLBACK_ROUTINE (
    /*_In_*/ PVOID NotificationStructure,
    /*_Inout_opt_*/ PVOID Context
);
typedef DRIVER_NOTIFICATION_CALLBACK_ROUTINE
    *PDRIVER_NOTIFICATION_CALLBACK_ROUTINE;

// Copied from line 30764
typedef enum _IO_NOTIFICATION_EVENT_CATEGORY {
  EventCategoryReserved,
  EventCategoryHardwareProfileChange,
  EventCategoryDeviceInterfaceChange,
  EventCategoryTargetDeviceChange
} IO_NOTIFICATION_EVENT_CATEGORY;

DECLARE_HANDLE(PDRIVER_OBJECT); // SI: defined in ddk/wdm.h

// SLAyer: incomplete
/* Reimplementing this myself, as there seems to be no WDF implementation
 * to be found. THIS IS PROBABLY INCORRECT! --KK #PS636
 */
NTSTATUS
IoRegisterPlugPlayNotification(
    /*_In_*/ IO_NOTIFICATION_EVENT_CATEGORY EventCategory,
    /*_In_*/ ULONG EventCategoryFlags,
    /*_In_opt_*/ PVOID EventCategoryData,
    /*_In_*/ PDRIVER_OBJECT DriverObject,
    /*_In_*/ PDRIVER_NOTIFICATION_CALLBACK_ROUTINE CallbackRoutine,
    /*_Inout_opt_ __drv_aliasesMem*/ PVOID Context,
    /*_Outptr_result_nullonfailure_*/
    /*_At_(*NotificationEntry,
        _When_(return==0, __drv_allocatesMem(Mem)))*/
    PVOID *NotificationEntry
    )
{
  SLAyer_nondetT(NTSTATUS);
}

#define PNPNOTIFY_DEVICE_INTERFACE_INCLUDE_EXISTING_INTERFACES    0x00000001

// copied from line 30916
typedef struct _DEVICE_INTERFACE_CHANGE_NOTIFICATION {
    USHORT Version;
    USHORT Size;
    GUID Event;
    //
    // Event-specific data
    //
    GUID InterfaceClassGuid;
    PUNICODE_STRING SymbolicLinkName;
} DEVICE_INTERFACE_CHANGE_NOTIFICATION, *PDEVICE_INTERFACE_CHANGE_NOTIFICATION;

// line 31462
typedef struct _SCATTER_GATHER_LIST {
    ULONG NumberOfElements;
    // ULONG_PTR Reserved;
    // SCATTER_GATHER_ELEMENT Elements[];
} SCATTER_GATHER_LIST, *PSCATTER_GATHER_LIST;

// line 32972
#define PCI_TYPE0_ADDRESSES             6
#define PCI_TYPE1_ADDRESSES             2
#define PCI_TYPE2_ADDRESSES             5

// Patch: commenting most fields out
typedef struct _PCI_COMMON_HEADER {
    USHORT  VendorID;                   // (ro)
    USHORT  DeviceID;                   // (ro)
    USHORT  Command;                    // Device control
    /*
    USHORT  Status;
    */
    UCHAR   RevisionID;                 // (ro)
    /*
    UCHAR   ProgIf;                     // (ro)
    UCHAR   SubClass;                   // (ro)
    UCHAR   BaseClass;                  // (ro)
    UCHAR   CacheLineSize;              // (ro+)
    UCHAR   LatencyTimer;               // (ro+)
    UCHAR   HeaderType;                 // (ro)
    UCHAR   BIST;                       // Built in self test
    */
    union {
        struct _PCI_HEADER_TYPE_0 {
            /*
            ULONG   BaseAddresses[PCI_TYPE0_ADDRESSES];
            ULONG   CIS;
            */
            USHORT  SubVendorID;
            USHORT  SubSystemID;
            /*
            ULONG   ROMBaseAddress;
            UCHAR   CapabilitiesPtr;
            UCHAR   Reserved1[3];
            ULONG   Reserved2;
            UCHAR   InterruptLine;      //
            UCHAR   InterruptPin;       // (ro)
            UCHAR   MinimumGrant;       // (ro)
            UCHAR   MaximumLatency;     // (ro)
            */
        } type0;
        /*
        // PCI to PCI Bridge
        struct _PCI_HEADER_TYPE_1 {
            ULONG   BaseAddresses[PCI_TYPE1_ADDRESSES];
            UCHAR   PrimaryBus;
            UCHAR   SecondaryBus;
            UCHAR   SubordinateBus;
            UCHAR   SecondaryLatency;
            UCHAR   IOBase;
            UCHAR   IOLimit;
            USHORT  SecondaryStatus;
            USHORT  MemoryBase;
            USHORT  MemoryLimit;
            USHORT  PrefetchBase;
            USHORT  PrefetchLimit;
            ULONG   PrefetchBaseUpper32;
            ULONG   PrefetchLimitUpper32;
            USHORT  IOBaseUpper16;
            USHORT  IOLimitUpper16;
            UCHAR   CapabilitiesPtr;
            UCHAR   Reserved1[3];
            ULONG   ROMBaseAddress;
            UCHAR   InterruptLine;
            UCHAR   InterruptPin;
            USHORT  BridgeControl;
        } type1;
        // PCI to CARDBUS Bridge
        struct _PCI_HEADER_TYPE_2 {
            ULONG   SocketRegistersBaseAddress;
            UCHAR   CapabilitiesPtr;
            UCHAR   Reserved;
            USHORT  SecondaryStatus;
            UCHAR   PrimaryBus;
            UCHAR   SecondaryBus;
            UCHAR   SubordinateBus;
            UCHAR   SecondaryLatency;
            struct  {
                ULONG   Base;
                ULONG   Limit;
            }       Range[PCI_TYPE2_ADDRESSES-1];
            UCHAR   InterruptLine;
            UCHAR   InterruptPin;
            USHORT  BridgeControl;
        } type2;
        */
    } u;
} PCI_COMMON_HEADER, *PPCI_COMMON_HEADER;

// line 33062
  /* Patch. The code in wdm and miniport uses some weird syntax; I think
   * that this struct `inherits' some fields from PCI_COMMON_HEADER. In
   * particular, VendorID is udes in pci_drv/HW/nic_init.c.
   */
typedef struct _PCI_COMMON_CONFIG {
    USHORT  VendorID;                   // (ro)
    USHORT  DeviceID;                   // (ro)
    USHORT  Command;                    // Device control
    /*
    USHORT  Status;
    */
    UCHAR   RevisionID;                 // (ro)
    /*
    UCHAR   ProgIf;                     // (ro)
    UCHAR   SubClass;                   // (ro)
    UCHAR   BaseClass;                  // (ro)
    UCHAR   CacheLineSize;              // (ro+)
    UCHAR   LatencyTimer;               // (ro+)
    UCHAR   HeaderType;                 // (ro)
    UCHAR   BIST;                       // Built in self test
    */
    union {
        struct _PCI_HEADER_TYPE_0  type0;
        /*
        // PCI to PCI Bridge
        struct _PCI_HEADER_TYPE_1  type1;
        // PCI to CARDBUS Bridge
        struct _PCI_HEADER_TYPE_2  type2;
        */
    } u;
    /* This is an additional field that is defined only in
     * PCI_COMMON_CONFIG and not in PCI_COMMON_HEADER.
     */
    UCHAR DeviceSpecific[192];
} PCI_COMMON_CONFIG, *PPCI_COMMON_CONFIG;

// line 33108
#define PCI_ENABLE_WRITE_AND_INVALIDATE     0x0010

/******************************************************************************
 * File: wdf.h.
 ******************************************************************************/
#define WDF_EXTERN_C

/******************************************************************************
 * File: wdftypes.h.
 ******************************************************************************/

typedef enum _WDF_TRI_STATE {
  WdfFalse = FALSE,
  WdfTrue = TRUE,
  WdfUseDefault = 2,
} WDF_TRI_STATE, *PWDF_TRI_STATE;

typedef PVOID WDFCONTEXT;

DECLARE_HANDLE(WDFDMAENABLER);
DECLARE_HANDLE(WDFDMATRANSACTION);
DECLARE_HANDLE(WDFCOMMONBUFFER);
DECLARE_HANDLE(WDFLOOKASIDE);

// Copied from WDK/inc/kmdf/1.9/wdftypes.h, line 77.
//
// Forward declare structures needed later header files
//
// Patch: added underscore to WDFDEVICE_INIT.

typedef struct _WDF_OBJECT_ATTRIBUTES *PWDF_OBJECT_ATTRIBUTES;

#define WDF_NO_OBJECT_ATTRIBUTES (NULL)
#define WDF_NO_EVENT_CALLBACK (NULL)
#define WDF_NO_HANDLE (NULL)
#define WDF_NO_CONTEXT (NULL)
#define WDF_NO_SEND_OPTIONS (NULL)

// Patch: Never even declared in wd*.h, let alone declared as HANDLE.
DECLARE_HANDLE(PWDF_DRIVER_GLOBALS); // SI: defined in wdfglobals.h
/* Moving this up, as a definition in wdm.h needs it --KK
DECLARE_HANDLE(PDRIVER_OBJECT); // SI: defined in ddk/wdm.h
*/
DECLARE_HANDLE(WDFCMRESLIST); // SI: defined in wdftypes.h.

// Copied from WDK/inc/kmdf/1.9/wdftypes.h, line 91.
//
// General Handle Type, should always be typeless
//
// SLAyer: provide an implementation of WDFOBJECT.
typedef enum _SLAyerObjTyp {
  SLAyerWdfObject,
  SLAyerWdfDevice,
  SLAyerWdfDriver,
  SLAyerWdfIoTarget,
  SLAyerWdfQueue,
  SLAyerWdfRequest,
  SLAyerWdfTimer,
  SLAyerWdfWmiInstance,
} SLAyerObjTyp ;

typedef struct _SLAyer_WDFDEVICE_Ext {
  struct _SLAyer_WDFOBJECT *Queue;      // Device's Queue
  struct _SLAyer_WDFOBJECT *WmiInstance1;// Device's WmiInstance #1
  struct _SLAyer_WDFOBJECT *WmiInstance2;// Device's WmiInstance #2
  struct _SLAyer_WDFOBJECT *WmiInstance3;// Device's WmiInstance #3
} SLAyer_WDFDEVICE_Ext;

typedef struct _SLAyer_WDFQUEUE_Ext {
  struct _SLAyer_WDFOBJECT *Device;
  void *InputBuffer;
} SLAyer_WDFQUEUE_Ext;

typedef struct _SLAyer_WDFREQUEST_Ext {
  int Buffer; // SI: un-used? Seems a duplicate of InputBuffer.
  void *InputBuffer;
} SLAyer_WDFREQUEST_Ext;

typedef struct _SLAyer_WDFWMIINSTANCE_Ext {
  struct _SLAyer_WDFOBJECT *Device;
} SLAyer_WDFWMIINSTANCE_Ext;

/*
SI: we should use Childx to store pointers to the children. For
instance, a SLAyerWdfDevice will have a queue, a couple of
wmiinstances.  Then we won't need these SLAyer_WDFOBJECTs within
SLAyer_WDFx_Ext's.
*/
typedef struct _SLAyer_WDFOBJECT {
  // Framework Memory Management
  unsigned int RefCount;
  // Object Hierarchy
  struct _SLAyer_WDFOBJECT *Parent; // SI: un-used right now?
  struct _SLAyer_WDFOBJECT *Child1, *Child2; // Arbitrarily 2 children.
  // Ctxt Space
  void *Context;
  // Type of WdfObject
  SLAyerObjTyp typ;
  // union TypData {
  SLAyer_WDFDEVICE_Ext      typDevice;
  SLAyer_WDFQUEUE_Ext       typQueue; // SI: typQueue.Device same as Parent?
  SLAyer_WDFREQUEST_Ext     typRequest;
  SLAyer_WDFWMIINSTANCE_Ext typWmiInstance;
  //} typData;
} SLAyer_WDFOBJECT;

typedef SLAyer_WDFOBJECT *WDFOBJECT, **PWDFOBJECT;

//
// core handles
//
typedef SLAyer_WDFOBJECT *WDFDRIVER;
typedef SLAyer_WDFOBJECT *WDFDEVICE;

DECLARE_HANDLE( WDFWMIPROVIDER );
typedef SLAyer_WDFOBJECT *WDFWMIINSTANCE;

typedef SLAyer_WDFOBJECT *WDFQUEUE;

//DECLARE_HANDLE( WDFREQUEST );
// Patch: implementation of opaque type WDFREQUEST.
/*
 typedef struct _WDFREQUEST {
   int              Buffer; // "32 bits should be enough for anybody"
   void *           InputBuffer;
 } *WDFREQUEST;
*/
typedef SLAyer_WDFOBJECT *WDFREQUEST;

typedef struct _WDFBUFFER
{
  long X;
  long Y;
  long Z[4];
} WDFBUFFER;

DECLARE_HANDLE( WDFFILEOBJECT );
DECLARE_HANDLE( WDFDPC );
typedef SLAyer_WDFOBJECT *WDFTIMER;
DECLARE_HANDLE( WDFWORKITEM );
DECLARE_HANDLE( WDFINTERRUPT );

DECLARE_HANDLE( WDFSPINLOCK );
DECLARE_HANDLE( WDFWAITLOCK );

DECLARE_HANDLE( WDFMEMORY );

typedef SLAyer_WDFOBJECT *WDFIOTARGET;

DECLARE_HANDLE( WDFKEY );
DECLARE_HANDLE( WDFSTRING );

//copied from WDK/inc/kmdf/1.5/wdftypes.h, line 141
DECLARE_HANDLE(WDFCOLLECTION);
DECLARE_HANDLE( WDFCHILDLIST );

// SI: reimplement this as a SLAyer_WDFOBJECT sub-type.
/* typedef struct _WDFBUFFER */
/* { */
/*   long X; */
/*   long Y; */
/*   long Z[4]; */
/* } WDFBUFFER; */

/******************************************************************************
 * File: wdfobject.h.
 ******************************************************************************/

// Copied from c:/slam/WDK/inc/wdf/kmdf/1.9/wdfobject.h, line 27.
//
// Specifies the highest IRQL level allowed on callbacks
// to the device driver.
//
typedef enum _WDF_EXECUTION_LEVEL {
    WdfExecutionLevelInvalid = 0x00,
    WdfExecutionLevelInheritFromParent,
    WdfExecutionLevelPassive,
    WdfExecutionLevelDispatch,
} WDF_EXECUTION_LEVEL;

//
// Specifies the concurrency of callbacks to the device driver
//
typedef enum _WDF_SYNCHRONIZATION_SCOPE {
    WdfSynchronizationScopeInvalid = 0x00,
    WdfSynchronizationScopeInheritFromParent,
    WdfSynchronizationScopeDevice,
    WdfSynchronizationScopeQueue,
    WdfSynchronizationScopeNone,
} WDF_SYNCHRONIZATION_SCOPE;

typedef
__drv_functionClass(EVT_WDF_OBJECT_CONTEXT_CLEANUP)
__drv_sameIRQL
__drv_maxIRQL(DISPATCH_LEVEL)
VOID
EVT_WDF_OBJECT_CONTEXT_CLEANUP(
    __in
    WDFOBJECT Object
    );

typedef EVT_WDF_OBJECT_CONTEXT_CLEANUP *PFN_WDF_OBJECT_CONTEXT_CLEANUP;

typedef
__drv_functionClass(EVT_WDF_OBJECT_CONTEXT_DESTROY)
__drv_sameIRQL
__drv_maxIRQL(DISPATCH_LEVEL)
VOID
EVT_WDF_OBJECT_CONTEXT_DESTROY(
    __in
    WDFOBJECT Object
    );

typedef EVT_WDF_OBJECT_CONTEXT_DESTROY *PFN_WDF_OBJECT_CONTEXT_DESTROY;

typedef const struct _WDF_OBJECT_CONTEXT_TYPE_INFO *PCWDF_OBJECT_CONTEXT_TYPE_INFO;

// SLAyer: context per object
typedef void* MK_CONTEXT() ;
typedef MK_CONTEXT *PFN_MK_CONTEXT;
#define SLAyer_MK_CONTEXT_NAME(_contexttype) SLAyer_mk_ ## _contexttype

typedef struct _WDF_OBJECT_ATTRIBUTES {
  ULONG Size;
  PFN_WDF_OBJECT_CONTEXT_CLEANUP EvtCleanupCallback;
  PFN_WDF_OBJECT_CONTEXT_DESTROY EvtDestroyCallback;
  WDF_EXECUTION_LEVEL ExecutionLevel;
  WDF_SYNCHRONIZATION_SCOPE SynchronizationScope;
  WDFOBJECT ParentObject;
  size_t ContextSizeOverride;
  PCWDF_OBJECT_CONTEXT_TYPE_INFO ContextTypeInfo;
  // SLAyer: context per object
  PFN_MK_CONTEXT MkContext;
} WDF_OBJECT_ATTRIBUTES, *PWDF_OBJECT_ATTRIBUTES;

VOID
FORCEINLINE
WDF_OBJECT_ATTRIBUTES_INIT(
    __out PWDF_OBJECT_ATTRIBUTES Attributes
    )
{
  /* Patch: */
  WDF_EXECUTION_LEVEL ParentExecutionLevel;
  WDF_SYNCHRONIZATION_SCOPE ParentSynchronizationScope;
  Attributes->ExecutionLevel=/*WdfExecutionLevelInheritFromParent*/ParentExecutionLevel;
  Attributes->SynchronizationScope=/*WdfSynchronizationScopeInheritFromParent*/ParentSynchronizationScope;
}

// SLAyer: context per object
#define SLAyer_WDF_OBJECT_ATTRIBUTES_SET_MK_CONTEXT(_attributes, _contexttype) \
  (_attributes)->MkContext = &(SLAyer_MK_CONTEXT_NAME(_contexttype))

// SLAyer: context per object. Store into ->MkContext too?
//#define WDF_OBJECT_ATTRIBUTES_SET_CONTEXT_TYPE(_attributes, _contexttype) \
//  (_attributes)->ContextTypeInfo = WDF_GET_CONTEXT_TYPE_INFO(_contexttype)->UniqueType;

#define WDF_OBJECT_ATTRIBUTES_SET_CONTEXT_TYPE(_attributes, _contexttype) \
  SLAyer_WDF_OBJECT_ATTRIBUTES_SET_MK_CONTEXT(_attributes, _contexttype)

//
// VOID
// FORCEINLINE
// WDF_OBJECT_ATTRIBUTES_INIT_CONTEXT_TYPE(
//     PWDF_OBJECT_ATTRIBUTES Attributes,
//     <typename>
//     )
//
// NOTE:  Do not put a ; at the end of the last line.  This will require the
// caller to specify a ; after the call.
//
// SLAyer: context per object
#define WDF_OBJECT_ATTRIBUTES_INIT_CONTEXT_TYPE(_attributes,  _contexttype) \
  WDF_OBJECT_ATTRIBUTES_INIT(_attributes);                              \
  WDF_OBJECT_ATTRIBUTES_SET_CONTEXT_TYPE(_attributes, _contexttype);

typedef
PCWDF_OBJECT_CONTEXT_TYPE_INFO
(__cdecl *PFN_GET_UNIQUE_CONTEXT_TYPE)(
    VOID
    );

//
// Since C does not have strong type checking we must invent our own
//
typedef struct _WDF_OBJECT_CONTEXT_TYPE_INFO {
    //
    // The size of this structure in bytes
    //
    ULONG Size;

    //
    // String representation of the context's type name, i.e. "DEVICE_CONTEXT"
    //
    PCHAR ContextName;

    //
    // The size of the context in bytes.  This will be the size of the context
    // associated with the handle unless
    // WDF_OBJECT_ATTRIBUTES::ContextSizeOverride is specified.
    //
    size_t ContextSize;

    //
    // If NULL, this structure is the unique type identifier for the context
    // type.  If != NULL, the UniqueType pointer value is the unique type id
    // for the context type.
    //
    PCWDF_OBJECT_CONTEXT_TYPE_INFO UniqueType;

    //
    // Function pointer to retrieve the context type information structure
    // pointer from the provider of the context type.  This function is invoked
    // by the client driver's entry point by the KMDF stub after all class
    // drivers are loaded and before DriverEntry is invoked.
    //
    PFN_GET_UNIQUE_CONTEXT_TYPE EvtDriverGetUniqueContextType;

} WDF_OBJECT_CONTEXT_TYPE_INFO, *PWDF_OBJECT_CONTEXT_TYPE_INFO;

//
// Converts a type name a unique name in which we can retrieve type specific
// information.
//
#define WDF_TYPE_NAME_TO_TYPE_INFO(_contexttype) \
      _WDF_ ## _contexttype ## _TYPE_INFO

//
// Converts a type name a unique name to the structure which will initialize
// it through an external component.
//
#define WDF_TYPE_NAME_TO_EXTERNAL_INIT(_contexttype) \
      _WDF_ ## _contexttype ## _EXTERNAL_INIT

#define WDF_TYPE_NAME_TO_EXTERNAL_INIT_FUNCTION(_contexttype) \
      _contexttype ## _EXTERNAL_INIT_FUNCTION

//
// Returns an address to the type information representing this typename
//
#define WDF_GET_CONTEXT_TYPE_INFO(_contexttype) \
    (&WDF_TYPE_NAME_TO_TYPE_INFO(_contexttype))

//
// Used to help generate our own usable pointer to the type typedef.  For instance,
// a call as WDF_TYPE_NAME_POINTER_TYPE(DEVICE_CONTEXT) would generate:
//
// WDF_POINTER_TYPE_DEVICE_CONTEXT
//
// which would be the equivalent of DEVICE_CONTEXT*
//
#define WDF_TYPE_NAME_POINTER_TYPE(_contexttype) \
    WDF_POINTER_TYPE_ ## _contexttype

//
// Declares a typename so that in can be associated with a handle.  This will
// use the type's name with a _ prepended as the "friendly name" (which results
// in the autogenerated casting function being named WdfObjectGet_<typename>, ie
// WdfObjectGet_DEVICE_CONTEXT.  See WDF_DECLARE_CONTEXT_TYPE_WITH_NAME for
// more details on what is generated.
//
#define WDF_DECLARE_CONTEXT_TYPE(_contexttype)  \
        WDF_DECLARE_CONTEXT_TYPE_WITH_NAME(_contexttype, WdfObjectGet_ ## _contexttype)

//
// WDF_DECLARE_CONTEXT_TYPE_WITH_NAME performs the following 3 tasks
//
// 1)  declare a typedef for the context type so that its pointer type can be
//     referred to later
// 2)  declare and initialize global structure that represents the type
//     information for this
//     context type
// 3)  declare and implement a function named _castingfunction
//     which does the proper type conversion.
//
// WDF_DECLARE_TYPE_AND_GLOBALS implements 1 & 2
// WDF_DECLARE_CASTING_FUNCTION implements 3
//
// For instance, the invocation of
// WDF_DECLARE_CONTEXT_TYPE_WITH_NAME(DEVICE_CONTEXT, WdfDeviceGetContext)
// would result in the following being generated:
//
// typedef DEVICE_CONTEXT* WDF_POINTER_TYPE_DEVICE_CONTEXT;
//
// extern const __declspec(selectany) WDF_OBJECT_CONTEXT_TYPE_INFO  _WDF_DEVICE_CONTEXT_TYPE_INFO =
// {
//     sizeof(WDF_OBJECT_CONTEXT_TYPE_INFO),
//     "DEVICE_CONTEXT",
//     sizeof(DEVICE_CONTEXT),
// };
//
// WDF_POINTER_TYPE_DEVICE_CONTEXT
// WdfDeviceGetContext(
//    WDFOBJECT Handle
//    )
// {
//     return (WDF_POINTER_TYPE_DEVICE_CONTEXT)
//         WdfObjectGetTypedContextWorker(
//              Handle,
//              (&_WDF_DEVICE_CONTEXT_TYPE_INFO)->UniqueType
//              );
// }
//
#define WDF_TYPE_INIT_BASE_SECTION_NAME ".kmdftypeinit"
#define WDF_TYPE_INIT_SECTION_NAME      ".kmdftypeinit$b"

//
// .data is the default section that global data would be placed into.  We
// cannot just use ".data" in __declspec(allocate()) without first declaring
// it in a #pragma section() even though it is a default section name.
//
#ifndef WDF_TYPE_DEFAULT_SECTION_NAME
#define WDF_TYPE_DEFAULT_SECTION_NAME ".data"
#endif // WDF_TYPE_DEFAULT_SECTION_NAME

#pragma section(WDF_TYPE_INIT_SECTION_NAME, read, write)
#pragma section(WDF_TYPE_DEFAULT_SECTION_NAME)

#define WDF_DECLARE_TYPE_AND_GLOBALS(_contexttype, _UniqueType, _GetUniqueType, _section)\
                                                                        \
typedef _contexttype* WDF_TYPE_NAME_POINTER_TYPE(_contexttype);         \
                                                                        \
WDF_EXTERN_C __declspec(allocate( _section )) __declspec(selectany) extern const WDF_OBJECT_CONTEXT_TYPE_INFO WDF_TYPE_NAME_TO_TYPE_INFO(_contexttype) =  \
{                                                                       \
    sizeof(WDF_OBJECT_CONTEXT_TYPE_INFO),                               \
    #_contexttype,                                                      \
    sizeof(_contexttype),                                               \
    _UniqueType,                                                        \
    _GetUniqueType,                                                     \
};                                                                      \

#define WDF_DECLARE_CASTING_FUNCTION(_contexttype, _castingfunction)    \
                                                                        \
__drv_aliasesMem                                                        \
WDF_EXTERN_C                                                            \
WDF_TYPE_NAME_POINTER_TYPE(_contexttype)                                \
FORCEINLINE                                                             \
_castingfunction(                                                       \
                 __in WDFOBJECT Handle                                  \
   )                                                                    \
{                                                                       \
    return (WDF_TYPE_NAME_POINTER_TYPE(_contexttype))                   \
        WdfObjectGetTypedContextWorker(                                 \
            Handle,                                                     \
            WDF_GET_CONTEXT_TYPE_INFO(_contexttype)->UniqueType         \
            );                                                          \
}

#define WDF_DECLARE_CONTEXT_TYPE_WITH_NAME(_contexttype, _castingfunction) \
                                                                        \
WDF_DECLARE_TYPE_AND_GLOBALS(                                           \
    _contexttype,                                                       \
    WDF_GET_CONTEXT_TYPE_INFO(_contexttype),                            \
    NULL,                                                               \
    WDF_TYPE_DEFAULT_SECTION_NAME)                                      \
                                                                        \
WDF_DECLARE_CASTING_FUNCTION(_contexttype, _castingfunction)

/*
SLAyer: Patch
SLayer version of WDF_DECLARE_CONTEXT_TYPE_WITH_NAME. We declare two
functions here: SLAyer_mk_ mallocs a context; _castingfunction
returns the object's context. Even though SLayer_mk_ returns a void*,
we need the object to remain a _contexttype. That's also the reason
why _castingfunction takes a SL_WDFOBJECT, rather than a void*.
(We also immediately take the address of the SLAyer_mk_ function, otherwise
it might get optimized away.)
*/
#ifdef WDF_DECLARE_CONTEXT_TYPE_WITH_NAME
#undef WDF_DECLARE_CONTEXT_TYPE_WITH_NAME
#endif
#define WDF_DECLARE_CONTEXT_TYPE_WITH_NAME(_contexttype, _castingfunction) \
                                                                        \
  void* SLAyer_mk_##_contexttype()                                      \
  {                                                                     \
    _contexttype * ctxt_obj;                                            \
    ctxt_obj = (_contexttype *)_SLAyer_malloc(sizeof( _contexttype ));  \
    return ctxt_obj ;                                                   \
  }                                                                     \
                                                                        \
  PFN_MK_CONTEXT SLAyer_use_##_contexttype = &SLAyer_mk_##_contexttype; \
                                                                        \
  _contexttype * _castingfunction(SLAyer_WDFOBJECT * Handle)            \
  {									\
    return (Handle->Context);                                          \
  }

/* These two are declared in func/featured/toasterMof.h
DECLARE_HANDLE(PToasterDeviceInformation);
DECLARE_HANDLE(PToasterControl);
*/

//
// WDF_DECLARE_SHARED_CONTEXT_TYPE_WITH_NAME is the same as
// WDF_DECLARE_CONTEXT_TYPE_WITH_NAME with respect to the types and structures
// that are created and initialized.  The casting function is different in that
// it passes the UniqueType to WdfObjectGetTypedContextWorker() instead of the
// global type structure created.  It also creates a structure which will contain
// an initialization function which will be invoked before DriverEntry() is
// called.
//
// It is the responsibilty of the component exporting the unique type to define
// and implement the function which will return the unique type.   The format of
// the define is:
//
// #define _contexttype ## _EXTERNAL_INIT_FUNCTION
//
// (e.g. #define DEVICE_CONTEXT_EXTERNALINIT_FUNCTION DeviceContextInit()
//  for a type of DEVICE_CONTEXT)
//
#define WDF_DECLARE_SHARED_CONTEXT_TYPE_WITH_NAME(_contexttype, _castingfunction) \
                                                                        \
WDF_DECLARE_TYPE_AND_GLOBALS(                                           \
    _contexttype,                                                       \
    NULL,                                                               \
    WDF_TYPE_NAME_TO_EXTERNAL_INIT_FUNCTION(_contexttype),              \
    WDF_TYPE_INIT_SECTION_NAME)                                         \
                                                                        \
WDF_DECLARE_CASTING_FUNCTION(_contexttype, _castingfunction)

//
// WDF_DECLARE_SHARED_CONTEXT_TYPE_WITH_NAME is the same as
// WDF_DECLARE_CONTEXT_TYPE_WITH_NAME with respect to the types and structures
// that are created and initialized.  The casting function is different in that
// it passes the UniqueType to WdfObjectGetTypedContextWorker() instead of the
// global type structure created.  It also creates a structure which will contain
// an initialization function which will be invoked before DriverEntry() is
// called.
//
// It is the responsibilty of the component exporting the unique type to define
// and implement the function which will return the unique type.   The format of
// the define is:
//
// #define _contexttype ## _EXTERNAL_INIT_FUNCTION
//
// (e.g. #define DEVICE_CONTEXT_EXTERNALINIT_FUNCTION DeviceContextInit()
//  for a type of DEVICE_CONTEXT)
//
#define WDF_DECLARE_SHARED_CONTEXT_TYPE_WITH_NAME(_contexttype, _castingfunction) \
                                                                        \
WDF_DECLARE_TYPE_AND_GLOBALS(                                           \
    _contexttype,                                                       \
    NULL,                                                               \
    WDF_TYPE_NAME_TO_EXTERNAL_INIT_FUNCTION(_contexttype),              \
    WDF_TYPE_INIT_SECTION_NAME)                                         \
                                                                        \
WDF_DECLARE_CASTING_FUNCTION(_contexttype, _castingfunction)

//
// Generic conversion macro from handle to type.  This should be used if the
// autogenerated conversion function does not suite the programmers calling style.
//
// The type parameter should be name of the type (e.g. DEVICE_CONTEXT), not the
// name of the pointer to the type (PDEVICE_CONTEXT).
//
// Example call:
//
// WDFDEVICE device;
// PDEVICE_CONTEXT pContext;
//
// pContext = WdfObjectGetTypedContext(device, DEVICE_CONTEXT);
//
//
#define WdfObjectGetTypedContext(handle, type)  \
(type*)                                         \
WdfObjectGetTypedContextWorker(                 \
    (WDFOBJECT) handle,                         \
    WDF_GET_CONTEXT_TYPE_INFO(type)->UniqueType \
    )

//
// WDF Function: WdfObjectGetTypedContextWorker
//
/* typedef */
/* WDFAPI */
/* PVOID */
/* (FASTCALL *PFN_WDFOBJECTGETTYPEDCONTEXTWORKER)( */
/*     __in */
/*     PWDF_DRIVER_GLOBALS DriverGlobals, */
/*     __in */
/*     WDFOBJECT Handle, */
/*     __in */
/*     PCWDF_OBJECT_CONTEXT_TYPE_INFO TypeInfo */
/*     ); */

PVOID /* FORCEINLINE */ WdfObjectGetTypedContextWorker(
/*     __in */
    WDFOBJECT Handle,
/*     __in */
    PCWDF_OBJECT_CONTEXT_TYPE_INFO TypeInfo
    )
{
  /* return ((PFN_WDFOBJECTGETTYPEDCONTEXTWORKER) WdfFunctions[WdfObjectGetTypedContextWorkerTableIndex])(WdfDriverGlobals, Handle, TypeInfo); */
  // Patch: provide an implementation of GetTypedContextWorker.
  PVOID ctxt = NULL;
  // Compare TypeInfo->ContextName to context->id to get right context.
  // ToDo: Should do some string->int hashing here instead.
  //  if (*(Handle->context0.id) == *(TypeInfo->ContextName)) ctxt = Handle->context0.data;
  return ctxt;
}


VOID
WdfObjectDelete(WDFOBJECT obj)
{
  // SI: Normally, we shouldn't be free-ing Device objects:
  // http://msdn.microsoft.com/en-us/library/windows/hardware/ff548734(v=vs.85).aspx
  if (obj == SL_Device_one) {
    return;
  }

  if (obj == SL_Device_two) {
    return;
  }

  if (NULL != obj->Context) { free(obj->Context); }
  free(obj);
  return;
}

/******************************************************************************
 * File: wdfsync.h
 ******************************************************************************/

VOID WdfSpinLockAcquire(WDFSPINLOCK l) {}
VOID WdfSpinLockRelease(WDFSPINLOCK l) {}

NTSTATUS WdfWaitLockCreate(PWDF_OBJECT_ATTRIBUTES LockAttibutes, WDFWAITLOCK *Lock)
{
  SLAyer_nondetT(NTSTATUS);
}

VOID WdfWaitLockAcquire(WDFWAITLOCK l, PLONGLONG Timeout) {}
VOID WdfWaitLockRelease(WDFWAITLOCK l) {}

/******************************************************************************
 * File: wdfdriver.h.
 ******************************************************************************/

// Copied from c:/slam/WDK/inc/wdf/kmdf/1.9/wdfdriver.h, line 45.
typedef
/* __ drv_functionClass(EVT_WDF_DRIVER_DEVICE_ADD) */
/* __drv_sameIRQL */
/* __drv_maxIRQL(PASSIVE_LEVEL) */
NTSTATUS
EVT_WDF_DRIVER_DEVICE_ADD(
/*     __in */
    WDFDRIVER Driver,
/*      __inout */
    PWDFDEVICE_INIT DeviceInit
    );
typedef EVT_WDF_DRIVER_DEVICE_ADD *PFN_WDF_DRIVER_DEVICE_ADD;

typedef
/* __drv_functionClass(EVT_WDF_DRIVER_UNLOAD) */
/* __drv_sameIRQL */
/* __drv_maxIRQL(PASSIVE_LEVEL) */
VOID
EVT_WDF_DRIVER_UNLOAD(
/*     __in */
    WDFDRIVER Driver
    );
typedef EVT_WDF_DRIVER_UNLOAD *PFN_WDF_DRIVER_UNLOAD;

// Copied from c:/slam/WDK/inc/wdf/kmdf/1.9/wdfdriver.h, line 97.
typedef struct _WDF_DRIVER_CONFIG {
  ULONG  Size;
  PFN_WDF_DRIVER_DEVICE_ADD  EvtDriverDeviceAdd;
  PFN_WDF_DRIVER_UNLOAD  EvtDriverUnload;
  ULONG  DriverInitFlags;
  ULONG  DriverPoolTag;
} WDF_DRIVER_CONFIG, *PWDF_DRIVER_CONFIG;

VOID
/* FORCEINLINE */
WDF_DRIVER_CONFIG_INIT(
    /* __out */ PWDF_DRIVER_CONFIG Config,
    /* __in_opt */ PFN_WDF_DRIVER_DEVICE_ADD EvtDriverDeviceAdd
    )
{
/* Patch: */
  /* RtlZeroMemory(Config, sizeof(WDF_DRIVER_CONFIG)); */
  Config->Size = 0;
  Config->EvtDriverDeviceAdd = 0;
  Config->EvtDriverUnload = 0;
  Config->DriverInitFlags = 0;
  Config->DriverPoolTag = 0;

  Config->Size = sizeof(WDF_DRIVER_CONFIG);
  Config->EvtDriverDeviceAdd = EvtDriverDeviceAdd;
}

// Copied from WDK/inc/wdf/kmdf/1.9/wdfdriver.h, line 201.
/* WDFAPI */
NTSTATUS
WdfDriverCreate(
    /* IN */           PDRIVER_OBJECT          DriverObject,
    /* IN */           PCUNICODE_STRING        RegistryPath,
    /* IN OPTIONAL */  PWDF_OBJECT_ATTRIBUTES  DriverAttributes,
    /* IN */           PWDF_DRIVER_CONFIG      DriverConfig,
    /* OUT OPTIONAL */ WDFDRIVER*              Driver
    )
{
  WDFDRIVER driver;
  driver = (WDFDRIVER)malloc(sizeof(SLAyer_WDFOBJECT));
  driver->Context =
    (DriverAttributes == WDF_NO_OBJECT_ATTRIBUTES) ? NULL :
    (*(DriverAttributes->MkContext))() ;
  SL_Driver = driver;
  if (WDF_NO_HANDLE != Driver) { *Driver = driver; }
  return STATUS_SUCCESS;
}

// line 255
typedef
//_IRQL_requires_max_(DISPATCH_LEVEL)
WDFAPI
PDRIVER_OBJECT
(*PFN_WDFDRIVERWDMGETDRIVEROBJECT)(
    //_In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
    //_In_
    WDFDRIVER Driver
    );

//_IRQL_requires_max_(DISPATCH_LEVEL)
PDRIVER_OBJECT
FORCEINLINE
WdfDriverWdmGetDriverObject(
    //_In_
    WDFDRIVER Driver
    )
{
  //return ((PFN_WDFDRIVERWDMGETDRIVEROBJECT) WdfFunctions[WdfDriverWdmGetDriverObjectTableIndex])(WdfDriverGlobals, Driver);
  return NULL;
}

 /******************************************************************************
 * File: wdfchildlist.h
 ******************************************************************************/
 //copied from line 32
 typedef enum _WDF_CHILD_LIST_RETRIEVE_DEVICE_STATUS {
    WdfChildListRetrieveDeviceUndefined = 0,
    WdfChildListRetrieveDeviceSuccess,
    WdfChildListRetrieveDeviceNotYetCreated,
    WdfChildListRetrieveDeviceNoSuchDevice,
} WDF_CHILD_LIST_RETRIEVE_DEVICE_STATUS, *PWDF_CHILD_LIST_RETRIEVE_DEVICE_STATUS;

 //copied from line 39

 typedef enum _WDF_RETRIEVE_CHILD_FLAGS {
    WdfRetrieveUnspecified = 0x0000,
    WdfRetrievePresentChildren = 0x0001,
    WdfRetrieveMissingChildren = 0x0002,
    WdfRetrievePendingChildren = 0x0004,
    WdfRetrieveAddedChildren = (WdfRetrievePresentChildren | WdfRetrievePendingChildren),
    WdfRetrieveAllChildren = (WdfRetrievePresentChildren | WdfRetrievePendingChildren | WdfRetrieveMissingChildren),
} WDF_RETRIEVE_CHILD_FLAGS;

 //copied from line 50
 typedef struct _WDF_CHILD_IDENTIFICATION_DESCRIPTION_HEADER {
    //
    // Size in bytes of the entire description, including this header.
    //
    // Same value as WDF_CHILD_LIST_CONFIG::IdentificationDescriptionSize
    // Used as a sanity check.
    //
    ULONG IdentificationDescriptionSize;
}   WDF_CHILD_IDENTIFICATION_DESCRIPTION_HEADER,
  *PWDF_CHILD_IDENTIFICATION_DESCRIPTION_HEADER;

  //copied from line 72

  typedef struct _WDF_CHILD_ADDRESS_DESCRIPTION_HEADER {
    //
    // Size in bytes of the entire description, including this header.
    //
    // Same value as WDF_CHILD_LIST_CONFIG::AddressDescriptionSize
    // Used as a sanity check.
    //
    ULONG AddressDescriptionSize;
}   WDF_CHILD_ADDRESS_DESCRIPTION_HEADER,
  *PWDF_CHILD_ADDRESS_DESCRIPTION_HEADER;

  //copied from line 94

typedef
NTSTATUS
(*PFN_WDF_CHILD_LIST_CREATE_DEVICE)(
    WDFCHILDLIST ChildList,
    PWDF_CHILD_IDENTIFICATION_DESCRIPTION_HEADER IdentificationDescription,
    PWDFDEVICE_INIT ChildInit
    );

typedef
VOID
(*PFN_WDF_CHILD_LIST_SCAN_FOR_CHILDREN)(
    WDFCHILDLIST ChildList
    );

typedef
VOID
(*PFN_WDF_CHILD_LIST_IDENTIFICATION_DESCRIPTION_COPY)(
    WDFCHILDLIST ChildList,
    PWDF_CHILD_IDENTIFICATION_DESCRIPTION_HEADER SourceIdentificationDescription,
    PWDF_CHILD_IDENTIFICATION_DESCRIPTION_HEADER DestinationIdentificationDescription
    );

typedef
NTSTATUS
(*PFN_WDF_CHILD_LIST_IDENTIFICATION_DESCRIPTION_DUPLICATE)(
    WDFCHILDLIST ChildList,
    PWDF_CHILD_IDENTIFICATION_DESCRIPTION_HEADER SourceIdentificationDescription,
    PWDF_CHILD_IDENTIFICATION_DESCRIPTION_HEADER DestinationIdentificationDescription
    );

typedef
BOOLEAN
(*PFN_WDF_CHILD_LIST_IDENTIFICATION_DESCRIPTION_COMPARE)(
    WDFCHILDLIST ChildList,
    PWDF_CHILD_IDENTIFICATION_DESCRIPTION_HEADER FirstIdentificationDescription,
    PWDF_CHILD_IDENTIFICATION_DESCRIPTION_HEADER SecondIdentificationDescription
    );

typedef
VOID
(*PFN_WDF_CHILD_LIST_IDENTIFICATION_DESCRIPTION_CLEANUP)(
    WDFCHILDLIST ChildList,
    PWDF_CHILD_IDENTIFICATION_DESCRIPTION_HEADER IdentificationDescription
    );

typedef
VOID
(*PFN_WDF_CHILD_LIST_ADDRESS_DESCRIPTION_COPY)(
    WDFCHILDLIST ChildList,
    PWDF_CHILD_ADDRESS_DESCRIPTION_HEADER SourceAddressDescription,
    PWDF_CHILD_ADDRESS_DESCRIPTION_HEADER DestinationAddressDescription
    );

typedef
NTSTATUS
(*PFN_WDF_CHILD_LIST_ADDRESS_DESCRIPTION_DUPLICATE)(
    WDFCHILDLIST ChildList,
    PWDF_CHILD_ADDRESS_DESCRIPTION_HEADER SourceAddressDescription,
    PWDF_CHILD_ADDRESS_DESCRIPTION_HEADER DestinationAddressDescription
    );

typedef
VOID
(*PFN_WDF_CHILD_LIST_ADDRESS_DESCRIPTION_CLEANUP)(
    WDFCHILDLIST ChildList,
    PWDF_CHILD_ADDRESS_DESCRIPTION_HEADER AddressDescription
    );

typedef
BOOLEAN
(*PFN_WDF_CHILD_LIST_DEVICE_REENUMERATED)(
    WDFCHILDLIST ChildList,
    WDFDEVICE OldDevice,
    PWDF_CHILD_ADDRESS_DESCRIPTION_HEADER OldAddressDescription,
    PWDF_CHILD_ADDRESS_DESCRIPTION_HEADER NewAddressDescription
    );
  //copied from wdf/kmdf/1.7/wdfchildlist.h, line 96

typedef
NTSTATUS
(EVT_WDF_CHILD_LIST_CREATE_DEVICE)(
    WDFCHILDLIST ChildList,
    PWDF_CHILD_IDENTIFICATION_DESCRIPTION_HEADER IdentificationDescription,
    PWDFDEVICE_INIT ChildInit
    );

typedef EVT_WDF_CHILD_LIST_CREATE_DEVICE *PFN_WDF_CHILD_LIST_CREATE_DEVICE;

typedef
NTSTATUS
(EVT_WDF_CHILD_LIST_IDENTIFICATION_DESCRIPTION_DUPLICATE)(
    WDFCHILDLIST ChildList,
    PWDF_CHILD_IDENTIFICATION_DESCRIPTION_HEADER SourceIdentificationDescription,
    PWDF_CHILD_IDENTIFICATION_DESCRIPTION_HEADER DestinationIdentificationDescription
    );

typedef EVT_WDF_CHILD_LIST_IDENTIFICATION_DESCRIPTION_DUPLICATE *PFN_WDF_CHILD_LIST_IDENTIFICATION_DESCRIPTION_DUPLICATE;

typedef
BOOLEAN
(EVT_WDF_CHILD_LIST_IDENTIFICATION_DESCRIPTION_COMPARE)(
    WDFCHILDLIST ChildList,
    PWDF_CHILD_IDENTIFICATION_DESCRIPTION_HEADER FirstIdentificationDescription,
    PWDF_CHILD_IDENTIFICATION_DESCRIPTION_HEADER SecondIdentificationDescription
    );

typedef EVT_WDF_CHILD_LIST_IDENTIFICATION_DESCRIPTION_COMPARE *PFN_WDF_CHILD_LIST_IDENTIFICATION_DESCRIPTION_COMPARE;

typedef
VOID
(EVT_WDF_CHILD_LIST_IDENTIFICATION_DESCRIPTION_CLEANUP)(
    WDFCHILDLIST ChildList,
    PWDF_CHILD_IDENTIFICATION_DESCRIPTION_HEADER IdentificationDescription
    );

typedef EVT_WDF_CHILD_LIST_IDENTIFICATION_DESCRIPTION_CLEANUP *PFN_WDF_CHILD_LIST_IDENTIFICATION_DESCRIPTION_CLEANUP;

typedef struct _WDF_CHILD_RETRIEVE_INFO {
  ULONG                                                 Size;
  PWDF_CHILD_IDENTIFICATION_DESCRIPTION_HEADER          IdentificationDescription;
  PWDF_CHILD_ADDRESS_DESCRIPTION_HEADER                 AddressDescription;
  WDF_CHILD_LIST_RETRIEVE_DEVICE_STATUS                 Status;
  PFN_WDF_CHILD_LIST_IDENTIFICATION_DESCRIPTION_COMPARE EvtChildListIdentificationDescriptionCompare;
} WDF_CHILD_RETRIEVE_INFO, *PWDF_CHILD_RETRIEVE_INFO;

typedef struct _WDF_CHILD_LIST_CONFIG {
  ULONG                                                   Size;
  ULONG                                                   IdentificationDescriptionSize;
  ULONG                                                   AddressDescriptionSize;
  PFN_WDF_CHILD_LIST_CREATE_DEVICE                        EvtChildListCreateDevice;
  PFN_WDF_CHILD_LIST_SCAN_FOR_CHILDREN                    EvtChildListScanForChildren;
  PFN_WDF_CHILD_LIST_IDENTIFICATION_DESCRIPTION_COPY      EvtChildListIdentificationDescriptionCopy;
  PFN_WDF_CHILD_LIST_IDENTIFICATION_DESCRIPTION_DUPLICATE EvtChildListIdentificationDescriptionDuplicate;
  PFN_WDF_CHILD_LIST_IDENTIFICATION_DESCRIPTION_CLEANUP   EvtChildListIdentificationDescriptionCleanup;
  PFN_WDF_CHILD_LIST_IDENTIFICATION_DESCRIPTION_COMPARE   EvtChildListIdentificationDescriptionCompare;
  PFN_WDF_CHILD_LIST_ADDRESS_DESCRIPTION_COPY             EvtChildListAddressDescriptionCopy;
  PFN_WDF_CHILD_LIST_ADDRESS_DESCRIPTION_DUPLICATE        EvtChildListAddressDescriptionDuplicate;
  PFN_WDF_CHILD_LIST_ADDRESS_DESCRIPTION_CLEANUP          EvtChildListAddressDescriptionCleanup;
  PFN_WDF_CHILD_LIST_DEVICE_REENUMERATED                  EvtChildListDeviceReenumerated;
} WDF_CHILD_LIST_CONFIG, *PWDF_CHILD_LIST_CONFIG;

//copied from line 321

typedef struct _WDF_CHILD_LIST_ITERATOR {
  ULONG Size;
  ULONG Flags;
  PVOID Reserved[4];
} WDF_CHILD_LIST_ITERATOR, *PWDF_CHILD_LIST_ITERATOR;

// stubs
VOID WDF_CHILD_LIST_CONFIG_INIT(PWDF_CHILD_LIST_CONFIG Config,
                                ULONG IdDescSize,
                                PFN_WDF_CHILD_LIST_CREATE_DEVICE EvtChildListCreateDevice)
{
}

VOID WDF_CHILD_LIST_ITERATOR_INIT(
  _Out_  PWDF_CHILD_LIST_ITERATOR Iterator,
  _In_   ULONG Flags
)
{
}

NTSTATUS
WdfChildListAddOrUpdateChildDescriptionAsPresent(WDFDEVICE Device,
                                                 PWDF_CHILD_IDENTIFICATION_DESCRIPTION_HEADER          IdentificationDescription,
                                                 PWDF_CHILD_ADDRESS_DESCRIPTION_HEADER                 AddressDescription)
{
  SLAyer_nondetT(NTSTATUS);
}

// PS #640 Consider generalizing return values
NTSTATUS WdfChildListUpdateChildDescriptionAsMissing(
  /*[in]*/  WDFCHILDLIST ChildList,
  /*[in]*/  PWDF_CHILD_IDENTIFICATION_DESCRIPTION_HEADER IdentificationDescription
)
{
  SLAyer_nondetT(NTSTATUS);
}


NTSTATUS
WdfChildListAddOrUpdateChildDescriptionAsMissing(WDFDEVICE Device,
                                                 PWDF_CHILD_IDENTIFICATION_DESCRIPTION_HEADER          IdentificationDescription,
                                                 PWDF_CHILD_ADDRESS_DESCRIPTION_HEADER                 AddressDescription)
{
  SLAyer_nondetT(NTSTATUS);
}

VOID WdfChildListBeginScan(WDFCHILDLIST List) {}
VOID WdfChildListEndScan(WDFCHILDLIST List) {}

VOID WdfChildListBeginIteration(
  /*[in]*/  WDFCHILDLIST ChildList,
  /*[in]*/  PWDF_CHILD_LIST_ITERATOR Iterator
)
{
}

VOID WdfChildListEndIteration(
  /*[in]*/  WDFCHILDLIST ChildList,
  /*[in]*/  PWDF_CHILD_LIST_ITERATOR Iterator
)
{
}

VOID
WDF_CHILD_IDENTIFICATION_DESCRIPTION_HEADER_INIT(
 PWDF_CHILD_IDENTIFICATION_DESCRIPTION_HEADER IdentificationDescription,
 ULONG IdDescSize)
{
}

BOOLEAN WdfChildListRequestChildEject(
  /*[in]*/  WDFCHILDLIST ChildList,
  /*[in]*/  PWDF_CHILD_IDENTIFICATION_DESCRIPTION_HEADER IdentificationDescription
)
{
  SLAyer_nondetT(BOOLEAN);
}

VOID WDF_CHILD_RETRIEVE_INFO_INIT(
  _Out_  PWDF_CHILD_RETRIEVE_INFO Info,
  _In_   PWDF_CHILD_IDENTIFICATION_DESCRIPTION_HEADER IdentificationDescription
)
{
}

NTSTATUS WdfChildListRetrieveNextDevice(
  /*[in]*/       WDFCHILDLIST ChildList,
  /*[in]*/       PWDF_CHILD_LIST_ITERATOR Iterator,
  /*[out]*/      WDFDEVICE *Device,
  /*[in, out]*/  PWDF_CHILD_RETRIEVE_INFO Info
)
{
  SLAyer_nondetT(NTSTATUS);
}

WDFDEVICE WdfChildListGetDevice(
/*   [in] */  WDFCHILDLIST ChildList
)
{
  return SL_Device_two;
}


/******************************************************************************
 * File: wdfdevice.h.
 ****************************************************************************/

// wdf/kmdf/1.9/wdfdevice.h
// line 389
typedef enum _WDF_FILEOBJECT_CLASS {
    WdfFileObjectInvalid = 0,
    WdfFileObjectNotRequired = 1,
    WdfFileObjectWdfCanUseFsContext = 2,
    WdfFileObjectWdfCanUseFsContext2 = 3,
    WdfFileObjectWdfCannotUseFsContexts = 4,
    WdfFileObjectCanBeOptional = 0x80000000,
} WDF_FILEOBJECT_CLASS, *PWDF_FILEOBJECT_CLASS;

// Copied from /cygdrive/c/Program Files (x86)/Windows Kits/8.0/Include/wdf/kmdf/1.9/wdfdevice.h
// line 357
typedef enum _WDF_POWER_POLICY_SX_WAKE_USER_CONTROL {
    WakeUserControlInvalid = 0,
    WakeDoNotAllowUserControl,
    WakeAllowUserControl,
} WDF_POWER_POLICY_SX_WAKE_USER_CONTROL;

// line 419
typedef enum _WDF_DEVICE_IO_TYPE {
    WdfDeviceIoUndefined = 0,
    WdfDeviceIoNeither,
    WdfDeviceIoBuffered,
    WdfDeviceIoDirect,
} WDF_DEVICE_IO_TYPE, *PWDF_DEVICE_IO_TYPE;

// line 435
typedef enum _WDF_DEVICE_FAILED_ACTION {
    WdfDeviceFailedUndefined = 0,
    WdfDeviceFailedAttemptRestart,
    WdfDeviceFailedNoRestart,
} WDF_DEVICE_FAILED_ACTION;

// wdf/kmdf/1.9/wdfdevice.h
// line 465
typedef
VOID
EVT_WDF_DEVICE_FILE_CREATE(
    _In_
    WDFDEVICE Device,
    _In_
    WDFREQUEST Request,
    _In_
    WDFFILEOBJECT FileObject
    );

typedef EVT_WDF_DEVICE_FILE_CREATE *PFN_WDF_DEVICE_FILE_CREATE;

typedef
VOID
EVT_WDF_FILE_CLOSE(
    _In_
    WDFFILEOBJECT FileObject
    );

typedef EVT_WDF_FILE_CLOSE *PFN_WDF_FILE_CLOSE;

typedef
VOID
EVT_WDF_FILE_CLEANUP(
    _In_
    WDFFILEOBJECT FileObject
    );

typedef EVT_WDF_FILE_CLEANUP *PFN_WDF_FILE_CLEANUP;

// wdf/kmdf/1.9/wdfdevice.h
// line 465
typedef struct _WDF_FILEOBJECT_CONFIG {
    ULONG Size;
    PFN_WDF_DEVICE_FILE_CREATE  EvtDeviceFileCreate;
    PFN_WDF_FILE_CLOSE   EvtFileClose;
    PFN_WDF_FILE_CLEANUP EvtFileCleanup;
    WDF_TRI_STATE AutoForwardCleanupClose;
    WDF_FILEOBJECT_CLASS FileObjectClass;

} WDF_FILEOBJECT_CONFIG, *PWDF_FILEOBJECT_CONFIG;

// wdf/kmdf/1.9/wdfdevice.h
// line 340
// Copied from /cygdrive/c/Program Files (x86)/Windows Kits/8.0/Include/wdf/kmdf/1.9/wdfdevice.h
// line 1164
typedef struct _WDF_DEVICE_POWER_POLICY_WAKE_SETTINGS {
    ULONG Size;
    DEVICE_POWER_STATE DxState;
    WDF_POWER_POLICY_SX_WAKE_USER_CONTROL UserControlOfWakeSettings;
    WDF_TRI_STATE Enabled;
    BOOLEAN ArmForWakeIfChildrenAreArmedForWake;
    BOOLEAN IndicateChildWakeOnParentWake;
} WDF_DEVICE_POWER_POLICY_WAKE_SETTINGS, *PWDF_DEVICE_POWER_POLICY_WAKE_SETTINGS;

// Copied from c:/Program Files (x86)/Windows Kits/8.0/Include/wdf/kmdf/1.11/wdfdevice.h
// line 344

typedef enum _WDF_POWER_POLICY_S0_IDLE_CAPABILITIES {
    IdleCapsInvalid = 0,
    IdleCannotWakeFromS0,
    IdleCanWakeFromS0,
    IdleUsbSelectiveSuspend,
} WDF_POWER_POLICY_S0_IDLE_CAPABILITIES;

// wdf/kmdf/1.9/wdfdevice.h
// line 347
typedef enum _WDF_POWER_POLICY_S0_IDLE_USER_CONTROL {
    IdleUserControlInvalid = 0,
    IdleDoNotAllowUserControl,
    IdleAllowUserControl,
} WDF_POWER_POLICY_S0_IDLE_USER_CONTROL;

typedef enum _WDF_POWER_DEVICE_STATE {
    WdfPowerDeviceInvalid = 0,
    WdfPowerDeviceD0,
    WdfPowerDeviceD1,
    WdfPowerDeviceD2,
    WdfPowerDeviceD3,
    WdfPowerDeviceD3Final,
    WdfPowerDevicePrepareForHibernation,
    WdfPowerDeviceMaximum,
} WDF_POWER_DEVICE_STATE, *PWDF_POWER_DEVICE_STATE;

// line 551
VOID
FORCEINLINE
WDF_FILEOBJECT_CONFIG_INIT(
    /*_Out_   */ PWDF_FILEOBJECT_CONFIG FileEventCallbacks,
    /*_In_opt_*/ PFN_WDF_DEVICE_FILE_CREATE EvtDeviceFileCreate,
    /*_In_opt_*/ PFN_WDF_FILE_CLOSE EvtFileClose,
    /*_In_opt_*/ PFN_WDF_FILE_CLEANUP EvtFileCleanup
    )
{
    FileEventCallbacks->Size = sizeof(WDF_FILEOBJECT_CONFIG);

    FileEventCallbacks->EvtDeviceFileCreate  = EvtDeviceFileCreate;
    FileEventCallbacks->EvtFileClose   = EvtFileClose;
    FileEventCallbacks->EvtFileCleanup = EvtFileCleanup;

    FileEventCallbacks->FileObjectClass = WdfFileObjectWdfCannotUseFsContexts;
    FileEventCallbacks->AutoForwardCleanupClose = WdfUseDefault;
}

typedef enum _WDF_POWER_POLICY_IDLE_TIMEOUT_TYPE {
    DriverManagedIdleTimeout = 0,
    // SystemManagedIdleTimeout = 1,
    // SystemManagedIdleTimeoutWithHint = 2
} WDF_POWER_POLICY_IDLE_TIMEOUT_TYPE, *PWDF_POWER_POLICY_IDLE_TIMEOUT_TYPE;

typedef enum _WDF_POWER_POLICY_IDLE_TIMEOUT_CONSTANTS {
    IdleTimeoutDefaultConstant = 0,
} WDF_POWER_POLICY_IDLE_TIMEOUT_CONSTANTS;
#define IdleTimeoutDefaultValue ((ULONG) IdleTimeoutDefaultConstant)

// line 752
typedef
// _Function_class_(EVT_WDF_DEVICE_D0_ENTRY_POST_INTERRUPTS_ENABLED)
// _IRQL_requires_same_
// _IRQL_requires_max_(PASSIVE_LEVEL)
NTSTATUS
EVT_WDF_DEVICE_D0_ENTRY_POST_INTERRUPTS_ENABLED(
    // _In_
    WDFDEVICE Device,
    // _In_
    WDF_POWER_DEVICE_STATE PreviousState
    );

typedef EVT_WDF_DEVICE_D0_ENTRY_POST_INTERRUPTS_ENABLED *PFN_WDF_DEVICE_D0_ENTRY_POST_INTERRUPTS_ENABLED;

typedef
// _Function_class_(EVT_WDF_DEVICE_D0_EXIT_PRE_INTERRUPTS_DISABLED)
// _IRQL_requires_same_
// _IRQL_requires_max_(PASSIVE_LEVEL)
NTSTATUS
EVT_WDF_DEVICE_D0_EXIT_PRE_INTERRUPTS_DISABLED(
    // _In_
    WDFDEVICE Device,
    // _In_
    WDF_POWER_DEVICE_STATE TargetState
    );

// line 866
typedef
// _Function_class_(EVT_WDF_DEVICE_SELF_MANAGED_IO_SUSPEND)
// _IRQL_requires_same_
// _IRQL_requires_max_(PASSIVE_LEVEL)
NTSTATUS
EVT_WDF_DEVICE_SELF_MANAGED_IO_SUSPEND(
    // _In_
    WDFDEVICE Device
    );

typedef EVT_WDF_DEVICE_SELF_MANAGED_IO_SUSPEND *PFN_WDF_DEVICE_SELF_MANAGED_IO_SUSPEND;

typedef
// _Function_class_(EVT_WDF_DEVICE_SELF_MANAGED_IO_RESTART)
// _IRQL_requires_same_
// _IRQL_requires_max_(PASSIVE_LEVEL)
NTSTATUS
EVT_WDF_DEVICE_SELF_MANAGED_IO_RESTART(
    // _In_
    WDFDEVICE Device
    );

typedef EVT_WDF_DEVICE_SELF_MANAGED_IO_RESTART *PFN_WDF_DEVICE_SELF_MANAGED_IO_RESTART;

// Copied from c:/Program Files (x86)/Windows Kits/8.0/Include/wdf/kmdf/1.11/wdfdevice.h
// line 1083
typedef struct _WDF_DEVICE_POWER_POLICY_IDLE_SETTINGS {
    ULONG Size;
    WDF_POWER_POLICY_S0_IDLE_CAPABILITIES IdleCaps;
    DEVICE_POWER_STATE DxState;
    ULONG IdleTimeout;
    WDF_POWER_POLICY_S0_IDLE_USER_CONTROL UserControlOfIdleSettings;
    WDF_TRI_STATE Enabled;
    WDF_TRI_STATE PowerUpIdleDeviceOnSystemWake;
    WDF_POWER_POLICY_IDLE_TIMEOUT_TYPE IdleTimeoutType;
    WDF_TRI_STATE ExcludeD3Cold;
} WDF_DEVICE_POWER_POLICY_IDLE_SETTINGS, *PWDF_DEVICE_POWER_POLICY_IDLE_SETTINGS;

VOID
FORCEINLINE
WDF_DEVICE_POWER_POLICY_IDLE_SETTINGS_INIT(
    /*_Out_*/ PWDF_DEVICE_POWER_POLICY_IDLE_SETTINGS Settings,
    /*_In_ */ WDF_POWER_POLICY_S0_IDLE_CAPABILITIES IdleCaps
    )
{
    RtlZeroMemory(Settings, sizeof(WDF_DEVICE_POWER_POLICY_IDLE_SETTINGS));

    Settings->Size = sizeof(WDF_DEVICE_POWER_POLICY_IDLE_SETTINGS);

    Settings->IdleTimeout = IdleTimeoutDefaultValue;
    Settings->UserControlOfIdleSettings = IdleAllowUserControl;
    Settings->Enabled = WdfUseDefault;
    Settings->PowerUpIdleDeviceOnSystemWake = WdfUseDefault;
    Settings->IdleTimeoutType = DriverManagedIdleTimeout;
    Settings->ExcludeD3Cold = WdfUseDefault;

    Settings->IdleCaps = IdleCaps;

    switch (IdleCaps) {
    case IdleUsbSelectiveSuspend:
    case IdleCanWakeFromS0:
        Settings->DxState = PowerDeviceMaximum;
        break;

    case IdleCannotWakeFromS0:
        Settings->DxState = PowerDeviceD3;
        break;
    }
}

// Copied from c:/slam/WDK/inc/wdf/kmdf/1.9/wdfdevice.h, line 907.
typedef
/* __drv_functionClass(EVT_WDF_DEVICE_ARM_WAKE_FROM_S0) */
/* __drv_sameIRQL */
/* __drv_maxIRQL(PASSIVE_LEVEL) */
NTSTATUS
EVT_WDF_DEVICE_ARM_WAKE_FROM_S0(
/*     __in */
    WDFDEVICE Device
    );

typedef EVT_WDF_DEVICE_ARM_WAKE_FROM_S0 *PFN_WDF_DEVICE_ARM_WAKE_FROM_S0;

typedef
/* __drv_functionClass(EVT_WDF_DEVICE_ARM_WAKE_FROM_SX) */
/* __drv_sameIRQL */
/* __drv_maxIRQL(PASSIVE_LEVEL) */
NTSTATUS
EVT_WDF_DEVICE_ARM_WAKE_FROM_SX(
/*     __in */
    WDFDEVICE Device
    );

typedef EVT_WDF_DEVICE_ARM_WAKE_FROM_SX *PFN_WDF_DEVICE_ARM_WAKE_FROM_SX;

typedef
/* __drv_functionClass(EVT_WDF_DEVICE_ARM_WAKE_FROM_SX_WITH_REASON) */
/* __drv_sameIRQL */
/* __drv_maxIRQL(PASSIVE_LEVEL) */
NTSTATUS
EVT_WDF_DEVICE_ARM_WAKE_FROM_SX_WITH_REASON(
/*     __in */
    WDFDEVICE Device,
/*     __in */
    BOOLEAN DeviceWakeEnabled,
/*     __in */
    BOOLEAN ChildrenArmedForWake
    );

typedef EVT_WDF_DEVICE_ARM_WAKE_FROM_SX_WITH_REASON *PFN_WDF_DEVICE_ARM_WAKE_FROM_SX_WITH_REASON;

typedef
/* __drv_functionClass(EVT_WDF_DEVICE_DISARM_WAKE_FROM_S0) */
/* __drv_sameIRQL */
/* __drv_maxIRQL(PASSIVE_LEVEL) */
VOID
EVT_WDF_DEVICE_DISARM_WAKE_FROM_S0(
/*     __in */
    WDFDEVICE Device
    );

typedef EVT_WDF_DEVICE_DISARM_WAKE_FROM_S0 *PFN_WDF_DEVICE_DISARM_WAKE_FROM_S0;

typedef
/* __drv_functionClass(EVT_WDF_DEVICE_DISARM_WAKE_FROM_SX) */
/* __drv_sameIRQL */
/* __drv_maxIRQL(PASSIVE_LEVEL) */
VOID
EVT_WDF_DEVICE_DISARM_WAKE_FROM_SX(
/*     __in */
    WDFDEVICE Device
    );

typedef EVT_WDF_DEVICE_DISARM_WAKE_FROM_SX *PFN_WDF_DEVICE_DISARM_WAKE_FROM_SX;

typedef
/* __drv_functionClass(EVT_WDF_DEVICE_WAKE_FROM_S0_TRIGGERED) */
/* __drv_sameIRQL */
/* __drv_maxIRQL(PASSIVE_LEVEL) */
VOID
EVT_WDF_DEVICE_WAKE_FROM_S0_TRIGGERED(
/*     __in */
    WDFDEVICE Device
    );

typedef EVT_WDF_DEVICE_WAKE_FROM_S0_TRIGGERED *PFN_WDF_DEVICE_WAKE_FROM_S0_TRIGGERED;

typedef
/* __drv_functionClass(EVT_WDF_DEVICE_WAKE_FROM_SX_TRIGGERED) */
/* __drv_sameIRQL */
/* __drv_maxIRQL(PASSIVE_LEVEL) */
VOID
EVT_WDF_DEVICE_WAKE_FROM_SX_TRIGGERED(
/*     __in */
    WDFDEVICE Device
    );

typedef EVT_WDF_DEVICE_WAKE_FROM_SX_TRIGGERED *PFN_WDF_DEVICE_WAKE_FROM_SX_TRIGGERED;

// Copied from $SLAM/WDK/inc/wdf/kmdf/1.9/wdfdevice.h, line 695.
typedef
/* __drv_functionClass(EVT_WDF_DEVICE_D0_ENTRY) */
/* __drv_sameIRQL */
/* __drv_maxIRQL(PASSIVE_LEVEL) */
NTSTATUS
EVT_WDF_DEVICE_D0_ENTRY(
/*     __in */
    WDFDEVICE Device,
/*     __in */
    WDF_POWER_DEVICE_STATE PreviousState
    );
typedef EVT_WDF_DEVICE_D0_ENTRY *PFN_WDF_DEVICE_D0_ENTRY;

// Copied from $SLAM/WDK/inc/wdf/kmdf/1.9/, line 723.
typedef
/* __drv_functionClass(EVT_WDF_DEVICE_D0_EXIT) */
/* __drv_sameIRQL */
/* __drv_maxIRQL(PASSIVE_LEVEL) */
NTSTATUS
EVT_WDF_DEVICE_D0_EXIT(
/*     __in */
    WDFDEVICE Device,
/*     __in */
    WDF_POWER_DEVICE_STATE TargetState
    );
typedef EVT_WDF_DEVICE_D0_EXIT *PFN_WDF_DEVICE_D0_EXIT;

// Copied from $SLAM/WDK/inc/wdf/kmdf/1.9/, line 751.
typedef
/* __drv_functionClass(EVT_WDF_DEVICE_PREPARE_HARDWARE) */
/* __drv_sameIRQL */
/* __drv_maxIRQL(PASSIVE_LEVEL) */
NTSTATUS
EVT_WDF_DEVICE_PREPARE_HARDWARE(
/*     __in */
    WDFDEVICE Device,
/*     __in */
    WDFCMRESLIST ResourcesRaw,
/*     __in */
    WDFCMRESLIST ResourcesTranslated
    );

typedef EVT_WDF_DEVICE_PREPARE_HARDWARE *PFN_WDF_DEVICE_PREPARE_HARDWARE;

typedef
/* __drv_functionClass(EVT_WDF_DEVICE_RELEASE_HARDWARE) */
/* __drv_sameIRQL */
/* __drv_maxIRQL(PASSIVE_LEVEL) */
NTSTATUS
EVT_WDF_DEVICE_RELEASE_HARDWARE(
/*     __in */
    WDFDEVICE Device,
/*     __in */
    WDFCMRESLIST ResourcesTranslated
 );
typedef EVT_WDF_DEVICE_RELEASE_HARDWARE *PFN_WDF_DEVICE_RELEASE_HARDWARE;

typedef
/* __drv_functionClass(EVT_WDF_DEVICE_SELF_MANAGED_IO_CLEANUP) */
/* __drv_sameIRQL */
/* __drv_maxIRQL(PASSIVE_LEVEL) */
VOID
EVT_WDF_DEVICE_SELF_MANAGED_IO_CLEANUP(
/*     __in */
    WDFDEVICE Device
    );

typedef EVT_WDF_DEVICE_SELF_MANAGED_IO_CLEANUP *PFN_WDF_DEVICE_SELF_MANAGED_IO_CLEANUP;

typedef
NTSTATUS
EVT_WDF_DEVICE_SELF_MANAGED_IO_INIT(
    _In_
    WDFDEVICE Device
    );

typedef EVT_WDF_DEVICE_SELF_MANAGED_IO_INIT *PFN_WDF_DEVICE_SELF_MANAGED_IO_INIT;

typedef
VOID
EVT_WDF_DEVICE_FILE_CREATE(
    _In_
    WDFDEVICE Device,
    _In_
    WDFREQUEST Request,
    _In_
    WDFFILEOBJECT FileObject
    );

typedef EVT_WDF_DEVICE_FILE_CREATE *PFN_WDF_DEVICE_FILE_CREATE;

typedef
VOID
EVT_WDF_FILE_CLOSE(
    _In_
    WDFFILEOBJECT FileObject
    );

typedef EVT_WDF_FILE_CLOSE *PFN_WDF_FILE_CLOSE;

typedef
// _Function_class_(EVT_WDF_DEVICE_D0_EXIT_PRE_INTERRUPTS_DISABLED)
// _IRQL_requires_same_
// _IRQL_requires_max_(PASSIVE_LEVEL)
NTSTATUS
EVT_WDF_DEVICE_D0_EXIT_PRE_INTERRUPTS_DISABLED(
    // _In_
    WDFDEVICE Device,
    // _In_
    WDF_POWER_DEVICE_STATE TargetState
    );

typedef EVT_WDF_DEVICE_D0_EXIT_PRE_INTERRUPTS_DISABLED *PFN_WDF_DEVICE_D0_EXIT_PRE_INTERRUPTS_DISABLED;

// Copied from $SLAM/WDK/inc/wdf/kmdf/1.9/wdfdevice.h, line 995.
typedef struct _WDF_PNPPOWER_EVENT_CALLBACKS {
    ULONG Size;
    PFN_WDF_DEVICE_D0_ENTRY                 EvtDeviceD0Entry;
 PFN_WDF_DEVICE_D0_ENTRY_POST_INTERRUPTS_ENABLED EvtDeviceD0EntryPostInterruptsEnabled;
    PFN_WDF_DEVICE_D0_EXIT                  EvtDeviceD0Exit;
  PFN_WDF_DEVICE_D0_EXIT_PRE_INTERRUPTS_DISABLED EvtDeviceD0ExitPreInterruptsDisabled;
    PFN_WDF_DEVICE_PREPARE_HARDWARE         EvtDevicePrepareHardware;
    PFN_WDF_DEVICE_RELEASE_HARDWARE         EvtDeviceReleaseHardware;
    PFN_WDF_DEVICE_SELF_MANAGED_IO_CLEANUP  EvtDeviceSelfManagedIoCleanup;
/*     PFN_WDF_DEVICE_SELF_MANAGED_IO_FLUSH    EvtDeviceSelfManagedIoFlush; */
    PFN_WDF_DEVICE_SELF_MANAGED_IO_INIT     EvtDeviceSelfManagedIoInit;
     PFN_WDF_DEVICE_SELF_MANAGED_IO_SUSPEND  EvtDeviceSelfManagedIoSuspend;
     PFN_WDF_DEVICE_SELF_MANAGED_IO_RESTART  EvtDeviceSelfManagedIoRestart;
/*     PFN_WDF_DEVICE_SURPRISE_REMOVAL         EvtDeviceSurpriseRemoval; */
/*     PFN_WDF_DEVICE_QUERY_REMOVE             EvtDeviceQueryRemove; */
/*     PFN_WDF_DEVICE_QUERY_STOP               EvtDeviceQueryStop; */
/*     PFN_WDF_DEVICE_USAGE_NOTIFICATION       EvtDeviceUsageNotification; */
/*     PFN_WDF_DEVICE_RELATIONS_QUERY          EvtDeviceRelationsQuery; */
} WDF_PNPPOWER_EVENT_CALLBACKS, *PWDF_PNPPOWER_EVENT_CALLBACKS;

typedef EVT_WDF_DEVICE_WAKE_FROM_SX_TRIGGERED *PFN_WDF_DEVICE_WAKE_FROM_SX_TRIGGERED;

typedef struct _WDF_POWER_POLICY_EVENT_CALLBACKS {
    ULONG Size;
    PFN_WDF_DEVICE_ARM_WAKE_FROM_S0         EvtDeviceArmWakeFromS0;
    PFN_WDF_DEVICE_DISARM_WAKE_FROM_S0      EvtDeviceDisarmWakeFromS0;
    PFN_WDF_DEVICE_WAKE_FROM_S0_TRIGGERED   EvtDeviceWakeFromS0Triggered;
    PFN_WDF_DEVICE_ARM_WAKE_FROM_SX         EvtDeviceArmWakeFromSx;
    PFN_WDF_DEVICE_DISARM_WAKE_FROM_SX      EvtDeviceDisarmWakeFromSx;
    PFN_WDF_DEVICE_WAKE_FROM_SX_TRIGGERED   EvtDeviceWakeFromSxTriggered;
    PFN_WDF_DEVICE_ARM_WAKE_FROM_SX_WITH_REASON EvtDeviceArmWakeFromSxWithReason;
} WDF_POWER_POLICY_EVENT_CALLBACKS, *PWDF_POWER_POLICY_EVENT_CALLBACKS;

typedef
NTSTATUS
EVT_WDF_DEVICE_SELF_MANAGED_IO_INIT(
    _In_
    WDFDEVICE Device
    );

VOID
/* FORCEINLINE */
WDF_POWER_POLICY_EVENT_CALLBACKS_INIT(
    /* __out */ PWDF_POWER_POLICY_EVENT_CALLBACKS Callbacks
    )
{
  // Patch:
  /* RtlZeroMemory(Callbacks, sizeof(WDF_POWER_POLICY_EVENT_CALLBACKS)); */
  Callbacks->Size = 0;
  Callbacks->EvtDeviceArmWakeFromS0 = 0;
  Callbacks->EvtDeviceDisarmWakeFromS0 = 0;
  Callbacks->EvtDeviceWakeFromS0Triggered = 0;
  Callbacks->EvtDeviceArmWakeFromSx = 0;
  Callbacks->EvtDeviceDisarmWakeFromSx = 0;
  Callbacks->EvtDeviceWakeFromSxTriggered = 0;
  Callbacks->EvtDeviceArmWakeFromSxWithReason = 0;

  Callbacks->Size = sizeof(WDF_POWER_POLICY_EVENT_CALLBACKS);
}

// line 1818
typedef
// _IRQL_requires_max_(DISPATCH_LEVEL)
WDFAPI
PDEVICE_OBJECT
(*PFN_WDFDEVICEWDMGETDEVICEOBJECT)(
    // _In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
    // _In_
    WDFDEVICE Device
    );

// _IRQL_requires_max_(DISPATCH_LEVEL)
PDEVICE_OBJECT
FORCEINLINE
WdfDeviceWdmGetDeviceObject(
    // _In_
    WDFDEVICE Device
    )
{
    // return ((PFN_WDFDEVICEWDMGETDEVICEOBJECT) WdfFunctions[WdfDeviceWdmGetDeviceObjectTableIndex])(WdfDriverGlobals, Device);
    PDEVICE_OBJECT o;
    return o;
}

VOID
/* FORCEINLINE */
WDF_PNPPOWER_EVENT_CALLBACKS_INIT(
    /* __out */ PWDF_PNPPOWER_EVENT_CALLBACKS Callbacks
    )
{
  // Patch:
  /* RtlZeroMemory(Callbacks, sizeof(WDF_PNPPOWER_EVENT_CALLBACKS)); */
  Callbacks->Size = 0;
  Callbacks->EvtDeviceD0Entry = 0;
  Callbacks->EvtDeviceD0Exit = 0;
  Callbacks->EvtDevicePrepareHardware = 0;
  Callbacks->EvtDeviceReleaseHardware = 0;
  Callbacks->EvtDeviceSelfManagedIoCleanup = 0;

  Callbacks->Size = sizeof(WDF_PNPPOWER_EVENT_CALLBACKS);
}

//copied from wdf/kmdf/1.7/wdfdevice.h, line 1195

typedef struct _WDF_DEVICE_POWER_CAPABILITIES {
    //
    // Size of the structure in bytes
    //
    ULONG Size;

    WDF_TRI_STATE DeviceD1;
    WDF_TRI_STATE DeviceD2;

    WDF_TRI_STATE WakeFromD0;
    WDF_TRI_STATE WakeFromD1;
    WDF_TRI_STATE WakeFromD2;
    WDF_TRI_STATE WakeFromD3;

    //
    // Default value PowerDeviceMaximum indicates not to set this value
    //
    DEVICE_POWER_STATE DeviceState[PowerSystemMaximum];

    //
    // Default value PowerDeviceMaximum, PowerSystemMaximum indicates not to
    // set this value.
    //
    DEVICE_POWER_STATE DeviceWake;
    SYSTEM_POWER_STATE SystemWake;

    //
    // Default values of -1 indicate not to set this value
    //
    ULONG D1Latency;
    ULONG D2Latency;
    ULONG D3Latency;

    //
    // Ideal Dx state for the device to be put into when the machine moves into
    // Sx and the device is not armed for wake.  By default, the default will be
    // placed into D3.  If IdealDxStateForSx is lighter then
    // DeviceState[Sx], then DeviceState[Sx] will be used as the Dx state.
    //
    DEVICE_POWER_STATE IdealDxStateForSx;

} WDF_DEVICE_POWER_CAPABILITIES, *PWDF_DEVICE_POWER_CAPABILITIES;

// wdf/kmdf/1.9/wdfdevice.h
// line 1229
typedef struct _WDF_DEVICE_STATE {
    ULONG Size;
    WDF_TRI_STATE Disabled;
    WDF_TRI_STATE DontDisplayInUI;
    WDF_TRI_STATE Failed;
    WDF_TRI_STATE NotDisableable;
    WDF_TRI_STATE Removed;
    WDF_TRI_STATE ResourcesChanged;
} WDF_DEVICE_STATE, *PWDF_DEVICE_STATE;

// Copied from c:/slam/WDK/inc/wdf/kmdf/1.9/wdfdevice.h, line 2232.
//
// WDF Function: WdfDeviceInitSetPnpPowerEventCallbacks
//
typedef
/* __drv_maxIRQL(DISPATCH_LEVEL) */
/* WDFAPI */
VOID
(*PFN_WDFDEVICEINITSETPNPPOWEREVENTCALLBACKS)(
/*     __in */
    PWDF_DRIVER_GLOBALS DriverGlobals,
/*     __in */
    PWDFDEVICE_INIT DeviceInit,
/*     __in */
    PWDF_PNPPOWER_EVENT_CALLBACKS PnpPowerEventCallbacks
    );

// Patch: model of WDFDEVICE_INIT. Not in original wdfdevice.h file.
// WDFDEVICE_INIT is not defined in WDK/inc/wdf/kmdf/1.9/*.h.
/* typedef struct _WDFDEVICE_INIT { */
/*   WDF_PNPPOWER_EVENT_CALLBACKS PnpPowerEventCallbacks; */

/* } WDFDEVICE_INIT; */
// SI: WDFDEVICE_INIT is an opaque pointer
struct _WDFDEVICE_INIT {
  WDF_PNPPOWER_EVENT_CALLBACKS PnpPowerEventCallbacks;
};
typedef struct _WDFDEVICE_INIT *WDFDEVICE_INIT;

// SLAyer: implemented in specific harnesses.
__checkReturn
__drv_maxIRQL(PASSIVE_LEVEL)
NTSTATUS
FORCEINLINE
WdfDeviceCreate(    __inout
    PWDFDEVICE_INIT* DeviceInit,
    __in_opt
    PWDF_OBJECT_ATTRIBUTES DeviceAttributes,
    __out
    WDFDEVICE* Device
                    ) ;

/* __drv_maxIRQL(DISPATCH_LEVEL) */
VOID
/* FORCEINLINE */
WdfDeviceInitSetPnpPowerEventCallbacks(
/*     __in */
    PWDFDEVICE_INIT DeviceInit,
/*     __in */
    PWDF_PNPPOWER_EVENT_CALLBACKS PnpPowerEventCallbacks
				       )
{
  // SI: This is the original implementation in wdfdevice.h:
  //    ((PFN_WDFDEVICEINITSETPNPPOWEREVENTCALLBACKS) WdfFunctions[WdfDeviceInitSetPnpPowerEventCallbacksTableIndex])(WdfDriverGlobals, DeviceInit, PnpPowerEventCallbacks);
  // SI: SDV's wdf_sdv_stubs.c provides no implementation for this
  // function, relying instead on it's dispatch method discovery
  // mechanism to work out which are the PnpPowerEventCallbacks
  // functions. We should be able to use the same. (Alternatively,
  // we could implement a WdfFunctions table.)
}

VOID WdfDeviceInitSetExclusive(
  /* [in] */  PWDFDEVICE_INIT DeviceInit,
  /* [in] */  BOOLEAN IsExclusive
)
{
  // Patch: leave as stub?
}


// Copied from c:/slam/WDK/inc/wdf/kmdf/1.9/wdfdevice.h, line 2877.
__checkReturn
__drv_maxIRQL(PASSIVE_LEVEL)
NTSTATUS
FORCEINLINE
WdfDeviceCreate(
    __inout
    PWDFDEVICE_INIT* DeviceInit,
    __in_opt
    PWDF_OBJECT_ATTRIBUTES DeviceAttributes,
    __out
    WDFDEVICE* Device
		);

WDFDEVICE MkSLAyerWdfDevice(PWDF_OBJECT_ATTRIBUTES DeviceAttributes)
{
  WDFDEVICE dev;
  dev = (WDFDEVICE)_SLAyer_malloc(sizeof(SLAyer_WDFOBJECT));
  dev->typ = SLAyerWdfDevice;
  dev->Context =
    (DeviceAttributes == WDF_NO_OBJECT_ATTRIBUTES) ? NULL :
    (*(DeviceAttributes->MkContext))() ;
  dev->typ = SLAyerWdfDevice;
  dev->typDevice.Queue = NULL;
  dev->typDevice.WmiInstance1 = NULL;
  dev->typDevice.WmiInstance2 = NULL;
  dev->typDevice.WmiInstance3 = NULL;
  return dev;
}

 __checkReturn
 __drv_maxIRQL(PASSIVE_LEVEL)
 NTSTATUS
 FORCEINLINE
 WdfDeviceCreate(    __inout
     PWDFDEVICE_INIT* DeviceInit,
     __in_opt
     PWDF_OBJECT_ATTRIBUTES DeviceAttributes,
     __out
     WDFDEVICE* Device
     )
 {
   //      return ((PFN_WDFDEVICECREATE) WdfFunctions[WdfDeviceCreateTableIndex])(WdfDriverGlobals, DeviceInit, DeviceAttributes, Device);
   NTSTATUS status;
   WDFDEVICE dev;

   // First time called.
   if (SL_num_of_devices == SL_devices_zero) {
     dev = MkSLAyerWdfDevice(DeviceAttributes);
     // Keep a handle on the device
     SL_Device_one = dev;
     SL_num_of_devices = SL_devices_one;
     // returns
     *Device = dev;
     status = STATUS_SUCCESS;
   }
   // Second time called.
   else if (SL_num_of_devices == SL_devices_one) {
     dev = MkSLAyerWdfDevice(DeviceAttributes);
     // Keep a handle on the device
     SL_Device_two = dev;
     SL_num_of_devices = SL_devices_two;
     // returns.
     *Device = dev;
     status = STATUS_SUCCESS;
   }
   // Called >2 times.
   else {
     status = STATUS_UNSUCCESSFUL;
   }

   // SI: We should also free DeviceInit as it'll otherwise leak.
   //  free(DeviceInit); DeviceInit = NULL;

   return status;
 }

// line 1294.
typedef struct _WDF_DEVICE_PNP_CAPABILITIES {
    //
    // Size of the structure in bytes
    //
    ULONG Size;

    //
    // NOTE: To mark a PDO as raw, call WdfPdoInitAssignRawDevice
    //

    WDF_TRI_STATE LockSupported;
    WDF_TRI_STATE EjectSupported;
    WDF_TRI_STATE Removable;
    WDF_TRI_STATE DockDevice;
    WDF_TRI_STATE UniqueID;
    WDF_TRI_STATE SilentInstall;
    WDF_TRI_STATE SurpriseRemovalOK;
    WDF_TRI_STATE HardwareDisabled;
    WDF_TRI_STATE NoDisplayInUI;

    //
    // Default values of -1 indicate not to set this value
    //
    ULONG Address;
    ULONG UINumber;

} WDF_DEVICE_PNP_CAPABILITIES, *PWDF_DEVICE_PNP_CAPABILITIES;

// line 1330
VOID
FORCEINLINE
WDF_DEVICE_POWER_POLICY_WAKE_SETTINGS_INIT(
    /*_Out_*/ PWDF_DEVICE_POWER_POLICY_WAKE_SETTINGS Settings
    )
{
    RtlZeroMemory(Settings, sizeof(WDF_DEVICE_POWER_POLICY_WAKE_SETTINGS));

    Settings->Size = sizeof(WDF_DEVICE_POWER_POLICY_WAKE_SETTINGS);

    Settings->Enabled = WdfUseDefault;
    Settings->DxState = PowerDeviceMaximum;
    Settings->UserControlOfWakeSettings = WakeAllowUserControl;
}

// line 2330
typedef
// _Must_inspect_result_
// _IRQL_requires_max_(DISPATCH_LEVEL)
WDFAPI
NTSTATUS
(*PFN_WDFDEVICEASSIGNS0IDLESETTINGS)(
    // _In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
    // _In_
    WDFDEVICE Device,
    // _In_
    PWDF_DEVICE_POWER_POLICY_IDLE_SETTINGS Settings
    );

/* PS#637 --KK */
// _Must_inspect_result_
// _IRQL_requires_max_(DISPATCH_LEVEL)
NTSTATUS
FORCEINLINE
WdfDeviceAssignS0IdleSettings(
    // _In_
    WDFDEVICE Device,
    // _In_
    PWDF_DEVICE_POWER_POLICY_IDLE_SETTINGS Settings
    )
{
    // return ((PFN_WDFDEVICEASSIGNS0IDLESETTINGS) WdfFunctions[WdfDeviceAssignS0IdleSettingsTableIndex])(WdfDriverGlobals, Device, Settings);
    NTSTATUS status;
    return status;
}

// line 2361
typedef
// _Must_inspect_result_
// _IRQL_requires_max_(DISPATCH_LEVEL)
WDFAPI
NTSTATUS
(*PFN_WDFDEVICEASSIGNSXWAKESETTINGS)(
    // _In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
    // _In_
    WDFDEVICE Device,
    // _In_
    PWDF_DEVICE_POWER_POLICY_WAKE_SETTINGS Settings
    );

/* PS#637 --KK */
// _Must_inspect_result_
// _IRQL_requires_max_(DISPATCH_LEVEL)
NTSTATUS
FORCEINLINE
WdfDeviceAssignSxWakeSettings(
    // _In_
    WDFDEVICE Device,
    // _In_
    PWDF_DEVICE_POWER_POLICY_WAKE_SETTINGS Settings
    )
{
    // return ((PFN_WDFDEVICEASSIGNSXWAKESETTINGS) WdfFunctions[WdfDeviceAssignSxWakeSettingsTableIndex])(WdfDriverGlobals, Device, Settings);
    NTSTATUS status;
    return status;
}

// line 3091
typedef
// _IRQL_requires_max_(DISPATCH_LEVEL)
WDFAPI
VOID
(*PFN_WDFDEVICEINITSETFILEOBJECTCONFIG)(
    // _In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
    // _In_
    PWDFDEVICE_INIT DeviceInit,
    // _In_
    PWDF_FILEOBJECT_CONFIG FileObjectConfig,
    // _In_opt_
    PWDF_OBJECT_ATTRIBUTES FileObjectAttributes
    );

// _IRQL_requires_max_(DISPATCH_LEVEL)
VOID
FORCEINLINE
WdfDeviceInitSetFileObjectConfig(
    // _In_
    PWDFDEVICE_INIT DeviceInit,
    // _In_
    PWDF_FILEOBJECT_CONFIG FileObjectConfig,
    // _In_opt_
    PWDF_OBJECT_ATTRIBUTES FileObjectAttributes
    )
{
    //((PFN_WDFDEVICEINITSETFILEOBJECTCONFIG) WdfFunctions[WdfDeviceInitSetFileObjectConfigTableIndex])(WdfDriverGlobals, DeviceInit, FileObjectConfig, FileObjectAttributes);
}

// SLAyer: implemented.
NTSTATUS
WdfRequestRetrieveInputBuffer
(
 WDFREQUEST Request,
 size_t     MinimumRequiredSize,
 PVOID      *Buffer,
 size_t     *Length
 )
{
  int nondet;
  NTSTATUS res;

  if (nondet) {
    *Buffer = Request->typRequest.InputBuffer;
    *Length = sizeof(Request->typRequest.InputBuffer);
    res = STATUS_SUCCESS ;
  } else {
    res = STATUS_UNSUCCESSFUL;
  }
  return res;
}

// SLAyer: probably needs implementing
NTSTATUS WdfRequestRetrieveOutputMemory(
  /* [in] */   WDFREQUEST Request,
  /* [out] */  WDFMEMORY *Memory
)
{
  SLAyer_nondetT(NTSTATUS);
}

NTSTATUS WdfRequestRetrieveInputMemory(
  /* [in] */   WDFREQUEST Request,
  /* [out] */  WDFMEMORY *Memory
)
{
  SLAyer_nondetT(NTSTATUS);
}

VOID WdfDeviceInitSetDeviceType(PWDFDEVICE_INIT DeviceInit, DEVICE_TYPE DeviceType)
{
}

// PS #644 DeviceInit lifetime
VOID WdfDeviceInitFree(PWDFDEVICE_INIT DeviceInit)
{
  //  free(DeviceInit);
}

VOID WDF_DEVICE_PNP_CAPABILITIES_INIT(PWDF_DEVICE_PNP_CAPABILITIES Caps)
{
}

VOID WdfDeviceSetPnpCapabilities(WDFDEVICE Device, PWDF_DEVICE_PNP_CAPABILITIES PnpCapabilities)
{
}

VOID WDF_DEVICE_POWER_CAPABILITIES_INIT(PWDF_DEVICE_POWER_CAPABILITIES Caps)
{
}

VOID WdfDeviceSetPowerCapabilities(WDFDEVICE Device, PWDF_DEVICE_POWER_CAPABILITIES PowerCapabilities)
{
}

NTSTATUS WdfDeviceAssignMofResourceName(
  /*[in]*/  WDFDEVICE Device,
  /*[in]*/  PCUNICODE_STRING MofResourceName
)
{
  NTSTATUS status;
  return status;
}

typedef ULONG ACCESS_MASK;

NTSTATUS WdfDeviceOpenRegistryKey(
  /*[in]*/            WDFDEVICE Device,
  /*[in]*/            ULONG DeviceInstanceKeyType,
  /*[in]*/            ACCESS_MASK DesiredAccess,
  /*[in, optional]*/  PWDF_OBJECT_ATTRIBUTES KeyAttributes,
  /*[out]*/           WDFKEY *Key
)
{
  SLAyer_nondetT(NTSTATUS);
}

// line 1389
VOID
FORCEINLINE
WDF_DEVICE_STATE_INIT(
    /*_Out_*/ PWDF_DEVICE_STATE PnpDeviceState
    )
{
    RtlZeroMemory(PnpDeviceState, sizeof(WDF_DEVICE_STATE));

    PnpDeviceState->Size = sizeof(WDF_DEVICE_STATE);

    //
    // Initializes all of the fields to the WdfUseDefault enum value
    //
    PnpDeviceState->Disabled = WdfUseDefault;
    PnpDeviceState->DontDisplayInUI = WdfUseDefault;
    PnpDeviceState->Failed = WdfUseDefault;
    PnpDeviceState->NotDisableable = WdfUseDefault;
    PnpDeviceState->Removed = WdfUseDefault;
    PnpDeviceState->ResourcesChanged = WdfUseDefault;
}

// line 2985
// _Must_inspect_result_
// _IRQL_requires_max_(PASSIVE_LEVEL)
NTSTATUS
FORCEINLINE
WdfDeviceInitAssignName(
    // _In_
    PWDFDEVICE_INIT DeviceInit,
    // _In_opt_
    PCUNICODE_STRING DeviceName
    )
{
  /* Can't really do what the documentation wants me to do, since
   * DeviceInit is an opaque structure.
   */
    // return ((PFN_WDFDEVICEINITASSIGNNAME) WdfFunctions[WdfDeviceInitAssignNameTableIndex])(WdfDriverGlobals, DeviceInit, DeviceName);
  int x;
  NTSTATUS status;
  status =  x ? STATUS_SUCCESS
              : STATUS_INSUFFICIENT_RESOURCES;
  return status;
}

// line 1764
typedef
//_IRQL_requires_max_(DISPATCH_LEVEL)
WDFAPI
VOID
(*PFN_WDFDEVICESETDEVICESTATE)(
    // _In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
    // _In_
    WDFDEVICE Device,
    // _In_
    PWDF_DEVICE_STATE DeviceState
    );
/* PS#637 --KK */
// _IRQL_requires_max_(DISPATCH_LEVEL)
VOID
FORCEINLINE
WdfDeviceSetDeviceState(
    // _In_
    WDFDEVICE Device,
    // _In_
    PWDF_DEVICE_STATE DeviceState
    )
{
    // ((PFN_WDFDEVICESETDEVICESTATE) WdfFunctions[WdfDeviceSetDeviceStateTableIndex])(WdfDriverGlobals, Device, DeviceState);
}

NTSTATUS WdfDeviceCreateDeviceInterface(
  /*[in]*/            WDFDEVICE Device,
  /*[in]*/            const GUID *InterfaceClassGUID,
  /*[in, optional]*/  PCUNICODE_STRING ReferenceString
)
{
  SLAyer_nondetT(NTSTATUS);
}

VOID WdfDeviceSetBusInformationForChildren(
  /*[in]*/  WDFDEVICE Device,
  /*[in]*/  PPNP_BUS_INFORMATION BusInformation
)
{
}

// SI: wrong. We probably need a harness IoTarget
WDFIOTARGET WdfDeviceGetIoTarget(
  WDFDEVICE Device
)
{
  return NULL;
}

NTSTATUS WdfIoTargetCreate(
  /*[in]*/            WDFDEVICE Device,
  /*[in, optional]*/  PWDF_OBJECT_ATTRIBUTES IoTargetAttributes,
  /*[out]*/           WDFIOTARGET *IoTarget
)
{
  int x;
  NTSTATUS status;

  if (x) {
    WDFIOTARGET target;
    target = (WDFIOTARGET)_SLAyer_malloc(sizeof(SLAyer_WDFOBJECT));
    target->typ = SLAyerWdfIoTarget;
    target->Context =
      (IoTargetAttributes == WDF_NO_OBJECT_ATTRIBUTES) ? NULL :
      (*(IoTargetAttributes->MkContext))() ;
    SL_IoTarget = target;
    *IoTarget = SL_IoTarget;
    status = STATUS_SUCCESS;
  } else {
    status = STATUS_UNSUCCESSFUL;
  }
  return status;
}

NTSTATUS WdfIoTargetClose(
  /*[in]*/            WDFIOTARGET *IoTarget
)
{
  SLAyer_nondetT(NTSTATUS);
}

// line 2143
typedef
//_IRQL_requires_max_(DISPATCH_LEVEL)
//WDFAPI
WDFDRIVER
(*PFN_WDFDEVICEGETDRIVER)(
    //_In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
    //_In_
    WDFDEVICE Device
    );

/* Reimplementing myself. This function is called from
 * WdfDeviceGetDriver, but the WDF implements this funciton using some
 * unknown array. --KK #PS637
 */
//_IRQL_requires_max_(DISPATCH_LEVEL)
WDFDRIVER
FORCEINLINE
WdfDeviceGetDriver(
    //_In_
    WDFDEVICE Device
    )
{
    //return ((PFN_WDFDEVICEGETDRIVER) WdfFunctions[WdfDeviceGetDriverTableIndex])(WdfDriverGlobals, Device);
    WDFDRIVER d;
    return d;
}

// line 2630
typedef
// _IRQL_requires_max_(DISPATCH_LEVEL)
// WDFAPI
VOID
(*PFN_WDFDEVICEINITSETPOWERPOLICYEVENTCALLBACKS)(
    // _In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
    // _In_
    PWDFDEVICE_INIT DeviceInit,
    // _In_
    PWDF_POWER_POLICY_EVENT_CALLBACKS PowerPolicyEventCallbacks
    );

/* PS#637 --KK */
// _IRQL_requires_max_(DISPATCH_LEVEL)
VOID
FORCEINLINE
WdfDeviceInitSetPowerPolicyEventCallbacks(
    // _In_
    PWDFDEVICE_INIT DeviceInit,
    // _In_
    PWDF_POWER_POLICY_EVENT_CALLBACKS PowerPolicyEventCallbacks
    )
{
    //((PFN_WDFDEVICEINITSETPOWERPOLICYEVENTCALLBACKS) WdfFunctions[WdfDeviceInitSetPowerPolicyEventCallbacksTableIndex])(WdfDriverGlobals, DeviceInit, PowerPolicyEventCallbacks);
}

// line 3505
typedef
// _Must_inspect_result_
// _IRQL_requires_max_(PASSIVE_LEVEL)
WDFAPI
NTSTATUS
(*PFN_WDFDEVICEALLOCANDQUERYPROPERTY)(
    // _In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
    // _In_
    WDFDEVICE Device,
    // _In_
    DEVICE_REGISTRY_PROPERTY DeviceProperty,
    // _In_
    // _Strict_type_match_
    POOL_TYPE PoolType,
    // _In_opt_
    PWDF_OBJECT_ATTRIBUTES PropertyMemoryAttributes,
    // _Out_
    WDFMEMORY* PropertyMemory
    );

/* PS#637 */
// _Must_inspect_result_
// _IRQL_requires_max_(PASSIVE_LEVEL)
NTSTATUS
FORCEINLINE
WdfDeviceAllocAndQueryProperty(
    // _In_
    WDFDEVICE Device,
    // _In_
    DEVICE_REGISTRY_PROPERTY DeviceProperty,
    // _In_
    // _Strict_type_match_
    POOL_TYPE PoolType,
    // _In_opt_
    PWDF_OBJECT_ATTRIBUTES PropertyMemoryAttributes,
    // _Out_
    WDFMEMORY* PropertyMemory
    )
{
    // return ((PFN_WDFDEVICEALLOCANDQUERYPROPERTY) WdfFunctions[WdfDeviceAllocAndQueryPropertyTableIndex])(WdfDriverGlobals, Device, DeviceProperty, PoolType, PropertyMemoryAttributes, PropertyMemory);
    NTSTATUS status;
    return status;
}

// _Must_inspect_result_
// _IRQL_requires_max_(PASSIVE_LEVEL)
NTSTATUS
FORCEINLINE
WdfDeviceCreateSymbolicLink(
    // _In_
    WDFDEVICE Device,
    // _In_
    PCUNICODE_STRING SymbolicLinkName
    )
{
    // return ((PFN_WDFDEVICECREATESYMBOLICLINK) WdfFunctions[WdfDeviceCreateSymbolicLinkTableIndex])(WdfDriverGlobals, Device, SymbolicLinkName);

  // Don't know what this method is supposed to do.
  NTSTATUS status;
  return status;
}

/******************************************************************************
 * File: wdfrequest.h
 ******************************************************************************/
// Copied from line 42
typedef enum _WDF_REQUEST_TYPE {
    WdfRequestTypeCreate = 0x0,
    WdfRequestTypeCreateNamedPipe = 0x1,
    WdfRequestTypeClose = 0x2,
    WdfRequestTypeRead = 0x3,
    WdfRequestTypeWrite = 0x4,
    WdfRequestTypeQueryInformation = 0x5,
    WdfRequestTypeSetInformation = 0x6,
    WdfRequestTypeQueryEA = 0x7,
    WdfRequestTypeSetEA = 0x8,
    WdfRequestTypeFlushBuffers = 0x9,
    WdfRequestTypeQueryVolumeInformation = 0xa,
    WdfRequestTypeSetVolumeInformation = 0xb,
    WdfRequestTypeDirectoryControl = 0xc,
    WdfRequestTypeFileSystemControl = 0xd,
    WdfRequestTypeDeviceControl = 0xe,
    WdfRequestTypeDeviceControlInternal = 0xf,
    WdfRequestTypeShutdown = 0x10,
    WdfRequestTypeLockControl = 0x11,
    WdfRequestTypeCleanup = 0x12,
    WdfRequestTypeCreateMailSlot = 0x13,
    WdfRequestTypeQuerySecurity = 0x14,
    WdfRequestTypeSetSecurity = 0x15,
    WdfRequestTypePower = 0x16,
    WdfRequestTypeSystemControl = 0x17,
    WdfRequestTypeDeviceChange = 0x18,
    WdfRequestTypeQueryQuota = 0x19,
    WdfRequestTypeSetQuota = 0x1A,
    WdfRequestTypePnp = 0x1B,
    WdfRequestTypeOther =0x1C,
    WdfRequestTypeUsb = 0x40,
    WdfRequestTypeNoFormat = 0xFF,
    WdfRequestTypeMax,
} WDF_REQUEST_TYPE;

// line 77
typedef enum _WDF_REQUEST_REUSE_FLAGS {
    WDF_REQUEST_REUSE_NO_FLAGS = 0x00000000,
    WDF_REQUEST_REUSE_SET_NEW_IRP = 0x00000001,
} WDF_REQUEST_REUSE_FLAGS;

// kmdf/1.11/wdfrequest.h, line 93
typedef enum _WDF_REQUEST_SEND_OPTIONS_FLAGS {
    WDF_REQUEST_SEND_OPTION_TIMEOUT = 0x00000001,
    WDF_REQUEST_SEND_OPTION_SYNCHRONOUS = 0x00000002,
    WDF_REQUEST_SEND_OPTION_IGNORE_TARGET_STATE = 0x00000004,
    WDF_REQUEST_SEND_OPTION_SEND_AND_FORGET = 0x00000008,
} WDF_REQUEST_SEND_OPTIONS_FLAGS;

// line 119
typedef struct _WDF_REQUEST_PARAMETERS {
    USHORT Size;
/*
    UCHAR MinorFunction;
    WDF_REQUEST_TYPE Type;
    // The following user parameters are based on the service that is being
    // invoked.  Drivers and file systems can determine which set to use based
    // on the above major and minor function codes.
    union {
        //
        // System service parameters for:  Create
        //
        struct {
            PIO_SECURITY_CONTEXT SecurityContext;
            ULONG Options;
            USHORT POINTER_ALIGNMENT FileAttributes;
            USHORT ShareAccess;
            ULONG POINTER_ALIGNMENT EaLength;
        } Create;
        // System service parameters for:  Read
        struct {
            size_t Length;
            ULONG POINTER_ALIGNMENT Key;
            LONGLONG DeviceOffset;
        } Read;
        // System service parameters for:  Write
        struct {
            size_t Length;
            ULONG POINTER_ALIGNMENT Key;
            LONGLONG DeviceOffset;
        } Write;
        // System service parameters for:  Device Control
        // Note that the user's output buffer is stored in the UserBuffer field
        // and the user's input buffer is stored in the SystemBuffer field.
        struct {
            size_t OutputBufferLength;
            size_t POINTER_ALIGNMENT InputBufferLength;
            ULONG POINTER_ALIGNMENT IoControlCode;
            PVOID Type3InputBuffer;
        } DeviceIoControl;
        struct {
            PVOID Arg1;
            PVOID  Arg2;
            ULONG POINTER_ALIGNMENT IoControlCode;
            PVOID Arg4;
        } Others;
    } Parameters;
*/
} WDF_REQUEST_PARAMETERS, *PWDF_REQUEST_PARAMETERS;

// copied from line 203
typedef struct _WDF_USB_REQUEST_COMPLETION_PARAMS *PWDF_USB_REQUEST_COMPLETION_PARAMS;

typedef struct _WDF_REQUEST_COMPLETION_PARAMS {
    //
    // Size of the structure in bytes
    //
    ULONG Size;

    WDF_REQUEST_TYPE Type;
    IO_STATUS_BLOCK IoStatus;

    union {
        struct {
            WDFMEMORY Buffer;
            size_t Length;
            size_t Offset;
        } Write;

        struct {
            WDFMEMORY Buffer;
            size_t Length;
            size_t Offset;
        } Read;

        struct {
            ULONG IoControlCode;

            struct {
                WDFMEMORY Buffer;
                size_t Offset;
            } Input;

            struct {
                WDFMEMORY Buffer;
                size_t Offset;
                size_t Length;
            } Output;
        } Ioctl;

        struct {
            union {
                PVOID Ptr;
                ULONG_PTR Value;
            } Argument1;
            union {
                PVOID Ptr;
                ULONG_PTR Value;
            } Argument2;
            union {
                PVOID Ptr;
                ULONG_PTR Value;
            } Argument3;
            union {
                PVOID Ptr;
                ULONG_PTR Value;
            } Argument4;
        } Others;

        struct {
            PWDF_USB_REQUEST_COMPLETION_PARAMS Completion;
        } Usb;
    } Parameters;

} WDF_REQUEST_COMPLETION_PARAMS, *PWDF_REQUEST_COMPLETION_PARAMS;

// Copied from line 281
 typedef
// _Function_class_(EVT_WDF_REQUEST_COMPLETION_ROUTINE)
// _IRQL_requires_same_
 VOID
 EVT_WDF_REQUEST_COMPLETION_ROUTINE(
     _In_
     WDFREQUEST Request,
     _In_
     WDFIOTARGET Target,
     _In_
     PWDF_REQUEST_COMPLETION_PARAMS Params,
     _In_
     WDFCONTEXT Context
     );

 typedef EVT_WDF_REQUEST_COMPLETION_ROUTINE *PFN_WDF_REQUEST_COMPLETION_ROUTINE;

// line kdmf/1.11/wdfrequest.h, line 318.
typedef struct _WDF_REQUEST_REUSE_PARAMS {
    //
    // Size of this structure in bytes
    //
    ULONG Size;

    //
    // Bit field combination of WDF_REQUEST_REUSE_Xxx values
    //
    ULONG Flags;

    //
    // The new status of the request.
    //
    NTSTATUS Status;

    //
    // New PIRP  to be contained in the WDFREQUEST.   Setting a new PIRP value
    // is only valid for WDFREQUESTs created by WdfRequestCreateFromIrp where
    // RequestFreesIrp == FALSE.  No other WDFREQUESTs (presented by the
    // I/O queue for instance) may have their IRPs changed.
    //
    PIRP NewIrp;

} WDF_REQUEST_REUSE_PARAMS, *PWDF_REQUEST_REUSE_PARAMS;

VOID
FORCEINLINE
WDF_REQUEST_REUSE_PARAMS_INIT(
    /*_Out_*/ PWDF_REQUEST_REUSE_PARAMS Params,
    /*_In_ */ ULONG Flags,
    /*_In_ */ NTSTATUS Status
    )
{
    RtlZeroMemory(Params, sizeof(WDF_REQUEST_REUSE_PARAMS));

    Params->Size = sizeof(WDF_REQUEST_REUSE_PARAMS);
    Params->Flags = Flags;
    Params->Status = Status;
}

// Line 522
typedef
//_IRQL_requires_max_(DISPATCH_LEVEL)
//WDFAPI
NTSTATUS
(*PFN_WDFREQUESTREUSE)(
 //   _In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
  //  _In_
    WDFREQUEST Request,
   // _In_
    PWDF_REQUEST_REUSE_PARAMS ReuseParams
    );

VOID WdfRequestComplete(
  /*[in]*/  WDFREQUEST Request,
  /*[in]*/  NTSTATUS Status
)
{
}

//_IRQL_requires_max_(DISPATCH_LEVEL)
NTSTATUS
FORCEINLINE
WdfRequestReuse(
    //_In_
    WDFREQUEST Request,
    //_In_
    PWDF_REQUEST_REUSE_PARAMS ReuseParams
    )
{
     // return ((PFN_WDFREQUESTREUSE) WdfFunctions[WdfRequestReuseTableIndex])(WdfDriverGlobals, Request, ReuseParams);
    SLAyer_nondetT(NTSTATUS);
}

//
// WDF Function: WdfRequestCreate
//
/* typedef */
/* _Must_inspect_result_ */
/* _IRQL_requires_max_(DISPATCH_LEVEL) */
/* WDFAPI */
/* NTSTATUS */
/* (*PFN_WDFREQUESTCREATE)( */
/*     _In_ */
/*     PWDF_DRIVER_GLOBALS DriverGlobals, */
/*     _In_opt_ */
/*     PWDF_OBJECT_ATTRIBUTES RequestAttributes, */
/*     _In_opt_ */
/*     WDFIOTARGET IoTarget, */
/*     _Out_ */
/*     WDFREQUEST* Request */
/*     ); */

/* _Must_inspect_result_ */
/* _IRQL_requires_max_(DISPATCH_LEVEL) */
NTSTATUS
FORCEINLINE
WdfRequestCreate(
    _In_opt_
    PWDF_OBJECT_ATTRIBUTES RequestAttributes,
    _In_opt_
    WDFIOTARGET IoTarget,
    _Out_
    WDFREQUEST* Request
    )
{
/*     return ((PFN_WDFREQUESTCREATE) WdfFunctions[WdfRequestCreateTableIndex])(WdfDriverGlobals, RequestAttributes, IoTarget, Request); */
  int x;
  if (x) {
    WDFREQUEST req = (WDFREQUEST)_SLAyer_malloc(sizeof(SLAyer_WDFOBJECT));
    req->typ = SLAyerWdfRequest;
    req->Context =
      (RequestAttributes == WDF_NO_OBJECT_ATTRIBUTES) ? NULL :
      (*(RequestAttributes->MkContext))() ;
    *Request = req;
    return STATUS_SUCCESS;
  } else {
    return STATUS_UNSUCCESSFUL;
  }
}

typedef VOID (*PINTERFACE_REFERENCE)(PVOID Context);
typedef VOID (*PINTERFACE_DEREFERENCE)(PVOID Context);

typedef struct _INTERFACE {
  unsigned int Size;
  unsigned int Version;
  void *       Context;
  PINTERFACE_REFERENCE   InterfaceReference;
  PINTERFACE_DEREFERENCE InterfaceDereference;
} INTERFACE, *PINTERFACE;

VOID WdfRequestCompleteWithInformation(
    WDFREQUEST Request,
    NTSTATUS Status,
    ULONG_PTR Information
)
{}

// Line 370
typedef struct _WDF_REQUEST_SEND_OPTIONS {
    //
    // Size of the structure in bytes
    //
    ULONG Size;

    //
    // Bit field combination of values from the WDF_REQUEST_SEND_OPTIONS_FLAGS
    // enumeration
    //
    ULONG Flags;

    //
    // Valid when WDF_REQUEST_SEND_OPTION_TIMEOUT is set
    //
    LONGLONG Timeout;

} WDF_REQUEST_SEND_OPTIONS, *PWDF_REQUEST_SEND_OPTIONS;

VOID
FORCEINLINE
WDF_REQUEST_SEND_OPTIONS_INIT(
    _Out_ PWDF_REQUEST_SEND_OPTIONS Options,
    _In_ ULONG Flags
    )
{
    RtlZeroMemory(Options, sizeof(WDF_REQUEST_SEND_OPTIONS));
    Options->Size = sizeof(WDF_REQUEST_SEND_OPTIONS);
    Options->Flags = Flags;
}

// 1.11, line 593
//_IRQL_requires_max_(DISPATCH_LEVEL)
VOID
FORCEINLINE
WdfRequestFormatRequestUsingCurrentType(
    _In_
    WDFREQUEST Request
    )
{
  //    ((PFN_WDFREQUESTFORMATREQUESTUSINGCURRENTTYPE) WdfFunctions[WdfRequestFormatRequestUsingCurrentTypeTableIndex])(WdfDriverGlobals, Request);
}

// Line 636
typedef
// _IRQL_requires_max_(DISPATCH_LEVEL)
// _When_(Options->Flags & WDF_REQUEST_SEND_OPTION_SYNCHRONOUS == 0, _Must_inspect_result_)
WDFAPI
BOOLEAN
(*PFN_WDFREQUESTSEND)(
    // _In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
    // _In_
    WDFREQUEST Request,
    // _In_
    WDFIOTARGET Target,
    // _In_opt_
    PWDF_REQUEST_SEND_OPTIONS Options
    );

/* Reimplementing. THe function WdfRequestSend calls this function,
 * but the WDF implements this function in some unknown array. -- KK
 * #PS637
 */
// _IRQL_requires_max_(DISPATCH_LEVEL)
// _When_(Options->Flags & WDF_REQUEST_SEND_OPTION_SYNCHRONOUS == 0, _Must_inspect_result_)
BOOLEAN
FORCEINLINE
WdfRequestSend(
    // _In_
    WDFREQUEST Request,
    // _In_
    WDFIOTARGET Target,
    // _In_opt_
    PWDF_REQUEST_SEND_OPTIONS Options
    )
{
    // return ((PFN_WDFREQUESTSEND) WdfFunctions[WdfRequestSendTableIndex])(WdfDriverGlobals, Request, Target, Options);
    BOOLEAN b;
    return b;
}

// line 671
typedef
// _Must_inspect_result_
// _IRQL_requires_max_(DISPATCH_LEVEL)
WDFAPI
NTSTATUS
(*PFN_WDFREQUESTGETSTATUS)(
    // _In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
    // _In_
    WDFREQUEST Request
    );

/* Reimplementing. The function WdfRequestGetStatus calls this
 * function, but the WDF implements this function in some unknown
 * array. --KK #PS637
 */
// _Must_inspect_result_
// _IRQL_requires_max_(DISPATCH_LEVEL)
NTSTATUS
FORCEINLINE
WdfRequestGetStatus(
    // _In_
    WDFREQUEST Request
    )
{
    // return ((PFN_WDFREQUESTGETSTATUS) WdfFunctions[WdfRequestGetStatusTableIndex])(WdfDriverGlobals, Request);
    NTSTATUS status;
    return status;
}

// Line 864
typedef
//_IRQL_requires_max_(DISPATCH_LEVEL)
WDFAPI
VOID
(*PFN_WDFREQUESTSETCOMPLETIONROUTINE)(
    //_In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
    //_In_
    WDFREQUEST Request,
    //_In_opt_
    PFN_WDF_REQUEST_COMPLETION_ROUTINE CompletionRoutine,
    //_In_opt_ __drv_aliasesMem
    WDFCONTEXT CompletionContext
    );

/* Reimplementing this. The function WdfRequestSetCompletionRoutine
 * needs this function, but WDF implements it using some unknown array.
 * -- KK #PS637
 */
//_IRQL_requires_max_(DISPATCH_LEVEL)
VOID
FORCEINLINE
WdfRequestSetCompletionRoutine(
    //_In_
    WDFREQUEST Request,
    //_In_opt_
    PFN_WDF_REQUEST_COMPLETION_ROUTINE CompletionRoutine,
    //_In_opt_ __drv_aliasesMem
    WDFCONTEXT CompletionContext
    )
{
    // ((PFN_WDFREQUESTSETCOMPLETIONROUTINE) WdfFunctions[WdfRequestSetCompletionRoutineTableIndex])(WdfDriverGlobals, Request, CompletionRoutine, CompletionContext);
}

/******************************************************************************
 * File: wdfiotarget.h
 ******************************************************************************/

// Copied from line 69

typedef
//_Function_class_(EVT_WDF_IO_TARGET_QUERY_REMOVE)
//_IRQL_requires_same_
//_IRQL_requires_max_(PASSIVE_LEVEL)
NTSTATUS
EVT_WDF_IO_TARGET_QUERY_REMOVE(
    _In_
    WDFIOTARGET IoTarget
    );

typedef EVT_WDF_IO_TARGET_QUERY_REMOVE *PFN_WDF_IO_TARGET_QUERY_REMOVE;

typedef
//_Function_class_(EVT_WDF_IO_TARGET_REMOVE_CANCELED)
//_IRQL_requires_same_
//_IRQL_requires_max_(PASSIVE_LEVEL)
VOID
EVT_WDF_IO_TARGET_REMOVE_CANCELED(
    _In_
    WDFIOTARGET IoTarget
    );

typedef EVT_WDF_IO_TARGET_REMOVE_CANCELED *PFN_WDF_IO_TARGET_REMOVE_CANCELED;

typedef
//_Function_class_(EVT_WDF_IO_TARGET_REMOVE_COMPLETE)
//_IRQL_requires_same_
//_IRQL_requires_max_(PASSIVE_LEVEL)
VOID
EVT_WDF_IO_TARGET_REMOVE_COMPLETE(
    _In_
    WDFIOTARGET IoTarget
    );

typedef EVT_WDF_IO_TARGET_REMOVE_COMPLETE *PFN_WDF_IO_TARGET_REMOVE_COMPLETE;

typedef enum _WDF_IO_TARGET_OPEN_TYPE {
    WdfIoTargetOpenUndefined = 0,
    WdfIoTargetOpenUseExistingDevice = 1,
    WdfIoTargetOpenByName = 2,
    WdfIoTargetOpenReopen = 3,
} WDF_IO_TARGET_OPEN_TYPE;

// Patch: remove most of the struct (not needed in CromData_trace).
typedef struct _WDF_IO_TARGET_OPEN_PARAMS {
    //
    // Size of this structure in bytes
    //
    ULONG Size;

/*     // */
/*     // Indicates which fields of this structure are going to be used in */
/*     // creating the WDFIOTARGET. */
/*     // */
     WDF_IO_TARGET_OPEN_TYPE Type;

/*     // */
/*     // Notification when the target is being queried for removal. */
/*     // If !NT_SUCCESS is returned, the query will fail and the target will */
/*     // remain opened. */
/*     // */
     PFN_WDF_IO_TARGET_QUERY_REMOVE EvtIoTargetQueryRemove;

/*     // */
/*     // The previous query remove has been canceled and the target can now be */
/*     // reopened. */
/*     // */
     PFN_WDF_IO_TARGET_REMOVE_CANCELED EvtIoTargetRemoveCanceled;

/*     // */
/*     // The query remove has succeeded and the target is now removed from the */
/*     // system. */
/*     // */
     PFN_WDF_IO_TARGET_REMOVE_COMPLETE EvtIoTargetRemoveComplete;

/*     // ========== WdfIoTargetOpenUseExistingDevice begin ========== */
/*     // */
/*     // The device object to send requests to */
/*     // */
/*     PDEVICE_OBJECT TargetDeviceObject; */

/*     // */
/*     // File object representing the TargetDeviceObject.  The PFILE_OBJECT will */
/*     // be passed as a parameter in all requests sent to the resulting */
/*     // WDFIOTARGET. */
/*     // */
/*     PFILE_OBJECT TargetFileObject; */
/*     // ========== WdfIoTargetOpenUseExistingDevice end ========== */

/*     // ========== WdfIoTargetOpenByName begin ========== */
/*     // */
/*     // Name of the device to open. */
/*     // */
     UNICODE_STRING TargetDeviceName;

/*     // */
/*     // The access desired on the device being opened up, ie WDM FILE_XXX_ACCESS */
/*     // such as FILE_ANY_ACCESS, FILE_SPECIAL_ACCESS, FILE_READ_ACCESS, or */
/*     // FILE_WRITE_ACCESS or you can use values such as GENERIC_READ, */
/*     // GENERIC_WRITE, or GENERIC_ALL. */
/*     // */
     ACCESS_MASK DesiredAccess;

/*     // */
/*     // Share access desired on the target being opened, ie WDM FILE_SHARE_XXX */
/*     // values such as FILE_SHARE_READ, FILE_SHARE_WRITE, FILE_SHARE_DELETE. */
/*     // */
/*     // A zero value means exclusive access to the target. */
/*     // */
     ULONG ShareAccess;

/*     // */
/*     // File  attributes, see ZwCreateFile in the DDK for a list of valid */
/*     // values and their meaning. */
/*     // */
/*     ULONG FileAttributes; */

/*     // */
/*     // Create disposition, see ZwCreateFile in the DDK for a list of valid */
/*     // values and their meaning. */
/*     // */
     ULONG CreateDisposition;

/*     // */
/*     // Options for opening the device, see CreateOptions for ZwCreateFile in the */
/*     // DDK for a list of valid values and their meaning. */
/*     // */
     ULONG CreateOptions;

/*     PVOID EaBuffer; */

/*     ULONG EaBufferLength; */

/*     PLONGLONG AllocationSize; */

/*     // ========== WdfIoTargetOpenByName end ========== */

/*     // */
/*     // On return for a create by name, this will contain one of the following */
/*     // values:  FILE_CREATED, FILE_OPENED, FILE_OVERWRITTEN, FILE_SUPERSEDED, */
/*     //          FILE_EXISTS, FILE_DOES_NOT_EXIST */
/*     // */
/*     ULONG FileInformation; */

} WDF_IO_TARGET_OPEN_PARAMS, *PWDF_IO_TARGET_OPEN_PARAMS;

// Copied frol line 222
VOID
FORCEINLINE
WDF_IO_TARGET_OPEN_PARAMS_INIT_CREATE_BY_NAME(
    /*_Out_*/ PWDF_IO_TARGET_OPEN_PARAMS Params,
    /*_In_ */ PCUNICODE_STRING TargetDeviceName,
    /*_In_ */ ACCESS_MASK DesiredAccess
    )
{
    RtlZeroMemory(Params, sizeof(WDF_IO_TARGET_OPEN_PARAMS));

    Params->Size = sizeof(WDF_IO_TARGET_OPEN_PARAMS);
    Params->Type = WdfIoTargetOpenByName;

    RtlCopyMemory(&Params->TargetDeviceName,
                  TargetDeviceName,
                  sizeof(UNICODE_STRING));
    Params->DesiredAccess = DesiredAccess;
    Params->CreateOptions = FILE_NON_DIRECTORY_FILE;
}

VOID
FORCEINLINE
WDF_IO_TARGET_OPEN_PARAMS_INIT_OPEN_BY_NAME(
    /*_Out_*/ PWDF_IO_TARGET_OPEN_PARAMS Params,
    /*_In_*/ PCUNICODE_STRING TargetDeviceName,
    /*_In_*/ ACCESS_MASK DesiredAccess
    )
{
    WDF_IO_TARGET_OPEN_PARAMS_INIT_CREATE_BY_NAME(Params,
                                                  TargetDeviceName,
                                                  DesiredAccess);
    Params->CreateDisposition = FILE_OPEN;
}

VOID
FORCEINLINE
WDF_IO_TARGET_OPEN_PARAMS_INIT_REOPEN(
    /*_Out_*/ PWDF_IO_TARGET_OPEN_PARAMS Params
    )
{
    RtlZeroMemory(Params, sizeof(WDF_IO_TARGET_OPEN_PARAMS));

    Params->Size = sizeof(WDF_IO_TARGET_OPEN_PARAMS);
    Params->Type = WdfIoTargetOpenReopen;
}

typedef struct _WDFMEMORY_OFFSET {
    //
    // Offset into the WDFMEMORY that the operation should start at.
    //
    size_t BufferOffset;

    //
    // Number of bytes that the operation should access.  If 0, the entire
    // length of the WDFMEMORY buffer will be used in the operation or ignored
    // depending on the API.
    //
    size_t BufferLength;

} WDFMEMORY_OFFSET, *PWDFMEMORY_OFFSET;

// line 306
typedef
// _Must_inspect_result_
// _IRQL_requires_max_(PASSIVE_LEVEL)
WDFAPI
NTSTATUS
(*PFN_WDFIOTARGETOPEN)(
    // _In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
    // _In_
    WDFIOTARGET IoTarget,
    // _In_
    PWDF_IO_TARGET_OPEN_PARAMS OpenParams
    );
/* Reimplementing. The function WdfIoTargetOpen calls this function, but
 * this function is implemented by the WDF in some unknown array. --KK
 * #PS637
 */
// _Must_inspect_result_
// _IRQL_requires_max_(PASSIVE_LEVEL)
NTSTATUS
FORCEINLINE
WdfIoTargetOpen(
    // _In_
    WDFIOTARGET IoTarget,
    // _In_
    PWDF_IO_TARGET_OPEN_PARAMS OpenParams
    )
{
    // return ((PFN_WDFIOTARGETOPEN) WdfFunctions[WdfIoTargetOpenTableIndex])(WdfDriverGlobals, IoTarget, OpenParams);
    NTSTATUS status;
    return status;
}

// Line 337
typedef
//_IRQL_requires_max_(PASSIVE_LEVEL)
WDFAPI
VOID
(*PFN_WDFIOTARGETCLOSEFORQUERYREMOVE)(
    //_In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
    //_In_
    WDFIOTARGET IoTarget
    );

/* Reimplementing myself. This function is alled by
 * WdfIoTargetCloseForQueryRemove, but the WDF implements this function
 * using some unknown array. --KK #PS637
 */
//_IRQL_requires_max_(PASSIVE_LEVEL)
VOID
FORCEINLINE
WdfIoTargetCloseForQueryRemove(
    //_In_
    WDFIOTARGET IoTarget
    )
{
    //((PFN_WDFIOTARGETCLOSEFORQUERYREMOVE) WdfFunctions[WdfIoTargetCloseForQueryRemoveTableIndex])(WdfDriverGlobals, IoTarget);
}

// line 904
typedef
//_Must_inspect_result_
//_IRQL_requires_max_(DISPATCH_LEVEL)
// WDFAPI
NTSTATUS
(*PFN_WDFIOTARGETFORMATREQUESTFORWRITE)(
    //_In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
    //_In_
    WDFIOTARGET IoTarget,
    //_In_
    WDFREQUEST Request,
    //_In_opt_
    WDFMEMORY InputBuffer,
    //_In_opt_
    PWDFMEMORY_OFFSET InputBufferOffset,
    //_In_opt_
    PLONGLONG DeviceOffset
    );

/* Reimplementing this as a stub. THe function
 * WdfIoTargetFormatRequestForWrite calls this, but it is defined in
 * some unknown array. -- KK #PS637
 */
//_Must_inspect_result_
//_IRQL_requires_max_(DISPATCH_LEVEL)
NTSTATUS
FORCEINLINE
WdfIoTargetFormatRequestForWrite(
    //_In_
    WDFIOTARGET IoTarget,
    //_In_
    WDFREQUEST Request,
    //_In_opt_
    WDFMEMORY InputBuffer,
    //_In_opt_
    PWDFMEMORY_OFFSET InputBufferOffset,
    //_In_opt_
    PLONGLONG DeviceOffset
    )
{
    /* return ((PFN_WDFIOTARGETFORMATREQUESTFORWRITE) WdfFunctions[WdfIoTargetFormatRequestForWriteTableIndex]
)(WdfDriverGlobals, IoTarget, Request, InputBuffer, InputBufferOffset, DeviceOffset); */    NTSTATUS status;
    return status;
}

typedef
// _Must_inspect_result_
// _IRQL_requires_max_(PASSIVE_LEVEL)
WDFAPI
NTSTATUS
(*PFN_WDFIOTARGETALLOCANDQUERYTARGETPROPERTY)(
    // _In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
    // _In_
    WDFIOTARGET IoTarget,
    // _In_
    DEVICE_REGISTRY_PROPERTY DeviceProperty,
    // _In_
    // _Strict_type_match_
    POOL_TYPE PoolType,
    // _In_opt_
    PWDF_OBJECT_ATTRIBUTES PropertyMemoryAttributes,
    // _Out_
    WDFMEMORY* PropertyMemory
    );

PFN_WDFIOTARGETALLOCANDQUERYTARGETPROPERTY
WdfIoTargetAllocAndQueryTargetPropertyTableIndex(
    WDFIOTARGET IoTarget,
    DEVICE_REGISTRY_PROPERTY DeviceProperty,
    POOL_TYPE PoolType,
    PWDF_OBJECT_ATTRIBUTES PropertyMemoryAttributes,
    WDFMEMORY* PropertyMemory
    )
{
  return NULL;
}

// _Must_inspect_result_
// _IRQL_requires_max_(PASSIVE_LEVEL)
// PS #640 Consider generalizing return values
NTSTATUS
FORCEINLINE
WdfIoTargetAllocAndQueryTargetProperty(
    // _In_
    WDFIOTARGET IoTarget,
    // _In_
    DEVICE_REGISTRY_PROPERTY DeviceProperty,
    // _In_
    // _Strict_type_match_
    POOL_TYPE PoolType,
    // _In_opt_
    PWDF_OBJECT_ATTRIBUTES PropertyMemoryAttributes,
    // _Out_
    WDFMEMORY* PropertyMemory
    )
{
    // return ((PFN_WDFIOTARGETALLOCANDQUERYTARGETPROPERTY) WdfFunctions[WdfIoTargetAllocAndQueryTargetPropertyTableIndex])(WdfDriverGlobals, IoTarget, DeviceProperty, PoolType, PropertyMemoryAttributes, PropertyMemory);
  SLAyer_nondetT(NTSTATUS);
}

// line 667
typedef
//_IRQL_requires_max_(DISPATCH_LEVEL)
WDFAPI
PDEVICE_OBJECT
(*PFN_WDFIOTARGETWDMGETTARGETDEVICEOBJECT)(
    //_In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
    //_In_
    WDFIOTARGET IoTarget
    );
/* reimplementing myself. this function is called by
 * WdfIoTargetWdmGetTargetDeviceObject, but the WDF implements this
 * function using an unknown array. --KK #PS637
 */
//_IRQL_requires_max_(DISPATCH_LEVEL)
PDEVICE_OBJECT
FORCEINLINE
WdfIoTargetWdmGetTargetDeviceObject(
    //_In_
    WDFIOTARGET IoTarget
    )
{
    //return ((PFN_WDFIOTARGETWDMGETTARGETDEVICEOBJECT) WdfFunctions[WdfIoTargetWdmGetTargetDeviceObjectTableIndex])(WdfDriverGlobals, IoTarget);
    return NULL;
}

// line 814
typedef
// _Must_inspect_result_
// _IRQL_requires_max_(DISPATCH_LEVEL)
WDFAPI
NTSTATUS
(*PFN_WDFIOTARGETFORMATREQUESTFORREAD)(
    // _In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
    // _In_
    WDFIOTARGET IoTarget,
    // _In_
    WDFREQUEST Request,
    // _In_opt_
    WDFMEMORY OutputBuffer,
    // _In_opt_
    PWDFMEMORY_OFFSET OutputBufferOffset,
    // _In_opt_
    PLONGLONG DeviceOffset
    );

/* reimplementing myself. This function is called from
 * WdfIotargetFormatRequestForRead, but the WDF implements this function
 * using an unknown array. --KK #PS637
 */
// _Must_inspect_result_
// _IRQL_requires_max_(DISPATCH_LEVEL)
NTSTATUS
FORCEINLINE
WdfIoTargetFormatRequestForRead(
    // _In_
    WDFIOTARGET IoTarget,
    // _In_
    WDFREQUEST Request,
    // _In_opt_
    WDFMEMORY OutputBuffer,
    // _In_opt_
    PWDFMEMORY_OFFSET OutputBufferOffset,
    // _In_opt_
    PLONGLONG DeviceOffset
    )
{
    // return ((PFN_WDFIOTARGETFORMATREQUESTFORREAD) WdfFunctions[WdfIoTargetFormatRequestForReadTableIndex])(WdfDriverGlobals, IoTarget, Request, OutputBuffer, OutputBufferOffset, DeviceOffset);
    NTSTATUS status;
    return status;
}

/******************************************************************************
 * File: wdfio.h
 ******************************************************************************/

typedef enum _WDF_IO_QUEUE_DISPATCH_TYPE {
    WdfIoQueueDispatchInvalid = 0,
    WdfIoQueueDispatchSequential,
    WdfIoQueueDispatchParallel,
    WdfIoQueueDispatchManual,
    WdfIoQueueDispatchMax,
} WDF_IO_QUEUE_DISPATCH_TYPE;

//
// Event callback definitions
//

typedef
/* _Function_class_(EVT_WDF_IO_QUEUE_IO_DEFAULT) */
/* _IRQL_requires_same_ */
/* _IRQL_requires_max_(DISPATCH_LEVEL) */
VOID
EVT_WDF_IO_QUEUE_IO_DEFAULT(
/*     _In_ */
    WDFQUEUE Queue,
/*     _In_ */
    WDFREQUEST Request
    );

typedef EVT_WDF_IO_QUEUE_IO_DEFAULT *PFN_WDF_IO_QUEUE_IO_DEFAULT;

typedef
/* _Function_class_(EVT_WDF_IO_QUEUE_IO_STOP) */
/* _IRQL_requires_same_ */
/* _IRQL_requires_max_(DISPATCH_LEVEL) */
VOID
EVT_WDF_IO_QUEUE_IO_STOP(
/*     _In_ */
    WDFQUEUE Queue,
/*     _In_ */
    WDFREQUEST Request,
/*     _In_ */
    ULONG ActionFlags
    );

typedef EVT_WDF_IO_QUEUE_IO_STOP *PFN_WDF_IO_QUEUE_IO_STOP;

typedef
/* _Function_class_(EVT_WDF_IO_QUEUE_IO_RESUME) */
/* _IRQL_requires_same_ */
/* _IRQL_requires_max_(DISPATCH_LEVEL) */
VOID
EVT_WDF_IO_QUEUE_IO_RESUME(
/*     _In_ */
    WDFQUEUE Queue,
/*     _In_ */
    WDFREQUEST Request
    );

typedef EVT_WDF_IO_QUEUE_IO_RESUME *PFN_WDF_IO_QUEUE_IO_RESUME;

typedef
/* _Function_class_(EVT_WDF_IO_QUEUE_IO_READ) */
/* _IRQL_requires_same_ */
/* _IRQL_requires_max_(DISPATCH_LEVEL) */
VOID
EVT_WDF_IO_QUEUE_IO_READ(
/*     _In_ */
    WDFQUEUE Queue,
/*     _In_ */
    WDFREQUEST Request,
/*     _In_ */
    size_t Length
    );

typedef EVT_WDF_IO_QUEUE_IO_READ *PFN_WDF_IO_QUEUE_IO_READ;

typedef
/* _Function_class_(EVT_WDF_IO_QUEUE_IO_WRITE) */
/* _IRQL_requires_same_ */
/* _IRQL_requires_max_(DISPATCH_LEVEL) */
VOID
EVT_WDF_IO_QUEUE_IO_WRITE(
/*     _In_ */
    WDFQUEUE Queue,
/*     _In_ */
    WDFREQUEST Request,
/*     _In_ */
    size_t Length
    );

typedef EVT_WDF_IO_QUEUE_IO_WRITE *PFN_WDF_IO_QUEUE_IO_WRITE;

typedef
/* _Function_class_(EVT_WDF_IO_QUEUE_IO_DEVICE_CONTROL) */
/* _IRQL_requires_same_ */
/* _IRQL_requires_max_(DISPATCH_LEVEL) */
VOID
EVT_WDF_IO_QUEUE_IO_DEVICE_CONTROL(
/*     _In_ */
    WDFQUEUE Queue,
/*     _In_ */
    WDFREQUEST Request,
/*     _In_ */
    size_t OutputBufferLength,
/*     _In_ */
    size_t InputBufferLength,
/*     _In_ */
    ULONG IoControlCode
    );

typedef EVT_WDF_IO_QUEUE_IO_DEVICE_CONTROL *PFN_WDF_IO_QUEUE_IO_DEVICE_CONTROL;

typedef
/* _Function_class_(EVT_WDF_IO_QUEUE_IO_INTERNAL_DEVICE_CONTROL) */
/* _IRQL_requires_same_ */
/* _IRQL_requires_max_(DISPATCH_LEVEL) */
VOID
EVT_WDF_IO_QUEUE_IO_INTERNAL_DEVICE_CONTROL(
/*     _In_ */
    WDFQUEUE Queue,
/*     _In_ */
    WDFREQUEST Request,
/*     _In_ */
    size_t OutputBufferLength,
/*     _In_ */
    size_t InputBufferLength,
/*     _In_ */
    ULONG IoControlCode
    );

typedef EVT_WDF_IO_QUEUE_IO_INTERNAL_DEVICE_CONTROL *PFN_WDF_IO_QUEUE_IO_INTERNAL_DEVICE_CONTROL;

typedef
/* _Function_class_(EVT_WDF_IO_QUEUE_IO_CANCELED_ON_QUEUE) */
/* _IRQL_requires_same_ */
/* _IRQL_requires_max_(DISPATCH_LEVEL) */
VOID
EVT_WDF_IO_QUEUE_IO_CANCELED_ON_QUEUE(
/*     _In_ */
    WDFQUEUE Queue,
/*     _In_ */
    WDFREQUEST Request
    );

typedef EVT_WDF_IO_QUEUE_IO_CANCELED_ON_QUEUE *PFN_WDF_IO_QUEUE_IO_CANCELED_ON_QUEUE;

typedef
/* _Function_class_(EVT_WDF_IO_QUEUE_STATE) */
/* _IRQL_requires_same_ */
/* _IRQL_requires_max_(DISPATCH_LEVEL) */
VOID
EVT_WDF_IO_QUEUE_STATE(
/*     _In_ */
    WDFQUEUE Queue,
/*     _In_ */
    WDFCONTEXT Context
    );

typedef EVT_WDF_IO_QUEUE_STATE *PFN_WDF_IO_QUEUE_STATE;

// Patch: remove most of the struct (not needed by CromData_trace).
//        And SDV is meant to be able to find fptrs anyway.
typedef struct _WDF_IO_QUEUE_CONFIG {

    ULONG                                       Size;

    WDF_IO_QUEUE_DISPATCH_TYPE                  DispatchType;

/*     WDF_TRI_STATE                               PowerManaged; */

/*     BOOLEAN                                     AllowZeroLengthRequests; */

/*     BOOLEAN                                     DefaultQueue; */

       PFN_WDF_IO_QUEUE_IO_DEFAULT                 EvtIoDefault;

       PFN_WDF_IO_QUEUE_IO_READ                    EvtIoRead;

       PFN_WDF_IO_QUEUE_IO_WRITE                   EvtIoWrite;

  // PFN_WDF_IO_QUEUE_IO_DEVICE_CONTROL
    void* EvtIoDeviceControl;

/*     PFN_WDF_IO_QUEUE_IO_INTERNAL_DEVICE_CONTROL EvtIoInternalDeviceControl; */

    PFN_WDF_IO_QUEUE_IO_STOP                    EvtIoStop;

/*     PFN_WDF_IO_QUEUE_IO_RESUME                  EvtIoResume; */

/*     PFN_WDF_IO_QUEUE_IO_CANCELED_ON_QUEUE       EvtIoCanceledOnQueue; */

    union {
        struct {
            ULONG NumberOfPresentedRequests;
        } Parallel;
    } Settings;

} WDF_IO_QUEUE_CONFIG, *PWDF_IO_QUEUE_CONFIG;

VOID
FORCEINLINE
WDF_IO_QUEUE_CONFIG_INIT_DEFAULT_QUEUE(
    __out PWDF_IO_QUEUE_CONFIG      Config,
    __in WDF_IO_QUEUE_DISPATCH_TYPE DispatchType
    )
{
  // Patch
  //RtlZeroMemory(Config, sizeof(WDF_IO_QUEUE_CONFIG));

    Config->Size = sizeof(WDF_IO_QUEUE_CONFIG);
/*     Config->PowerManaged = WdfUseDefault; */
/*     Config->DefaultQueue = TRUE; */
    Config->DispatchType = DispatchType;
    if (Config->DispatchType == WdfIoQueueDispatchParallel) {
        Config->Settings.Parallel.NumberOfPresentedRequests = (ULONG)-1;
    }
}

// PS #640 Consider generalizing return values
NTSTATUS WdfIoQueueCreate(
  IN  WDFDEVICE Device,
  IN  PWDF_IO_QUEUE_CONFIG Config,
  IN  PWDF_OBJECT_ATTRIBUTES QueueAttributes,
  OUT WDFQUEUE *Queue
)
{
  NTSTATUS status;
  int nondet;

  if (nondet) {
    WDFQUEUE q;
    q = (WDFQUEUE)_SLAyer_malloc(sizeof(SLAyer_WDFOBJECT));
    // Might need to use [Config], [QueueAttributes].
    q->typ = SLAyerWdfQueue;
    q->Parent = Device;
    q->typQueue.Device = Device;
    // Device --> q
    Device->typDevice.Queue = q;
    // returns
    if (NULL != Queue) { *Queue = q; }
    status = STATUS_SUCCESS;
  } else {
    status = STATUS_UNSUCCESSFUL;
  }
  return status;
}

// SLAyer: implemented in specific harnesses.
PDEVICE_OBJECT WdfDeviceWdmGetPhysicalDevice(WDFDEVICE Device);
PDEVICE_OBJECT WdfDeviceWdmGetAttachedDevice(WDFDEVICE Device);

WDFDEVICE WdfIoQueueGetDevice(IN WDFQUEUE Queue)
{
  return Queue->typQueue.Device;
}


/******************************************************************************
 * File: wdffdo.h
 ******************************************************************************/

VOID WdfFdoInitSetFilter(PWDFDEVICE_INIT DeviceInit)
{
}

VOID
WdfFdoLockStaticChildListForIteration(WDFDEVICE Fdo)
{
}

VOID
WdfFdoUnlockStaticChildListFromIteration(WDFDEVICE Fdo)
{
}

/*
  SLAyer: We're assuming that SL_Device_one is the Fdo, and
  SL_Device_two is the Child Pdo. So, AddStatic just skips,
  RetrieveNext returns SL_Device_two.
 */
NTSTATUS WdfFdoAddStaticChild(WDFDEVICE Fdo, WDFDEVICE Child)
{
  int x;
  if (x) { return STATUS_SUCCESS; }
  else { return STATUS_UNSUCCESSFUL; }
}

WDFDEVICE WdfFdoRetrieveNextStaticChild(WDFDEVICE Fdo, WDFDEVICE PreviousChild, ULONG Flags)
{
  return SL_Device_two;
}

VOID
WdfFdoInitSetDefaultChildListConfig(
PWDFDEVICE_INIT DeviceInit,
PWDF_CHILD_LIST_CONFIG      config,
PWDF_OBJECT_ATTRIBUTES DefaultChildListAttributes)
{
}

// PS #644 DeviceInit lifetime
// PS #640 Consider generalizing return values
PWDFDEVICE_INIT WdfPdoInitAllocate(WDFDEVICE ParentDevice)
{
  return &SL_WdfDeviceInit;
}

// PS #640 Consider generalizing return values
NTSTATUS WdfPdoMarkMissing(WDFDEVICE Device)
{
  SLAyer_nondetT(NTSTATUS);
}

VOID WdfPdoRequestEject(WDFDEVICE Device)
{
}

VOID WdfRequestSetInformation(
  /*in*/  WDFREQUEST Request,
  /*in*/  ULONG_PTR Information
)
{
}

NTSTATUS
WdfPdoInitAssignDeviceID(PWDFDEVICE_INIT DeviceInit, PCUNICODE_STRING DeviceID)
{
  SLAyer_nondetT(NTSTATUS);
}

NTSTATUS
WdfPdoInitAddHardwareID(PWDFDEVICE_INIT DeviceInit, PCUNICODE_STRING HardwareID)
{
  SLAyer_nondetT(NTSTATUS);
}

NTSTATUS
WdfPdoInitAddCompatibleID(PWDFDEVICE_INIT DeviceInit, PCUNICODE_STRING CompatibleID)
{
  SLAyer_nondetT(NTSTATUS);
}

NTSTATUS
WdfPdoInitAssignInstanceID(PWDFDEVICE_INIT DeviceInit, PCUNICODE_STRING InstanceID)
{
  SLAyer_nondetT(NTSTATUS);
}

NTSTATUS
WdfPdoInitAddDeviceText(PWDFDEVICE_INIT DeviceInit, PCUNICODE_STRING Desc, PCUNICODE_STRING Loc, int LocalId)
{
  SLAyer_nondetT(NTSTATUS);
}

VOID
WdfPdoInitSetDefaultLocale(PWDFDEVICE_INIT DeviceInit, ULONG locale) {}

WDFCHILDLIST
WdfFdoGetDefaultChildList(WDFDEVICE Fdo) {}

//line 188
// _Must_inspect_result_
// _IRQL_requires_max_(PASSIVE_LEVEL)
NTSTATUS
FORCEINLINE
WdfFdoInitQueryProperty(
    // _In_
    PWDFDEVICE_INIT DeviceInit,
    // _In_
    DEVICE_REGISTRY_PROPERTY DeviceProperty,
    // _In_
    ULONG BufferLength,
    // _Out_writes_bytes_all_opt_(BufferLength)
    PVOID PropertyBuffer,
    // _Out_
    PULONG ResultLength
    )
{
   // return ((PFN_WDFFDOINITQUERYPROPERTY) WdfFunctions[WdfFdoInitQueryPropertyTableIndex])(WdfDriverGlobals, DeviceInit, DeviceProperty, BufferLength, PropertyBuffer, ResultLength);
  NTSTATUS status;
  return status;
}

// line 339
typedef
// _Must_inspect_result_
// _IRQL_requires_max_(PASSIVE_LEVEL)
WDFAPI
NTSTATUS
(*PFN_WDFFDOQUERYFORINTERFACE)(
    // _In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
    // _In_
    WDFDEVICE Fdo,
    // _In_
    LPCGUID InterfaceType,
    // _Out_
    PINTERFACE Interface,
    // _In_
    USHORT Size,
    // _In_
    USHORT Version,
    // _In_opt_
    PVOID InterfaceSpecificData
    );

/* PS#637 -- KK */
// _Must_inspect_result_
//_IRQL_requires_max_(PASSIVE_LEVEL)
NTSTATUS
FORCEINLINE
WdfFdoQueryForInterface(
    // _In_
    WDFDEVICE Fdo,
    // _In_
    LPCGUID InterfaceType,
    // _Out_
    PINTERFACE Interface,
    // _In_
    USHORT Size,
    // _In_
    USHORT Version,
    // _In_opt_
    PVOID InterfaceSpecificData
    )
{
    // return ((PFN_WDFFDOQUERYFORINTERFACE) WdfFunctions[WdfFdoQueryForInterfaceTableIndex])(WdfDriverGlobals, Fdo, InterfaceType, Interface, Size, Version, InterfaceSpecificData);
    NTSTATUS status;
    return status;
}

/******************************************************************************
 * File: wdfwmi.h
 ******************************************************************************/
 //copied from line 30
 typedef enum _WDF_WMI_PROVIDER_CONTROL {
    WdfWmiControlInvalid = 0,
    WdfWmiEventControl,
    WdfWmiInstanceControl,
} WDF_WMI_PROVIDER_CONTROL;

// copied from line 54
typedef enum _WDF_WMI_PROVIDER_FLAGS {
    WdfWmiProviderEventOnly = 0x0001,
    WdfWmiProviderExpensive = 0x0002,
    WdfWmiProviderTracing =   0x0004,
    WdfWmiProviderValidFlags = WdfWmiProviderEventOnly | WdfWmiProviderExpensive | WdfWmiProviderTracing,
} WDF_WMI_PROVIDER_FLAGS;

//copied from line 57

typedef
NTSTATUS
(*PFN_WDF_WMI_INSTANCE_QUERY_INSTANCE)(
    IN WDFWMIINSTANCE WmiInstance,
    IN ULONG OutBufferSize,
    IN PVOID OutBuffer,
    OUT PULONG BufferUsed
    );

typedef
NTSTATUS
(*PFN_WDF_WMI_INSTANCE_SET_INSTANCE)(
    IN WDFWMIINSTANCE WmiInstance,
    IN ULONG InBufferSize,
    IN PVOID InBuffer
    );

typedef
NTSTATUS
(*PFN_WDF_WMI_INSTANCE_SET_ITEM)(
    IN WDFWMIINSTANCE WmiInstance,
    IN ULONG DataItemId,
    IN ULONG InBufferSize,
    IN PVOID InBuffer
    );

typedef
NTSTATUS
(*PFN_WDF_WMI_INSTANCE_EXECUTE_METHOD)(
    IN WDFWMIINSTANCE WmiInstance,
    IN ULONG MethodId,
    IN ULONG InBufferSize,
    IN ULONG OutBufferSize,
    IN OUT PVOID Buffer,
    OUT PULONG BufferUsed
    );

 //copied from wdf/inc/kmdf/1.7/wdfwmi.h, line 68

 typedef
NTSTATUS
(EVT_WDF_WMI_INSTANCE_QUERY_INSTANCE)(
    IN WDFWMIINSTANCE WmiInstance,
    IN ULONG OutBufferSize,
    IN PVOID OutBuffer,
    OUT PULONG BufferUsed
    );

 typedef
NTSTATUS
(EVT_WDF_WMI_INSTANCE_SET_INSTANCE)(
    IN WDFWMIINSTANCE WmiInstance,
    IN ULONG InBufferSize,
    IN PVOID InBuffer
    );

 typedef
NTSTATUS
(EVT_WDF_WMI_INSTANCE_SET_ITEM)(
    IN WDFWMIINSTANCE WmiInstance,
    IN ULONG DataItemId,
    IN ULONG InBufferSize,
    IN PVOID InBuffer
    );

 //copied from line 94

 typedef
NTSTATUS
(*PFN_WDF_WMI_PROVIDER_FUNCTION_CONTROL)(
    IN WDFWMIPROVIDER WmiProvider,
    IN WDF_WMI_PROVIDER_CONTROL Control,
    IN BOOLEAN Enable
    );

 //copied from line 102

typedef struct _WDF_WMI_PROVIDER_CONFIG {
    //
    // Size of this structure in bytes
    //
    ULONG Size;

    //
    // The GUID being registered
    //
    GUID Guid;

    //
    // Combination of values from the enum WDF_WMI_PROVIDER_FLAGS
    //
    ULONG Flags;

    //
    // Minimum expected buffer size for query and set instance requests.
    // Ignored if WdfWmiProviderEventOnly is set in Flags.
    //
    ULONG MinInstanceBufferSize;

    //
    // Callback when caller is opening a provider which ha been marked as
    // expensive or when a caller is interested in events.
    //
    PFN_WDF_WMI_PROVIDER_FUNCTION_CONTROL EvtWmiProviderFunctionControl;

} WDF_WMI_PROVIDER_CONFIG, *PWDF_WMI_PROVIDER_CONFIG;

// copied from line 115
typedef
NTSTATUS
EVT_WDF_WMI_INSTANCE_EXECUTE_METHOD(
    _In_
    WDFWMIINSTANCE WmiInstance,
    _In_
    ULONG MethodId,
    _In_
    ULONG InBufferSize,
    _In_
    ULONG OutBufferSize,
    //_When_(InBufferSize >= OutBufferSize, _Inout_updates_bytes_(InBufferSize))
    //_When_(InBufferSize < OutBufferSize, _Inout_updates_bytes_(OutBufferSize))
    PVOID Buffer,
    _Out_
    PULONG BufferUsed
    );

typedef EVT_WDF_WMI_INSTANCE_EXECUTE_METHOD *PFN_WDF_WMI_INSTANCE_EXECUTE_METHOD;

//copied from line 145

typedef struct _WDF_WMI_INSTANCE_CONFIG {
    //
    // Size of the structure in bytes
    //
    ULONG Size;

    //
    // Optional parameter.  If NULL, ProviderConfig must be set to a valid pointer
    // value.   If specified, indicates the provider to create an instance for.
    //
    WDFWMIPROVIDER Provider;

    //
    // Optional parameter.  If NULL, Provider must be set to a valid handle
    // value.  If specifeid, indicates the configuration for a provider to be
    // created and for this instance to be associated with.
    //
    PWDF_WMI_PROVIDER_CONFIG ProviderConfig;

    //
    // If the Provider is configured as read only and this field is set to TRUE,
    // the EvtWmiInstanceQueryInstance is ignored and WDF will blindly copy the
 // context associated with this instance (using RtlCopyMemory, with no locks
    // held) into the query buffer.
    //
    BOOLEAN UseContextForQuery;

    //
    // If TRUE, the instance will be registered as well as created.
    //
    BOOLEAN Register;

    //
    // Callback when caller wants to query the entire data item's buffer.
    //
    PFN_WDF_WMI_INSTANCE_QUERY_INSTANCE EvtWmiInstanceQueryInstance;

    //
    // Callback when caller wants to set the entire data item's buffer.
    //
    PFN_WDF_WMI_INSTANCE_SET_INSTANCE EvtWmiInstanceSetInstance;

    //
    // Callback when caller wants to set a single field in the data item's buffer
    //
    PFN_WDF_WMI_INSTANCE_SET_ITEM EvtWmiInstanceSetItem;

    //
    // Callback when caller wants to execute a method on the data item.
    //
    PFN_WDF_WMI_INSTANCE_EXECUTE_METHOD EvtWmiInstanceExecuteMethod;

} WDF_WMI_INSTANCE_CONFIG, *PWDF_WMI_INSTANCE_CONFIG;

// SLayer: implemented in specific harnesses.
WDFDEVICE WdfWmiInstanceGetDevice( /* [in] */  WDFWMIINSTANCE WmiInstance);

VOID WDF_WMI_PROVIDER_CONFIG_INIT(
  _Out_  PWDF_WMI_PROVIDER_CONFIG Config,
  _In_   const GUID *Guid
)
{
}

// PS #632
 #define WDF_PTR_ADD_OFFSET_TYPE(_ptr, _offset, _type) \
  (_ptr)

 #define WDF_PTR_ADD_OFFSET(_ptr, _offset) \
         WDF_PTR_ADD_OFFSET_TYPE(_ptr, _offset, PVOID)

// Line 277
NTSTATUS
FORCEINLINE
WDF_WMI_BUFFER_APPEND_STRING(
    /*_Out_writes_bytes_(BufferLength)*/ PVOID Buffer,
    /*_In_ */ ULONG BufferLength,
    /*_In_ */ PCUNICODE_STRING String,
    /*_Out_*/ PULONG RequiredSize
    )
{
    NTSTATUS status;
    return status;
  /*
  NTSTATUS status;
  return status;
    //
    // Compute the length of buffer we need to use.  Upon error the caller can
    // use this length to report the required length.  On success, the caller
    // can use this length to know how many bytes were written.
    //
    //RequiredSize = String->Length + sizeof(USHORT);
    //
    // UNICODE_STRING.Length is the length of the string in bytes, not characters
    //

    // First check to see if there is enough space
    // 1)  to store the length of the string
    // 2)  to store the string itself
    //
    if (BufferLength < (String->Length + sizeof(USHORT))) {
        //
        // Not enough room in the string, report back how big a buffer is
        // required.
        //
        return STATUS_BUFFER_TOO_SMALL;
    }

    //
    // Store the length of the string
    //
    *(USHORT *) Buffer = String->Length;

    //
    // Copy the string to the buffer
    //
    RtlCopyMemory(WDF_PTR_ADD_OFFSET(Buffer, sizeof(USHORT)),
                  String->Buffer,
                  String->Length);
  */
    //return STATUS_SUCCESS;
}

VOID WDF_WMI_INSTANCE_CONFIG_INIT_PROVIDER_CONFIG(
  _Out_  PWDF_WMI_INSTANCE_CONFIG Config,
  _In_   PWDF_WMI_PROVIDER_CONFIG ProviderConfig
)
{
}

// PS #640 Consider generalizing return values
NTSTATUS WdfWmiInstanceCreate(
  /*[in]*/             WDFDEVICE Device,
  /*[in]*/             PWDF_WMI_INSTANCE_CONFIG InstanceConfig,
  /*[in, optional]*/   PWDF_OBJECT_ATTRIBUTES InstanceAttributes,
  /*[out, optional]*/  WDFWMIINSTANCE *Instance
)
{
  int nondet;
  NTSTATUS status;

  if (nondet) {
    WDFWMIINSTANCE w;
    w = (WDFWMIINSTANCE)_SLAyer_malloc(sizeof(SLAyer_WDFOBJECT));
    w->Context =
      (InstanceAttributes == WDF_NO_OBJECT_ATTRIBUTES) ? NULL :
      (*(InstanceAttributes->MkContext))() ;
    w->typ = SLAyerWdfWmiInstance;
    w->Parent = Device;
    w->typWmiInstance.Device = Device;
    // Device --> w. We have space for two [w]s.
    if (Device->typDevice.WmiInstance1 == NULL) {
      Device->typDevice.WmiInstance1 = w;
    } else if (Device->typDevice.WmiInstance2 == NULL) {
      Device->typDevice.WmiInstance2 = w;
    } else if (Device->typDevice.WmiInstance3 == NULL) {
      Device->typDevice.WmiInstance3 = w;
    } else {
      // SLAyer: Need to return UNSUCCESSFUL and clean up mallocs.
    }
    // returns
    if (NULL != Instance) { *Instance = w; }
    status = STATUS_SUCCESS;
  } else {
    status = STATUS_UNSUCCESSFUL;
  }
  return status;
}

WDFDEVICE WdfWmiInstanceGetDevice( /* [in] */  WDFWMIINSTANCE WmiInstance)
{
  return WmiInstance->typWmiInstance.Device;
}

/******************************************************************************
 * File: wdfregistry.h
 ******************************************************************************/
NTSTATUS WdfRegistryQueryULong(
  /*[in]*/   WDFKEY Key,
  /*[in]*/   PCUNICODE_STRING ValueName,
  /*[out]*/  PULONG Value
)
{
  SLAyer_nondetT(NTSTATUS);
}

VOID WdfRegistryClose(
  /* [in] */  WDFKEY Key
)
{
}

/******************************************************************************
 * File: wdfqueryinterface.h
 ******************************************************************************/
//copied from line 32

typedef
NTSTATUS
(EVT_WDF_DEVICE_PROCESS_QUERY_INTERFACE_REQUEST)(
    IN WDFDEVICE Device,
    IN LPGUID InterfaceType,
    IN OUT PINTERFACE ExposedInterface,
    IN OUT PVOID ExposedInterfaceSpecificData
    );

typedef EVT_WDF_DEVICE_PROCESS_QUERY_INTERFACE_REQUEST *PFN_WDF_DEVICE_PROCESS_QUERY_INTERFACE_REQUEST;

 //copied from line 43

 typedef struct _WDF_QUERY_INTERFACE_CONFIG {
    //
    // Size of this structure in bytes.
    //
    ULONG Size;

    //
    // Interface to be returned to the caller.  Optional if BehaviorType is set
    // to WdfQueryInterfaceTypePassThrough or ImportInterface is set to TRUE.
    //
    PINTERFACE Interface;

    //
    // The GUID identifying the interface
    //
    const GUID * InterfaceType;

    //
    // Valid only for PDOs.  The framework will allocate a new request and
    // forward it down the parent's device stack.
    //
    BOOLEAN SendQueryToParentStack;

    //
    // Driver supplied callback which is called after some basic interface
    // validation has been performed (size, version, and guid checking).  This
    // is an optional parameter and may be NULL unless ImportInterface is
    // specified.
    //
    // If the callback returns !NT_SUCCESS, this error will be returned to the
    // caller and the query interface will fail.
    //
    // In this callback, the caller is free to modify the ExposedInterface in
    // any manner of its choosing.  For instance, the callback may change any
    // field in the interface.  The callback may also alloate a dynamic context
    // to be associated with the interface; the InterfaceReference and
    // InterfaceDereference functions may also be overridden.
    //
    // If ImportInterface is set to TRUE, then this is a required field and the
    // callback must initialize the interface (the framework will leave the
    // ExposedInterface buffer exactly as it received it) since the framework
    // has no way of knowing which fields to fill in and which to leave alone.
    //
    PFN_WDF_DEVICE_PROCESS_QUERY_INTERFACE_REQUEST EvtDeviceProcessQueryInterfaceRequest;

    //
    // If TRUE, the interface provided by the caller contains data that the
    // driver is interested in.  By setting to this field to TRUE, the
    // EvtDeviceProcessQueryInterfaceRequest callback must initialize the
    // ExposedInterface.
    //
    // If FALSE, the entire ExposedInterface is initialized through a memory
    // copy before the EvtDeviceProcessQueryInterfaceRequest is called.
    //
    BOOLEAN ImportInterface;

} WDF_QUERY_INTERFACE_CONFIG, *PWDF_QUERY_INTERFACE_CONFIG;

VOID
FORCEINLINE
WdfDeviceInterfaceReferenceNoOp(
    PVOID Context
    )
{
    UNREFERENCED_PARAMETER(Context);
}

VOID
FORCEINLINE
WdfDeviceInterfaceDereferenceNoOp(
    PVOID Context
    )
{
    UNREFERENCED_PARAMETER(Context);
}

VOID WDF_QUERY_INTERFACE_CONFIG_INIT
(
 PWDF_QUERY_INTERFACE_CONFIG InterfaceConfig,
 PINTERFACE Inteface,
 const GUID *InterfaceType,
 PFN_WDF_DEVICE_PROCESS_QUERY_INTERFACE_REQUEST EvtDeviceProcessQueryInterfaceRequest)
{
}

NTSTATUS WdfDeviceAddQueryInterface(WDFDEVICE Device,
                                    PWDF_QUERY_INTERFACE_CONFIG InterfaceConfig)
{
  SLAyer_nondetT(NTSTATUS);
}

/******************************************************************************
 * File: wdfroletypes.h.
 ******************************************************************************/
typedef EVT_WDF_OBJECT_CONTEXT_CLEANUP EVT_WDF_DEVICE_CONTEXT_CLEANUP;

/******************************************************************************
 * File: wudfwdm.h
 ******************************************************************************/
// Line 203
#define CmResourceTypePort                1   // ResType_IO (0x0002)
#define CmResourceTypeInterrupt           2   // ResType_IRQ (0x0004)
#define CmResourceTypeMemory              3   // ResType_Mem (0x0001)

/*****************************************************************************
 * File: wmistr.h
 ****************************************************************************/

/* KK -- Apparently this is wrong, LARGE_INTEGER is a structured type.
 * See the implementation in shared/WTypesbase.h, I've copied it into
 * this file.
 */
// DECLARE_HANDLE(LARGE_INTEGER);

// Copied from line 56
#define WNODE_FLAG_ALL_DATA        0x00000001 // set for WNODE_ALL_DATA
#define WNODE_FLAG_SINGLE_INSTANCE 0x00000002 // set for WNODE_SINGLE_INSTANCE
#define WNODE_FLAG_SINGLE_ITEM     0x00000004 // set for WNODE_SINGLE_ITEM
#define WNODE_FLAG_EVENT_ITEM      0x00000008 // set for WNODE_EVENT_ITEM

#define WMIGUID_NOTIFICATION          0x0004

// Copied from line 144
typedef struct _WNODE_HEADER
{
    ULONG BufferSize;        // Size of entire buffer inclusive of this ULONG
    ULONG ProviderId;    // Provider Id of driver returning this buffer
/*    union
    {
        //ULONG64 HistoricalContext;  // Logger use
        struct
            { */
            ULONG Version;           // Reserved
/*
            ULONG Linkage;           // Linkage field reserved for WMI
        } DUMMYSTRUCTNAME;
    } DUMMYUNIONNAME;
    union
    {
        ULONG CountLost;         // Reserved
        HANDLE KernelHandle;     // Kernel handle for data block
*/
        LARGE_INTEGER TimeStamp; // Timestamp as returned in units of 100ns
/*
                                 // since 1/1/1601
    } DUMMYUNIONNAME2;
*/
    GUID Guid;                  // Guid for data block returned with results
/*
    ULONG ClientContext;
*/
    ULONG Flags;             // Flags, see below
} WNODE_HEADER, *PWNODE_HEADER;

typedef struct tagWNODE_SINGLE_INSTANCE
{
    struct _WNODE_HEADER WnodeHeader;

                            // Offset from beginning of WNODE_SINGLE_INSTANCE
                            // to instance name. Use when
                            // WNODE_FLAG_STATIC_INSTANCE_NAMES is reset
                            // (Dynamic instance names)
    ULONG OffsetInstanceName;

                            // Instance index when
                            // WNODE_FLAG_STATIC_INSTANCE_NAME is set
    ULONG InstanceIndex;    // (Static Instance Names)

    ULONG DataBlockOffset;  // offset from beginning of WNODE to data block
    ULONG SizeDataBlock;    // Size of data block for instance

    UCHAR VariableData[];
    // instance names and padding so data block begins on 8 byte boundry

    // data block
} WNODE_SINGLE_INSTANCE, *PWNODE_SINGLE_INSTANCE;

/******************************************************************************
 * File: 1394.h (DDK?)
 ******************************************************************************/

// common.h ???
// Patch: GUIDs.
//#define LPGUID int*
// {C459DF55-DB08-11d1-B009-00A0C9081FF6}
LPGUID GUID_1394DIAG ;
// {737613E5-69EA-4b96-9C2A-EEBC220F4C39}
LPGUID GUID_1394VDEV ;

// 1394.h, line 700.
#define REQUEST_ISOCH_FREE_RESOURCES            10

// 1394.h, line 721.
#define REQUEST_SET_LOCAL_HOST_PROPERTIES       31

// 1394.h, line 1060.
typedef struct _IRB_REQ_ISOCH_FREE_RESOURCES {
    HANDLE              hResource;          // Resource handle
} IRB_REQ_ISOCH_FREE_RESOURCES;

// 1394.h, line 1251.
typedef struct _IRB_REQ_SET_LOCAL_HOST_PROPERTIES {
    ULONG           nLevel;
    PVOID           Information;
} IRB_REQ_SET_LOCAL_HOST_PROPERTIES;

// 1394.h, line 1409.
#define IRB_BUS_RESERVED_SZ                     8
#define IRB_PORT_RESERVED_SZ                    8

typedef struct _IRB {

    //
    // Holds the zero based Function number that corresponds to the request
    // that device drivers are asking the 1394 Bus driver to carry out.
    //

    ULONG           FunctionNumber;

    //
    // Holds Flags that may be unique to this particular operation.
    //

    ULONG           Flags;

    //
    // Reserved for internal bus driver use and/or future expansion.
    //

    ULONG_PTR       BusReserved[IRB_BUS_RESERVED_SZ];

    //
    // Reserved for internal port driver usage.
    //

    ULONG_PTR       PortReserved[IRB_PORT_RESERVED_SZ];

    //
    // Holds the structures used in performing the various 1394 APIs.
    //

    union {

/*         IRB_REQ_ASYNC_READ                              AsyncRead; */
/*         IRB_REQ_ASYNC_WRITE                             AsyncWrite; */
/*         IRB_REQ_ASYNC_LOCK                              AsyncLock; */
/*         IRB_REQ_ISOCH_ALLOCATE_BANDWIDTH                IsochAllocateBandwidth; */
/*         IRB_REQ_ISOCH_ALLOCATE_CHANNEL                  IsochAllocateChannel; */
/*         IRB_REQ_ISOCH_ALLOCATE_RESOURCES                IsochAllocateResources; */
/*         IRB_REQ_ISOCH_ATTACH_BUFFERS                    IsochAttachBuffers; */
/*         IRB_REQ_ISOCH_DETACH_BUFFERS                    IsochDetachBuffers; */
/*         IRB_REQ_ISOCH_FREE_BANDWIDTH                    IsochFreeBandwidth; */
/*         IRB_REQ_ISOCH_FREE_CHANNEL                      IsochFreeChannel; */
        IRB_REQ_ISOCH_FREE_RESOURCES                    IsochFreeResources;
/*         IRB_REQ_ISOCH_LISTEN                            IsochListen; */
/*         IRB_REQ_ISOCH_QUERY_CURRENT_CYCLE_TIME          IsochQueryCurrentCycleTime; */
/*         IRB_REQ_ISOCH_QUERY_RESOURCES                   IsochQueryResources; */
/*         IRB_REQ_ISOCH_SET_CHANNEL_BANDWIDTH             IsochSetChannelBandwidth; */
/*         IRB_REQ_ISOCH_STOP                              IsochStop; */
/*         IRB_REQ_ISOCH_TALK                              IsochTalk; */
/* #if(NTDDI_VERSION >= NTDDI_WINXP) */
/*         IRB_REQ_ISOCH_MODIFY_STREAM_PROPERTIES          IsochModifyStreamProperties; */
/* #endif */
/*         IRB_REQ_ALLOCATE_ADDRESS_RANGE                  AllocateAddressRange; */
/*         IRB_REQ_FREE_ADDRESS_RANGE                      FreeAddressRange; */
/*         IRB_REQ_GET_LOCAL_HOST_INFORMATION              GetLocalHostInformation; */
/*         IRB_REQ_GET_1394_ADDRESS_FROM_DEVICE_OBJECT     Get1394AddressFromDeviceObject; */
/*         IRB_REQ_CONTROL                                 Control; */
/*         IRB_REQ_GET_MAX_SPEED_BETWEEN_DEVICES           GetMaxSpeedBetweenDevices; */
/*         IRB_REQ_SET_DEVICE_XMIT_PROPERTIES              SetDeviceXmitProperties; */
        IRB_REQ_SET_LOCAL_HOST_PROPERTIES               SetLocalHostProperties;
/*         IRB_REQ_GET_CONFIGURATION_INFORMATION           GetConfigurationInformation; */
/* #if(NTDDI_VERSION >= NTDDI_WIN7) */
/*         IRB_REQ_GET_CONFIG_ROM                          GetConfigRom; */
/* #endif */
/*         IRB_REQ_BUS_RESET                               BusReset; */
/*         IRB_REQ_GET_GENERATION_COUNT                    GetGenerationCount; */
/*         IRB_REQ_SEND_PHY_CONFIGURATION_PACKET           SendPhyConfigurationPacket; */
/* #if(NTDDI_VERSION >= NTDDI_WIN7) */
/*         IRB_REQ_SEND_PHY_PACKET                         SendPhyPacket; */
/*         IRB_REQ_RECEIVE_PHY_PACKETS                     ReceivePhyPackets; */
/* #endif */
/*         IRB_REQ_GET_SPEED_TOPOLOGY_MAPS                 GetSpeedTopologyMaps; */
/*         IRB_REQ_BUS_RESET_NOTIFICATION                  BusResetNotification; */
/*         IRB_REQ_ASYNC_STREAM                            AsyncStream; */

    } u;

} IRB, *PIRB;

// 1394.h, line 1503
#define SET_LOCAL_HOST_PROPERTIES_NO_CYCLE_STARTS       0x00000001
#if(NTDDI_VERSION >= NTDDI_WINXP)
#define SET_LOCAL_HOST_PROPERTIES_CYCLE_START_CONTROL   0x00000001
#endif
#define SET_LOCAL_HOST_PROPERTIES_GAP_COUNT             0x00000002
#define SET_LOCAL_HOST_PROPERTIES_MODIFY_CROM           0x00000003
#if(NTDDI_VERSION >= NTDDI_WINXP)
#define SET_LOCAL_HOST_PROPERTIES_MAX_PAYLOAD           0x00000004
#endif
#if(NTDDI_VERSION >= NTDDI_VISTA)
#define SET_LOCAL_HOST_PROPERTIES_DEBUG_ENTRY           0x00000005
#endif

#if(NTDDI_VERSION >= NTDDI_WINXP)
 typedef struct _SET_LOCAL_HOST_PROPS1 {
    ULONG       fulFlags;
} SET_LOCAL_HOST_PROPS1, *PSET_LOCAL_HOST_PROPS1;
#endif

typedef struct _SET_LOCAL_HOST_PROPS2 {
    ULONG       GapCountLowerBound;
} SET_LOCAL_HOST_PROPS2, *PSET_LOCAL_HOST_PROPS2;

// 1394.h, line 1542.
typedef struct _SET_LOCAL_HOST_PROPS3 {

    ULONG       fulFlags;
    HANDLE      hCromData;
    ULONG       nLength;
    PMDL        Mdl;

} SET_LOCAL_HOST_PROPS3, *PSET_LOCAL_HOST_PROPS3;

// line 1590.
#define SLHP_FLAG_ADD_CROM_DATA                         0x01
#define SLHP_FLAG_REMOVE_CROM_DATA                      0x02

typedef struct _ADDRESS_RANGE {
  /* USHORT */ int AR_Off_High;
  /* USHORT */ int AR_Length;
  /* ULONG */  int AR_Off_Low;
} ADDRESS_RANGE, *PADDRESS_RANGE;

//
// 1394 Cycle Time format.
//

typedef struct _CYCLE_TIME {
    ULONG               CL_CycleOffset:12;      // Bits 0-11
    ULONG               CL_CycleCount:13;       // Bits 12-24
    ULONG               CL_SecondCount:7;       // Bits 25-31
} CYCLE_TIME, *PCYCLE_TIME;

typedef
//__drv_requiresIRQL(DISPATCH_LEVEL)
void
(*PBUS_ISOCH_DESCRIPTOR_ROUTINE) (
    /*__in*/ PVOID  Context1,
    /*__in*/ PVOID  Context2
    );

//
// Definition of Isoch Descriptor.
//

typedef struct _ISOCH_DESCRIPTOR {

    //
    // Flags (used in synchronization).
    //

    ULONG                                   fulFlags;

/*     // */
/*     // Mdl pointing to buffer. */
/*     // */

/*     PMDL                                    Mdl; */

/*     // */
/*     // Length of combined buffer(s) as represented by the Mdl. */
/*     // */

/*     ULONG                                   ulLength; */

/*     // */
/*     // Payload size of each Isoch packet to be used in this descriptor. */
/*     // */

/*     ULONG                                   nMaxBytesPerFrame; */

/*     // */
/*     // Synchronization field; equivalent to Sy in the Isoch packet. */
/*     // */

/*     ULONG                                   ulSynch; */

/*     // */
/*     // Synchronization field; equivalent to Tag in the Isoch packet. */
/*     // */

/*     ULONG                                   ulTag; */

/*     // */
/*     // Cycle time field; returns time to be sent/received or when finished. */
/*     // */

/*     CYCLE_TIME                              CycleTime; */

/*     // */
/*     // Callback routine (if any) to be called when this descriptor completes. */
/*     // */

/*     PBUS_ISOCH_DESCRIPTOR_ROUTINE           Callback; */

/*     // */
/*     // First context (if any) parameter to be passed when doing callbacks. */
/*     // */

/*     PVOID                                   Context1; */

/*     // */
/*     // Second context (if any) parameter to be passed when doing callbacks. */
/*     // */

/*     PVOID                                   Context2; */

/*     // */
/*     // Holds the final status of this descriptor. */
/*     // */

/*     NTSTATUS                                status; */

/*     // */
/*     // Reserved for the device driver who submitted this descriptor to */
/*     // stomp in. */
/*     // */

/*     ULONG_PTR                               DeviceReserved[8]; */

/*     // */
/*     // Reserved for the bus driver to stomp in. */
/*     // */

/*     ULONG_PTR                               BusReserved[8]; */

/*     // */
/*     // Reserved for the port driver to stomp in. */
/*     // */

/*     ULONG_PTR                               PortReserved[16]; */

} ISOCH_DESCRIPTOR, *PISOCH_DESCRIPTOR;

typedef struct _NODE_DEVICE_EXTENSION {

/*     // */
/*     // Holds Tag to determine if this is really a "Node" Device Extension. */
/*     // */

/*     ULONG Tag; */

/*     // */
/*     // Holds the flag as to whether or not we've read the configuration */
/*     // information out of this device. */
/*     // */

/*     BOOLEAN bConfigurationInformationValid; */

/*     // */
/*     // Holds the Configuration Rom for this device.  Multi-functional */
/*     // devices (i.e. many units) will share this same Config Rom */
/*     // structure, but they are represented as a different Device Object. */
/*     // This is not the entire Config Rom, but does contain the root directory */
/*     // as well as everything in front of it. */
/*     // */

/*     PCONFIG_ROM ConfigRom; */

/*     // */
/*     // Holds the length of the UnitDirectory pointer. */
/*     // */

/*     ULONG UnitDirectoryLength; */

/*     // */
/*     // Holds the Unit Directory for this device.  Even on multi-functional */
/*     // devices (i.e. many units) this should be unique to each Device Object. */
/*     // */

/*     PVOID UnitDirectory; */

/*     // */
/*     // Holds the Unit Directory location for this device.  Only the lower 48 */
/*     // bits are valid in this IO_ADDRESS.  Useful for computing offsets from */
/*     // within the UnitDirectory as all offsets are relative. */
/*     // */

/*     IO_ADDRESS UnitDirectoryLocation; */

/*     // */
/*     // Holds the length of the UnitDependentDirectory pointer. */
/*     // */

/*     ULONG UnitDependentDirectoryLength; */

/*     // */
/*     // Holds the Unit Dependent directory for this device. */
/*     // */

/*     PVOID UnitDependentDirectory; */

/*     // */
/*     // Holds the Unit Dependent Directory location for this device.  Only the */
/*     // lower 48 bits are valid in this IO_ADDRESS.  Useful for computing */
/*     // offsets from within the UnitDependentDirectory as offsets are relative. */
/*     // */

/*     IO_ADDRESS UnitDependentDirectoryLocation; */

/*     // */
/*     // Holds the length of the VendorLeaf pointer. */
/*     // */

/*     ULONG VendorLeafLength; */

/*     // */
/*     // Holds the pointer to the Vendor Leaf information */
/*     // */

/*     PTEXTUAL_LEAF VendorLeaf; */

/*     // */
/*     // Holds the length of the VendorLeaf pointer. */
/*     // */

/*     ULONG ModelLeafLength; */

/*     // */
/*     // Holds the pointer to the Model Leaf information. */
/*     // */

/*     PTEXTUAL_LEAF ModelLeaf; */

/*     // */
/*     // Holds the 1394 10 bit BusId / 6 bit NodeId structure. */
/*     // */

/*     NODE_ADDRESS NodeAddress; */

/*     // */
/*     // Holds the speed to be used in reaching this device. */
/*     // */

/*     UCHAR Speed; */

/*     // */
/*     // Holds the priority at which to send packets. */
/*     // */

/*     UCHAR Priority; */

/*     // */
/*     // Holds the Irp used to notify this device object about events. */
/*     // */

/*     PIRP Irp; */

/*     // */
/*     // Holds the Device Object that this Device Extension hangs off of. */
/*     // */

/*     PDEVICE_OBJECT DeviceObject; */

    //
    // Holds the Port Device Object that this Device hangs off of.
    //

    PDEVICE_OBJECT PortDeviceObject;

/*     // */
/*     // Holds the pointer to corresponding information about this deivce */
/*     // in the bus driver's head. */
/*     // */

/*     PVOID DeviceInformation; */

/*     // */
/*     // Holds the pointer to the bus reset notification routine (if any). */
/*     // */

/*     PBUS_BUS_RESET_NOTIFICATION ResetRoutine; */

/*     // */
/*     // Holds the pointer to the context the client wanted when bus reset occurs. */
/*     // */

/*     PVOID ResetContext; */

} NODE_DEVICE_EXTENSION, *PNODE_DEVICE_EXTENSION;

// inc/1393api.h
typedef struct _SET_LOCAL_HOST_INFORMATION {
    ULONG           nLevel;
    ULONG           ulBufferSize;
    /* UCHAR */char           Information[1];
} SET_LOCAL_HOST_INFORMATION, *PSET_LOCAL_HOST_INFORMATION;

NTSTATUS
WdfStringCreate(PCUNICODE_STRING        original,
		PWDF_OBJECT_ATTRIBUTES a,
		WDFSTRING*            out) {
  int x;
  WDFSTRING s;

  if (x) {
    s = _SLAyer_malloc(sizeof(WDFSTRING));
    *out = s;
    return STATUS_SUCCESS;
  } else {
    return STATUS_INSUFFICIENT_RESOURCES;
  }
}

DECLARE_HANDLE(WDFMEMORY);

// For toasterMof.h
DECLARE_HANDLE(CHAR);

/******************************************************************************
 * File: wdftimer.h
 ******************************************************************************/
typedef
//_Function_class_(EVT_WDF_TIMER)
//_IRQL_requires_same_
//_IRQL_requires_max_(DISPATCH_LEVEL)
VOID
EVT_WDF_TIMER(
    _In_
    WDFTIMER Timer
    );

typedef EVT_WDF_TIMER *PFN_WDF_TIMER;

// line 51
typedef struct _WDF_TIMER_CONFIG {
    ULONG Size;
    PFN_WDF_TIMER EvtTimerFunc;
    ULONG Period;
    BOOLEAN AutomaticSerialization;
    ULONG TolerableDelay;
} WDF_TIMER_CONFIG, *PWDF_TIMER_CONFIG;

VOID
FORCEINLINE
WDF_TIMER_CONFIG_INIT(
    /* _Out_*/ PWDF_TIMER_CONFIG Config,
    /* _In_ */ PFN_WDF_TIMER     EvtTimerFunc
    )
{
    RtlZeroMemory(Config, sizeof(WDF_TIMER_CONFIG));
    Config->Size = sizeof(WDF_TIMER_CONFIG);
    Config->EvtTimerFunc = EvtTimerFunc;
    Config->Period = 0;
    Config->AutomaticSerialization = TRUE;
    Config->TolerableDelay = 0;
}

typedef
/*_Must_inspect_result_ */
/*_IRQL_requires_max_(DISPATCH_LEVEL) */
WDFAPI
NTSTATUS
(*PFN_WDFTIMERCREATE)(
    /*_In_ */
    PWDF_DRIVER_GLOBALS DriverGlobals,
    /*_In_ */
    PWDF_TIMER_CONFIG Config,
    /*_In_ */
    PWDF_OBJECT_ATTRIBUTES Attributes,
    /*_Out_ */
    WDFTIMER* Timer
    );

/*_Must_inspect_result_*/
/*_IRQL_requires_max_(DISPATCH_LEVEL)*/
NTSTATUS
FORCEINLINE
WdfTimerCreate(
    /*_In_*/
    PWDF_TIMER_CONFIG Config,
    /*_In_*/
    PWDF_OBJECT_ATTRIBUTES Attributes,
    /*_Out_*/
    WDFTIMER* Timer
    )
{
  WDFTIMER timer;

  timer = (WDFTIMER)_SLAyer_malloc(sizeof(SLAyer_WDFOBJECT));
  timer->typ = SLAyerWdfTimer;
  timer->Context =
    (Attributes == WDF_NO_OBJECT_ATTRIBUTES) ? NULL :
    (*(Attributes->MkContext))() ;
  SL_Timer = timer;
  *Timer = SL_Timer;

  return STATUS_SUCCESS;
}

typedef
// _IRQL_requires_max_(DISPATCH_LEVEL)
WDFAPI
BOOLEAN
(*PFN_WDFTIMERSTART)(
    // _In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
    // _In_
    WDFTIMER Timer,
    // _In_
    LONGLONG DueTime
    );

// _IRQL_requires_max_(DISPATCH_LEVEL)
BOOLEAN
FORCEINLINE
WdfTimerStart(
    // _In_
    WDFTIMER Timer,
    // _In_
    LONGLONG DueTime
    )
{
    // return ((PFN_WDFTIMERSTART) WdfFunctions[WdfTimerStartTableIndex])(WdfDriverGlobals, Timer, DueTime);
  SLAyer_nondetT(BOOLEAN);
}

typedef
// _When_(Wait == __true, _IRQL_requires_max_(PASSIVE_LEVEL))
// _When_(Wait == __false, _IRQL_requires_max_(DISPATCH_LEVEL))
WDFAPI
BOOLEAN
(*PFN_WDFTIMERSTOP)(
    // _In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
    // _In_
    WDFTIMER Timer,
    // _In_
    BOOLEAN Wait
    );

/* Reimplementing. THe function WdfTimerStop calls this function, but
 * this function is implemented by the WDF using some unknown array. --KK
 * #PS637
 */
// _When_(Wait == __true, _IRQL_requires_max_(PASSIVE_LEVEL))
// _When_(Wait == __false, _IRQL_requires_max_(DISPATCH_LEVEL))
BOOLEAN
FORCEINLINE
WdfTimerStop(
    // _In_
    WDFTIMER Timer,
    // _In_
    BOOLEAN Wait
    )
{
    BOOLEAN b;
    return b;
    // return ((PFN_WDFTIMERSTOP) WdfFunctions[WdfTimerStopTableIndex])(WdfDriverGlobals, Timer, Wait);
}

/******************************************************************************
 * File: wdmguid.h
 ******************************************************************************/
DEFINE_GUID( GUID_DEVICE_INTERFACE_ARRIVAL,        0xcb3a4004L, 0x46f0, 0x11d0, 0xb0, 0x8f, 0x00, 0x60, 0x97, 0x13, 0x05, 0x3f );
DEFINE_GUID( GUID_BUS_INTERFACE_STANDARD,               0x496B8280L, 0x6F25, 0x11D0, 0xBE, 0xAF, 0x08, 0x00, 0x2B, 0xE2, 0x09, 0x2F );

/******************************************************************************
 * File: wdfmemory.h
 ******************************************************************************/
// Line 47
/* I have placed the definition of _WDFMEMORY_OFFSET in File: wdfiotarget.h as
 * it is needed before this point in the file (in the early 3000s of lines, at
 * time of writing) -- KK
 */

// Line 134
typedef
//_Must_inspect_result_
//_When_(PoolType == 1 || PoolType == 257, _IRQL_requires_max_(APC_LEVEL))
//_When_(PoolType == 0 || PoolType == 256, _IRQL_requires_max_(DISPATCH_LEVEL))
WDFAPI
NTSTATUS
(*PFN_WDFMEMORYCREATE)(
    //_In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
    //_In_opt_
    PWDF_OBJECT_ATTRIBUTES Attributes,
    //_In_
    //_Strict_type_match_
    POOL_TYPE PoolType,
    //_In_opt_
    ULONG PoolTag,
    //_In_
    //_When_(BufferSize == 0, __drv_reportError(BufferSize cannot be zero))
    size_t BufferSize,
    //_Out_
    WDFMEMORY* Memory,
    //_Outptr_opt_result_bytebuffer_(BufferSize)
    PVOID* Buffer
    );

/* Creating a stub for this. The function WdfMemoryCreate below calls
 * this using some unknown array. --KK #PS637
 */
PFN_WDFMEMORYCREATE
WdfMemoryCreateTableIndex(
    PWDF_OBJECT_ATTRIBUTES Attributes,
    POOL_TYPE PoolType,
    ULONG PoolTag,
    size_t BufferSize,
    WDFMEMORY* Memory,
    PVOID* Buffer
    )
{
  return NULL;
}

// Line 228
typedef
//_IRQL_requires_max_(DISPATCH_LEVEL)
WDFAPI
PVOID
(*PFN_WDFMEMORYGETBUFFER)(
    //_In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
    //_In_
    WDFMEMORY Memory,
    //_Out_opt_
    size_t* BufferSize
    );

/* Reimplementing myself. This function is called from
 * WdfMemoryGetBuffer, and the WDF implements this function using some
 * unknown array. --KK #PS637
 */
PFN_WDFMEMORYGETBUFFER
WdfMemoryGetBufferTableIndex(
    WDFMEMORY Memory,
    size_t* BufferSize
    )
{
  return NULL;
}

//_IRQL_requires_max_(DISPATCH_LEVEL)
PVOID
FORCEINLINE
WdfMemoryGetBuffer(
    //_In_
    WDFMEMORY Memory,
    //_Out_opt_
    size_t* BufferSize
    )
{
    return WdfMemoryGetBufferTableIndex(Memory, BufferSize);
    //return ((PFN_WDFMEMORYGETBUFFER) WdfFunctions[WdfMemoryGetBufferTableIndex])(WdfDriverGlobals, Memory, BufferSize);
}

//_Must_inspect_result_
//_When_(PoolType == 1 || PoolType == 257, _IRQL_requires_max_(APC_LEVEL))
//_When_(PoolType == 0 || PoolType == 256, _IRQL_requires_max_(DISPATCH_LEVEL))
NTSTATUS
FORCEINLINE
WdfMemoryCreate(
    //_In_opt_
    PWDF_OBJECT_ATTRIBUTES Attributes,
    //_In_
    //_Strict_type_match_
    POOL_TYPE PoolType,
    //_In_opt_
    ULONG PoolTag,
    //_In_
    //_When_(BufferSize == 0, __drv_reportError(BufferSize cannot be zero))
    size_t BufferSize,
    //_Out_
    WDFMEMORY* Memory,
    //_Outptr_opt_result_bytebuffer_(BufferSize)
    PVOID* Buffer
    )
{
    return WdfMemoryCreateTableIndex(Attributes, PoolType, PoolTag,
      BufferSize, Memory, Buffer);
    // return ((PFN_WDFMEMORYCREATE) WdfFunctions[WdfMemoryCreateTableIndex])(WdfDriverGlobals, Attributes, PoolType, PoolTag, BufferSize, Memory, Buffer);
}

// line 422
// _Must_inspect_result_
// _When_(PoolType == 1 || PoolType == 257, _IRQL_requires_max_(APC_LEVEL))
// _When_(PoolType == 0 || PoolType == 256, _IRQL_requires_max_(DISPATCH_LEVEL))
NTSTATUS
FORCEINLINE
WdfLookasideListCreate(
    // _In_opt_
    PWDF_OBJECT_ATTRIBUTES LookasideAttributes,
    // _In_
    // _When_(BufferSize == 0, __drv_reportError(BufferSize cannot be zero))
    size_t BufferSize,
    // _In_
    // _Strict_type_match_
    POOL_TYPE PoolType,
    // _In_opt_
    PWDF_OBJECT_ATTRIBUTES MemoryAttributes,
    // _In_opt_
    ULONG PoolTag,
    // _Out_
    WDFLOOKASIDE* Lookaside
    )
{
  NTSTATUS status;
  return status;
    // return ((PFN_WDFLOOKASIDELISTCREATE) WdfFunctions[WdfLookasideListCreateTableIndex])(WdfDriverGlobals, LookasideAttributes, BufferSize, PoolType, MemoryAttributes, PoolTag, Lookaside);
}

/******************************************************************************
 * File: wdfcollection.h
 ******************************************************************************/

// line 41
typedef
// _Must_inspect_result_
// _IRQL_requires_max_(DISPATCH_LEVEL)
WDFAPI
NTSTATUS
(*PFN_WDFCOLLECTIONCREATE)(
    // _In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
    // _In_opt_
    PWDF_OBJECT_ATTRIBUTES CollectionAttributes,
    // _Out_
    WDFCOLLECTION* Collection
    );

// line 72
typedef
//_IRQL_requires_max_(DISPATCH_LEVEL)
//WDFAPI
ULONG
(*PFN_WDFCOLLECTIONGETCOUNT)(
    //_In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
    //_In_
    WDFCOLLECTION Collection
    );

// line 128
typedef
// _IRQL_requires_max_(DISPATCH_LEVEL)
WDFAPI
VOID
(*PFN_WDFCOLLECTIONREMOVE)(
    // _In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
    // _In_
    WDFCOLLECTION Collection,
    // _In_
    WDFOBJECT Item
    );

/* Reimplementing myself. This function is called from
 * WdfCollectionRemove, but the WDF implements this function using some
 * unknown array. --KK #PS637
 */
// _IRQL_requires_max_(DISPATCH_LEVEL)
VOID
FORCEINLINE
WdfCollectionRemove(
    // _In_
    WDFCOLLECTION Collection,
    // _In_
    WDFOBJECT Item
    )
{
    // ((PFN_WDFCOLLECTIONREMOVE) WdfFunctions[WdfCollectionRemoveTableIndex])(WdfDriverGlobals, Collection, Item);
}

// _Must_inspect_result_
// _IRQL_requires_max_(DISPATCH_LEVEL)
// PS #640 Consider generalizing return values
NTSTATUS
FORCEINLINE
WdfCollectionCreate(
    // _In_opt_
    PWDF_OBJECT_ATTRIBUTES CollectionAttributes,
    // _Out_
    WDFCOLLECTION* Collection
    )
{
  // return ((PFN_WDFCOLLECTIONCREATE) WdfFunctions[WdfCollectionCreateTableIndex])(WdfDriverGlobals, CollectionAttributes, Collection);
  SLAyer_nondetT(NTSTATUS);
}

typedef
/*_Must_inspect_result_*/
/*_IRQL_requires_max_(DISPATCH_LEVEL)*/
WDFAPI
NTSTATUS
(*PFN_WDFCOLLECTIONADD)(
    /*_In_*/
    PWDF_DRIVER_GLOBALS DriverGlobals,
    /*_In_*/
    WDFCOLLECTION Collection,
    /*_In_*/
    WDFOBJECT Object
    );

/* My own implementation of WdfFunctions[WdfCollectionAddTableIndex],
 * since the WDF does not provide one. -- KK #PS637
 */
NTSTATUS
WdfCollectionAddTableIndex(
    WDFCOLLECTION Collection,
    WDFOBJECT Object)
{
  int x;
  if(x){ return STATUS_SUCCESS; }
  else { return STATUS_UNSUCCESSFUL; }
}

// Line 186
typedef
//_IRQL_requires_max_(DISPATCH_LEVEL)
WDFAPI
WDFOBJECT
(*PFN_WDFCOLLECTIONGETITEM)(
    //_In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
    //_In_
    WDFCOLLECTION Collection,
    //_In_
    ULONG Index
    );

PFN_WDFCOLLECTIONGETITEM
WdfCollectionGetItemTableIndex(
    WDFCOLLECTION Collection,
    ULONG Index
    )
{
  return NULL;
}

//_IRQL_requires_max_(DISPATCH_LEVEL)
WDFOBJECT
FORCEINLINE
WdfCollectionGetItem(
    //_In_
    WDFCOLLECTION Collection,
    //_In_
    ULONG Index
    )
{
    return WdfCollectionGetItemTableIndex(Collection, Index);
    //return ((PFN_WDFCOLLECTIONGETITEM) WdfFunctions[WdfCollectionGetItemTableIndex])(WdfDriverGlobals, Collection, Index);
}

/*_Must_inspect_result_*/
/*_IRQL_requires_max_(DISPATCH_LEVEL)*/
// PS #640 Consider generalizing return values
NTSTATUS
FORCEINLINE
WdfCollectionAdd(
    /*_In_*/
    WDFCOLLECTION Collection,
    /*_In_*/
    WDFOBJECT Object
    )
{
    // WDF doesn't provide a WdfFunctions array
    // return ((PFN_WDFCOLLECTIONADD) WdfFunctions[WdfCollectionAddTableIndex])(WdfDriverGlobals, Collection, Object);
    return WdfCollectionAddTableIndex(Collection, Object);
}


ULONG
WdfCollectionGetCount(
    WDFCOLLECTION Collection
    )
{
  return 1;
}


/******************************************************************************
 * File: wdfcore.h
 ******************************************************************************/
#define WDF_TIMEOUT_TO_MS               ((LONGLONG) 1 * 10 * 1000)
#define WDF_TIMEOUT_TO_SEC              ((LONGLONG) 1 * 10 * 1000 * 1000)

WDF_REL_TIMEOUT_IN_SEC(
    /*_In_*/ ULONGLONG Time
    )
{
    return Time * -1 * WDF_TIMEOUT_TO_SEC;
}

LONGLONG
//FORCEINLINE
WDF_REL_TIMEOUT_IN_MS(
    /*_In_*/ ULONGLONG Time
    )
{
    return Time * -1 * WDF_TIMEOUT_TO_MS;
}

/* Moved these up, as they are needed earlier in the file. --KK
 #define WDF_PTR_ADD_OFFSET_TYPE(_ptr, _offset, _type) \
     ((_type) (((PUCHAR) (_ptr)) + (_offset)))

 #define WDF_PTR_ADD_OFFSET(_ptr, _offset) \
         WDF_PTR_ADD_OFFSET_TYPE(_ptr, _offset, PVOID)
*/

/******************************************************************************
 * File: wdfresource.h
 ******************************************************************************/
// line 593
typedef
// _IRQL_requires_max_(DISPATCH_LEVEL)
WDFAPI
ULONG
(*PFN_WDFCMRESOURCELISTGETCOUNT)(
    // _In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
    // _In_
    WDFCMRESLIST List
    );

/* PS#637 --KK */
// _IRQL_requires_max_(DISPATCH_LEVEL)
ULONG
FORCEINLINE
WdfCmResourceListGetCount(
    // _In_
    WDFCMRESLIST List
    )
{
  ULONG ulong;
  return ulong;
    // return ((PFN_WDFCMRESOURCELISTGETCOUNT) WdfFunctions[WdfCmResourceListGetCountTableIndex])(WdfDriverGlobals, List);
}

// line 618
typedef
// _IRQL_requires_max_(DISPATCH_LEVEL)
WDFAPI
PCM_PARTIAL_RESOURCE_DESCRIPTOR
(*PFN_WDFCMRESOURCELISTGETDESCRIPTOR)(
    // _In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
    // _In_
    WDFCMRESLIST List,
    // _In_
    ULONG Index
    );

// _IRQL_requires_max_(DISPATCH_LEVEL)
PCM_PARTIAL_RESOURCE_DESCRIPTOR
FORCEINLINE
WdfCmResourceListGetDescriptor(
    // _In_
    WDFCMRESLIST List,
    // _In_
    ULONG Index
    )
{
    return NULL;
    // return ((PFN_WDFCMRESOURCELISTGETDESCRIPTOR) WdfFunctions[WdfCmResourceListGetDescriptorTableIndex])(WdfDriverGlobals, List, Index);
}

/******************************************************************************
 * File: string.h
 ******************************************************************************/

/* My implementation. I can't find an implementation for this, but I got
 * the header from km/crt/string.h. --KK
 */
size_t
wcslen(
    const wchar_t * _Str
    )
{
  size_t size = 0;
  while(*_Str){
    size++;
    _Str++;
  }
  return size;
}

/******************************************************************************
 * File: string.h
 ******************************************************************************/
// line 189
extern const UNICODE_STRING     SDDL_DEVOBJ_SYS_ALL_ADM_RWX_WORLD_RW_RES_R;

/******************************************************************************
 * File: wdfcontrol.h
 ******************************************************************************/
// PS #644 DeviceInit lifetime
// _Must_inspect_result_
// _IRQL_requires_max_(PASSIVE_LEVEL)
PWDFDEVICE_INIT
FORCEINLINE
WdfControlDeviceInitAllocate(
    // _In_
    WDFDRIVER Driver,
    // _In_
    /*CONST*/ UNICODE_STRING* SDDLString
    )
{
    // return ((PFN_WDFCONTROLDEVICEINITALLOCATE) WdfFunctions[WdfControlDeviceInitAllocateTableIndex])(WdfDriverGlobals, Driver, SDDLString);
  return &SL_WdfDeviceInit;
}

// line 138
// _IRQL_requires_max_(DISPATCH_LEVEL)
VOID
FORCEINLINE
WdfControlFinishInitializing(
    // _In_
    WDFDEVICE Device
    )
{
  return;
    // ((PFN_WDFCONTROLFINISHINITIALIZING) WdfFunctions[WdfControlFinishInitializingTableIndex])(WdfDriverGlobals, Device);
}


/*****************************************************************************
 * File: km/mslldp.h
 ****************************************************************************/

#define ETH_LENGTH_OF_ADDRESS 6

/*****************************************************************************
 * File: xfilter.h
 ****************************************************************************/
#define ETH_IS_MULTICAST(Address) \
    (BOOLEAN)(((PUCHAR)(Address))[0] & ((UCHAR)0x01))

#define ETH_LENGTH_OF_ADDRESS 6

#define ETH_IS_BROADCAST(Address)               \
    ((((PUCHAR)(Address))[0] == ((UCHAR)0xff)) && (((PUCHAR)(Address))[1] == ((UCHAR)0xff)) && (((PUCHAR)(Address))[2] == ((UCHAR)0xff)) && (((PUCHAR)(Address))[3] == ((UCHAR)0xff)) && (((PUCHAR)(Address))[4] == ((UCHAR)0xff)) && (((PUCHAR)(Address))[5] == ((UCHAR)0xff)))

#define ETH_COMPARE_NETWORK_ADDRESSES(_A, _B, _Result)          \
{                                                               \
    if (*(ULONG UNALIGNED *)&(_A)[2] >                          \
         *(ULONG UNALIGNED *)&(_B)[2])                          \
    {                                                           \
        *(_Result) = 1;                                         \
    }                                                           \
    else if (*(ULONG UNALIGNED *)&(_A)[2] <                     \
                *(ULONG UNALIGNED *)&(_B)[2])                   \
    {                                                           \
        *(_Result) = (UINT)-1;                                  \
    }                                                           \
    else if (*(USHORT UNALIGNED *)(_A) >                        \
                *(USHORT UNALIGNED *)(_B))                      \
    {                                                           \
        *(_Result) = 1;                                         \
    }                                                           \
    else if (*(USHORT UNALIGNED *)(_A) <                        \
                *(USHORT UNALIGNED *)(_B))                      \
    {                                                           \
        *(_Result) = (UINT)-1;                                  \
    }                                                           \
    else                                                        \
    {                                                           \
        *(_Result) = 0;                                         \
    }                                                           \
}

/*****************************************************************************
 * File: wdfinterrupt.h
 ****************************************************************************/
// line 66
// This is the function that gets invoked when the hardware ISR occurs.
// This function is called at the IRQL at which the interrupt is serviced:
//  - DIRQL for DIRQL interrupt handling.
//  - PASSIVE_LEVEL for passive-level interrupt handling.
typedef
//_Function_class_(EVT_WDF_INTERRUPT_ISR)
// _IRQL_requires_same_
// _IRQL_requires_min_(PASSIVE_LEVEL)
BOOLEAN
EVT_WDF_INTERRUPT_ISR(
    // _In_
    WDFINTERRUPT Interrupt,
    // _In_
    ULONG MessageID
    );

typedef EVT_WDF_INTERRUPT_ISR *PFN_WDF_INTERRUPT_ISR;

// This is the function that gets called back into the driver
// when the DpcForIsr fires.  It will be called at DISPATCH_LEVEL.
typedef
// _Function_class_(EVT_WDF_INTERRUPT_DPC)
// _IRQL_requires_same_
// _IRQL_requires_(DISPATCH_LEVEL)
VOID
EVT_WDF_INTERRUPT_DPC(
    // _In_
    WDFINTERRUPT Interrupt,
    // _In_
    WDFOBJECT AssociatedObject
    );

typedef EVT_WDF_INTERRUPT_DPC *PFN_WDF_INTERRUPT_DPC;

// This is the function that gets called back into the driver
// to enable the interrupt in the hardware.  It will be called
// at the same IRQL at which the interrupt will be serviced:
//  - DIRQL for DIRQL interrupt handling.
//  - PASSIVE_LEVEL for passive-level interrupt handling.
typedef
// _Function_class_(EVT_WDF_INTERRUPT_ENABLE)
// _IRQL_requires_same_
// _IRQL_requires_min_(PASSIVE_LEVEL)
NTSTATUS
EVT_WDF_INTERRUPT_ENABLE(
    // _In_
    WDFINTERRUPT Interrupt,
    // _In_
    WDFDEVICE AssociatedDevice
    );

typedef EVT_WDF_INTERRUPT_ENABLE *PFN_WDF_INTERRUPT_ENABLE;

// This is the function that gets called back into the driver
// to disable the interrupt in the hardware.  It will be called
// at the same IRQL at which the interrupt is serviced:
//  - DIRQL for DIRQL interrupt handling.
//  - PASSIVE_LEVEL for passive-level interrupt handling.
typedef
// _Function_class_(EVT_WDF_INTERRUPT_DISABLE)
// _IRQL_requires_same_
// _IRQL_requires_min_(PASSIVE_LEVEL)
NTSTATUS
EVT_WDF_INTERRUPT_DISABLE(
    // _In_
    WDFINTERRUPT Interrupt,
    // _In_
    WDFDEVICE AssociatedDevice
    );

typedef EVT_WDF_INTERRUPT_DISABLE *PFN_WDF_INTERRUPT_DISABLE;

// line 87
typedef
// _Function_class_(EVT_WDF_INTERRUPT_SYNCHRONIZE)
// _IRQL_requires_same_
// _IRQL_requires_min_(PASSIVE_LEVEL)
BOOLEAN
EVT_WDF_INTERRUPT_SYNCHRONIZE(
    // _In_
    WDFINTERRUPT Interrupt,
    // _In_
    WDFCONTEXT Context
    );

typedef EVT_WDF_INTERRUPT_SYNCHRONIZE *PFN_WDF_INTERRUPT_SYNCHRONIZE;

// line 176
typedef EVT_WDF_INTERRUPT_DISABLE *PFN_WDF_INTERRUPT_DISABLE;

//
// Interrupt Configuration Structure
//
typedef struct _WDF_INTERRUPT_CONFIG {
    ULONG              Size;
    // If this interrupt is to be synchronized with other interrupt(s) assigned
    // to the same WDFDEVICE, create a WDFSPINLOCK and assign it to each of the
    // WDFINTERRUPTs config.
    WDFSPINLOCK                     SpinLock;
/*
    WDF_TRI_STATE                   ShareVector;
    BOOLEAN                         FloatingSave;
    // DIRQL handling: automatic serialization of the DpcForIsr/WaitItemForIsr.
    // Passive-level handling: automatic serialization of all callbacks.
    BOOLEAN                         AutomaticSerialization;
    // Event Callbacks
    PFN_WDF_INTERRUPT_ISR           EvtInterruptIsr;
    PFN_WDF_INTERRUPT_DPC           EvtInterruptDpc;
*/
    PFN_WDF_INTERRUPT_ENABLE        EvtInterruptEnable;
    PFN_WDF_INTERRUPT_DISABLE       EvtInterruptDisable;
/*
    PFN_WDF_INTERRUPT_WORKITEM      EvtInterruptWorkItem;
    // These fields are only used when interrupt is created in
    // EvtDevicePrepareHardware callback.
    PCM_PARTIAL_RESOURCE_DESCRIPTOR InterruptRaw;
    PCM_PARTIAL_RESOURCE_DESCRIPTOR InterruptTranslated;
    // Optional passive lock for handling interrupts at passive-level.
    WDFWAITLOCK                     WaitLock;
    // TRUE: handle interrupt at passive-level.
    // FALSE: handle interrupt at DIRQL level. This is the default.
    BOOLEAN                         PassiveHandling;
    // TRUE: Interrupt is reported inactive on explicit power down
    //       instead of disconnecting it.
    // FALSE: Interrupt is disconnected instead of reporting inactive
    //        on explicit power down.
    // DEFAULT: Framework decides the right value.
    WDF_TRI_STATE                   ReportInactiveOnPowerDown;
*/
} WDF_INTERRUPT_CONFIG, *PWDF_INTERRUPT_CONFIG;

/*****************************************************************************
 * File: wdfdmaenabler.h
 ****************************************************************************/
// line 38
typedef enum _WDF_DMA_PROFILE {
    WdfDmaProfileInvalid = 0,
    WdfDmaProfilePacket,
    WdfDmaProfileScatterGather,
    WdfDmaProfilePacket64,
    WdfDmaProfileScatterGather64,
    WdfDmaProfileScatterGatherDuplex,
    WdfDmaProfileScatterGather64Duplex,
    WdfDmaProfileSystem,
    WdfDmaProfileSystemDuplex,
} WDF_DMA_PROFILE;

typedef enum _WDF_DMA_DIRECTION {
    WdfDmaDirectionReadFromDevice = FALSE,
    WdfDmaDirectionWriteToDevice = TRUE,
} WDF_DMA_DIRECTION;

// line 158
typedef struct _WDF_DMA_ENABLER_CONFIG {
    // Size of this structure in bytes
    ULONG                Size;
/*
    // One of the above WDF_DMA_PROFILES
    WDF_DMA_PROFILE      Profile;
    // Maximum DMA Transfer handled in bytes.
    size_t               MaximumLength;
    // The various DMA PnP/Power event callbacks
    PFN_WDF_DMA_ENABLER_FILL                  EvtDmaEnablerFill;
    PFN_WDF_DMA_ENABLER_FLUSH                 EvtDmaEnablerFlush;
    PFN_WDF_DMA_ENABLER_DISABLE               EvtDmaEnablerDisable;
    PFN_WDF_DMA_ENABLER_ENABLE                EvtDmaEnablerEnable;
    PFN_WDF_DMA_ENABLER_SELFMANAGED_IO_START  EvtDmaEnablerSelfManagedIoStart;
    PFN_WDF_DMA_ENABLER_SELFMANAGED_IO_STOP   EvtDmaEnablerSelfManagedIoStop;
    // Overrides the address width specified by the DMA profile.
    ULONG               AddressWidthOverride;
    // Overrides the version of the WDM DMA interfaces that WDF uses
    // (0 for default).
    ULONG               WdmDmaVersionOverride;
    // Bit field combination of values from the WDF_DMA_ENABLER_CONFIG_FLAGS
    // enumeration
    ULONG               Flags;
*/
} WDF_DMA_ENABLER_CONFIG, *PWDF_DMA_ENABLER_CONFIG;

/*****************************************************************************
 * File: wdfdmatransaction.h
 ****************************************************************************/
typedef
// _Function_class_(EVT_WDF_PROGRAM_DMA)
// _IRQL_requires_same_
// _IRQL_requires_(DISPATCH_LEVEL)
BOOLEAN
EVT_WDF_PROGRAM_DMA(
    // _In_
    WDFDMATRANSACTION Transaction,
    // _In_
    WDFDEVICE Device,
    // _In_
    WDFCONTEXT Context,
    // _In_
    WDF_DMA_DIRECTION Direction,
    // _In_
    PSCATTER_GATHER_LIST SgList
    );

/*****************************************************************************
 * File: wdfworkitem.h
 ****************************************************************************/
// line 39
// This is the function that gets called back into the driver
// when the WorkItem fires.
//
typedef
// _Function_class_(EVT_WDF_WORKITEM)
// _IRQL_requires_same_
// _IRQL_requires_max_(PASSIVE_LEVEL)
VOID
EVT_WDF_WORKITEM(
    // _In_
    WDFWORKITEM WorkItem
    );

typedef EVT_WDF_WORKITEM *PFN_WDF_WORKITEM;

// line 66
typedef struct _WDF_WORKITEM_CONFIG {
    ULONG Size;
    PFN_WDF_WORKITEM EvtWorkItemFunc;
    // If this is TRUE, the workitem will automatically serialize
    // with the event callback handlers of its Parent Object.
    //
    // Parent Object's callback constraints should be compatible
    // with the work item (PASSIVE_LEVEL), or the request will fail.
    BOOLEAN AutomaticSerialization;
} WDF_WORKITEM_CONFIG, *PWDF_WORKITEM_CONFIG;

/*****************************************************************************
 * File: ntddndis.h
 ****************************************************************************/
// line 199
typedef ULONG NDIS_OID, *PNDIS_OID;

// line 541
#define OID_GEN_LINK_SPEED                      0x00010107
#define OID_GEN_MEDIA_CONNECT_STATUS            0x00010114
#define OID_GEN_CURRENT_PACKET_FILTER           0x0001010E

//line 2308
typedef struct _NDIS_PM_PACKET_PATTERN
{
    ULONG   Priority;                   // Importance of the given pattern.
    ULONG   Reserved;                   // Context information for transports.
    ULONG   MaskSize;                   // Size in bytes of the pattern mask.
    ULONG   PatternOffset;              // Offset from beginning of this
                                        // structure to the pattern bytes.
    ULONG   PatternSize;                // Size in bytes of the pattern.
    ULONG   PatternFlags;               // Flags (TBD).
} NDIS_PM_PACKET_PATTERN, *PNDIS_PM_PACKET_PATTERN;

//line 2314
typedef enum _NDIS_DEVICE_POWER_STATE
{
    NdisDeviceStateUnspecified = 0,
/*
    NdisDeviceStateD0,
    NdisDeviceStateD1,
    NdisDeviceStateD2,
    NdisDeviceStateD3,
    NdisDeviceStateMaximum
*/
} NDIS_DEVICE_POWER_STATE, *PNDIS_DEVICE_POWER_STATE;

typedef struct _NDIS_PM_WAKE_UP_CAPABILITIES
{
    NDIS_DEVICE_POWER_STATE MinMagicPacketWakeUp;
/*
    NDIS_DEVICE_POWER_STATE MinPatternWakeUp;
    NDIS_DEVICE_POWER_STATE MinLinkChangeWakeUp;
*/
} NDIS_PM_WAKE_UP_CAPABILITIES, *PNDIS_PM_WAKE_UP_CAPABILITIES;

typedef struct _NDIS_PNP_CAPABILITIES
{
    ULONG                           Flags;
    NDIS_PM_WAKE_UP_CAPABILITIES    WakeUpCapabilities;
} NDIS_PNP_CAPABILITIES, *PNDIS_PNP_CAPABILITIES;

// line 2487
 typedef enum _NDIS_MEDIA_STATE
{
    NdisMediaStateConnected,
    NdisMediaStateDisconnected
} NDIS_MEDIA_STATE, *PNDIS_MEDIA_STATE;

// line 2530
typedef int NDIS_STATUS, *PNDIS_STATUS;

/*****************************************************************************
 * File: shared/devioctl.h
 ****************************************************************************/
// line 162
#define FILE_READ_ACCESS          ( 0x0001 )    // file & pipe
#define FILE_WRITE_ACCESS         ( 0x0002 )    // file & pipe

/*****************************************************************************
 * File: km/miniport.h
 ****************************************************************************/
// line 143
#define MEMORY_ALLOCATION_ALIGNMENT 16

// line 232
// There
#define DECLSPEC_ALIGN(x)

//line 6429
#define PCI_WHICHSPACE_CONFIG               0x0

/*****************************************************************************
 * File: sal.h
 * Note that this is SAL 1.x that is required for PCI
 ****************************************************************************/
#define __field_ecount(fe) 

/*****************************************************************************
 * File: wdfcommonbuffer.h
 ****************************************************************************/
// line 155
typedef
// _IRQL_requires_max_(DISPATCH_LEVEL)
WDFAPI
PVOID
(*PFN_WDFCOMMONBUFFERGETALIGNEDVIRTUALADDRESS)(
    // _In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
    // _In_
    WDFCOMMONBUFFER CommonBuffer
    );

// _IRQL_requires_max_(DISPATCH_LEVEL)
PVOID
FORCEINLINE
WdfCommonBufferGetAlignedVirtualAddress(
    // _In_
    WDFCOMMONBUFFER CommonBuffer
    )
{
  return NULL;
    //return ((PFN_WDFCOMMONBUFFERGETALIGNEDVIRTUALADDRESS) WdfFunctions[WdfCommonBufferGetAlignedVirtualAddressTableIndex])(WdfDriverGlobals, CommonBuffer);
}

typedef
// _IRQL_requires_max_(DISPATCH_LEVEL)
WDFAPI
PHYSICAL_ADDRESS
(*PFN_WDFCOMMONBUFFERGETALIGNEDLOGICALADDRESS)(
    // _In_
    PWDF_DRIVER_GLOBALS DriverGlobals,
    // _In_
    WDFCOMMONBUFFER CommonBuffer
    );

// _IRQL_requires_max_(DISPATCH_LEVEL)
PHYSICAL_ADDRESS
FORCEINLINE
WdfCommonBufferGetAlignedLogicalAddress(
    // _In_
    WDFCOMMONBUFFER CommonBuffer
    )
{
  PHYSICAL_ADDRESS p;
  return p;
    // return ((PFN_WDFCOMMONBUFFERGETALIGNEDLOGICALADDRESS) WdfFunctions[WdfCommonBufferGetAlignedLogicalAddressTableIndex])(WdfDriverGlobals, CommonBuffer);
}

#endif // #ifndef _HARNESS_H_
