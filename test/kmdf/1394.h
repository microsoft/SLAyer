/******************************************************************************
 * File: 1394.h (device driver?)
 * Moved these definitions out from the main harness file, as they
 * conflicted with definitions needed by toaster. This file should only
 * be included by the 1394 test drivers.
 ******************************************************************************/

#define POOLTAG_1394 4931

#define IOCTL_SET_LOCAL_HOST_INFORMATION  \
  ((0x00000022 << 16) | ((0x0800 + 30) << 14) | (0 << 2) | (0))

// Copied from 1394samp.h.
typedef struct _DEVICE_EXTENSION {
    PDEVICE_OBJECT          PortDeviceObject;
    PDEVICE_OBJECT          PhysicalDeviceObject;
    PDEVICE_OBJECT          StackDeviceObject;

    WDFDEVICE               WdfDevice;
    WDFIOTARGET             StackIoTarget;
    WDFIOTARGET             PortDeviceIoTarget;

    WDFQUEUE                IoctlQueue;

    WDFQUEUE                BusResetRequestsQueue;

   WDFSPINLOCK				CromSpinLock;
	WDFSPINLOCK				AsyncSpinLock;
	WDFSPINLOCK				IsochSpinLock;
	WDFSPINLOCK				IsochResourceSpinLock;

/*     ULONG                   GenerationCount; */
    LIST_ENTRY              CromData;
    LIST_ENTRY              AsyncAddressData;
    LIST_ENTRY              IsochDetachData;
    LIST_ENTRY              IsochResourceData;

} DEVICE_EXTENSION, *PDEVICE_EXTENSION;

WDF_DECLARE_CONTEXT_TYPE_WITH_NAME(DEVICE_EXTENSION, GetDeviceContext)

//
// This is used to keep track of dynamic crom calls.
//
typedef struct _CROM_DATA {
  LIST_ENTRY      CromList;
  HANDLE          hCromData;
  PVOID           Buffer;
  PMDL            pMdl;
} CROM_DATA, *PCROM_DATA;

// Review: the corresponding Patn allocs CromData->_ [...;Buffer:B; pMdl:pM] * B->_ * pM->_.

//
// This is used to store data for each async address range.
//
typedef struct _ASYNC_ADDRESS_DATA {
  LIST_ENTRY              AsyncAddressList;
  PDEVICE_EXTENSION       DeviceExtension;
  PVOID                   Buffer;
  ULONG                   nLength;
  ULONG                   nAddressesReturned;
  PADDRESS_RANGE          AddressRange;
  HANDLE                  hAddressRange;
  PMDL                    pMdl;
} ASYNC_ADDRESS_DATA, *PASYNC_ADDRESS_DATA;

// Review: the corresponding Patn allocs AsyncAddrData->[...;Buffer:B;AddressRange:A;pMdl:pM] * B->_ * A->_ * pM->_.


//
// This is used to store data needed when calling IsochDetachBuffers.
// We need to store this data seperately for each call to IsochAtfrtachBuffers.
//
typedef struct _ISOCH_DETACH_DATA {
  LIST_ENTRY              IsochDetachList;
  PDEVICE_EXTENSION       DeviceExtension;
/*     PISOCH_DESCRIPTOR       IsochDescriptor; */
/*     WDFREQUEST                    Request; */
/*     PIRP                    newIrp; */
/*     PIRB                    DetachIrb; */
/*     PIRB                    AttachIrb; */
/*     NTSTATUS                AttachStatus; */
/*     KTIMER                  Timer; */
/*     KDPC                    TimerDpc; */
/*     HANDLE                  hResource; */
/*     ULONG                   numIsochDescriptors; */
/*     ULONG                   outputBufferLength; */
/*     ULONG                   bDetach; */
} ISOCH_DETACH_DATA, *PISOCH_DETACH_DATA;

/* Review */
/* The corresponding patn allocs  */
/* IsochDetachData->[...;DeviceExtension:SL_Context; IsochDescriptor:pIsochD; newIrp:pIrp; DetachIrb:d; AttachIrb:a] *  */
/* SL_Context->_ (shared by all isochdetachdatas) *  */
/* pIsochD->_ * pIrp->_ * */
/* (d=0 \/ d->_) * (a=0 \/ a->_) */

//
// This is used to store allocated isoch resources.
// We use this information in case of a surprise removal.
//
typedef struct _ISOCH_RESOURCE_DATA {
    LIST_ENTRY      IsochResourceList;
    HANDLE          hResource;
} ISOCH_RESOURCE_DATA, *PISOCH_RESOURCE_DATA;


/* void t1394_EvtDeviceSelfManagedIoCleanup(PDEVICE_EXTENSION deviceExtension) */
/* { */
/*   PLIST_ENTRY listEntry ; */
/*   while (!IsListEmpty(&deviceExtension->CromData)) { */
/*       PCROM_DATA CromData; */
/*       listEntry = RemoveHeadList(&deviceExtension->CromData); */
/*       CromData = CONTAINING_RECORD(listEntry, CROM_DATA, CromList); */
/*       if (CromData) { */
 /* 	//if (CromData->Buffer) ExFreePool(CromData->Buffer); */
 /* 	//if (CromData->pMdl) IoFreeMdl(CromData->pMdl); */
/* 	ExFreePool(CromData); */
/*       } */
/*     } */
/* } */
