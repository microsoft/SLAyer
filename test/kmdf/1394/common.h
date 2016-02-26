/*   Copyright (c) Microsoft Corporation.  All rights reserved. */

/* common code for kmdf/1394 test programs */


/* Source: WDK/src_5239/kmdf/1394/common.h, line 24. */
/* #if _1394VDEV_DRIVER_ */
/* #define _DRIVERNAME_  "1394VDEV" */
/* #else */
/* #define _DRIVERNAME_ "1394DIAG" */
/* #endif */


/* #define TL_TRACE        0 */
/* #define TL_WARNING      1 */
/* #define TL_ERROR        2 */
/* #define TL_FATAL        3 */

/* extern unsigned char t1394DebugLevel; */

/* #define TRACE( l, x )                       \ */
/*     if( (l) >= t1394DebugLevel ) {      \ */
/*         KdPrint( (_DRIVERNAME_ ": ") );     \ */
/*         KdPrint( x );                       \ */
/*     } */


/* Source: WDK/src_5239/kmdf/1394/common.h, line 39. */

//-----------------------------------------------------------------------------
// 4127 -- Conditional Expression is Constant warning
//-----------------------------------------------------------------------------
#define WHILE(a) \
while(__pragma(warning(disable:4127)) a __pragma(warning(disable:4127)))

