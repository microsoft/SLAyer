/*  Copyright (c) Microsoft Corporation.  All rights reserved.  */

/*
  Declare convenient interface to SLAyer intrinsics.

  When the "SLAyer" symbol is defined, analysis-specific definitions are used.
  Otherwise, executable definitions that under-approximate the SLAyer internal
  semantics are defined.

  This file is included automatically when the SLAyer frontend invokes the C
  compiler.  This default can be overridden by passing the SLAyer frontend the
  -no-builtins flag.

  The "SLAyer" symbol is defined when SLAyer invokes the C compiler.
*/

#ifndef _SLAYER_H_
#define _SLAYER_H_

#include <stdlib.h>
#include <assert.h>

/* Provide executable definitions of SLAyer intrinsics unless SLAyer is defined. */
#ifndef SLAyer

void* _SLAyer_malloc(size_t s) { return malloc(s); }
void _SLAyer_free(void* p) { free(p); }

void _SLAyer_error() { assert(("reached point asserted to be erroneous", 0)); }
void _SLAyer_unreachable() { assert(("reached point assumed to be unreachable", 0)); }

int _SLAyer_nondet() { return rand(); }

#endif // #ifndef SLAyer

#define container_of(ptr, type, member) _SLAyer_containerof(ptr, type, member)
#define CONTAINING_RECORD(ptr, type, member) _SLAyer_containerof(ptr, type, member)

#define malloc(exp) _SLAyer_malloc(exp)
#define free(exp) _SLAyer_free(exp)

#ifdef SLAyer
/* Redefine the result of the definition of assert from assert.h to be robust
   with respect to code that includes assert.h (after slayer.h). */
#define _wassert(_exp,_file,_line) _SLAyer_error()
#endif
#define assume(exp) _SLAyer_assume(exp)

#define nondet() _SLAyer_nondet()


/* Deprecated */
#define FAIL assert(0)
#define FAIL_IF(_e) assert(!(_e))

#endif // #ifndef _SLAYER_H_
