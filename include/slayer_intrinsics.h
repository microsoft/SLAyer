/*   Copyright (c) Microsoft Corporation.  All rights reserved.  */

/*
  Declare SLAyer intrinsics.

  SLAyer treats calls to the functions declared here specially, using internal
  analysis-specific semantics.

  SLAyer recognizes and specially treats the code produced by the macro
  definitions, so it is not necessary to use the provided macro names.

  This file is included automatically when SLAyer invokes the C compiler.
*/

#ifndef _SLAYER_INTRINSICS_H_
#define _SLAYER_INTRINSICS_H_

#define _SLAyer_NULL ((void *)0)

#ifdef _WIN64
#define _SLAyer_offsetof(s,m) (size_t)( (__int64)&(((s *)0)->m) )
#else
#define _SLAyer_offsetof(s,m) (size_t)&(((s *)0)->m)
#endif
#define _SLAyer_containerof(ptr, type, member) ((type *) ((char *)(ptr) - _SLAyer_offsetof(type, member)))

#ifdef _WIN64
void* _SLAyer_malloc(unsigned __int64);
#else
void* _SLAyer_malloc(unsigned int);
#endif
void _SLAyer_free(void*);

/* Calls to these functions other than through _SLAyer_assert or
   _SLAyer_assume may be translated suboptimally. */
void _SLAyer_error(void);
void _SLAyer_unreachable(void);

/* Translate to branches to avoid unnecessary bool->int->bool conversions. */
/* Use a definition like assert.h except with _SLAyer_unreachable in place of
   _wassert to enable redefining the result of the standard definition. */
#define _SLAyer_assert(exp) (void)( (!!(exp)) || (_SLAyer_error(), 0) )
#define _SLAyer_assume(exp) (void)( (!!(exp)) || (_SLAyer_unreachable(), 0) )

int _SLAyer_nondet();

#endif // #ifndef _SLAYER_INTRINSICS_H_
