/* Minimal stand-in for GHC's HsFFI.h (the harness shell has no GHC):
 * HsInt is StgInt, a signed integer of pointer width. */
#ifndef WF_STUB_HSFFI_H
#define WF_STUB_HSFFI_H
#include <stdint.h>
typedef intptr_t HsInt;
#endif
