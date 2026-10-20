/*
 * wireform-network/cbits/wireform_affinity.c, as
 * Wireform.Network.Transport.Profile imports it:
 *
 *   foreign import ccall unsafe "sched_setaffinity_single"
 *     c_pin_thread :: Int -> IO Int
 *
 * Caller contract: `core` is any Haskell Int (pinToCore takes the core
 * number from configuration); the result is 0 or -1 (errno set) and the
 * caller only checks for 0. The call pins the calling OS thread.
 * There is no input to fuzz beyond one integer, so the tests sweep it
 * exhaustively around every boundary instead of a fuzz target.
 */
#ifndef WF_NETWORK_AFFINITY_API_H
#define WF_NETWORK_AFFINITY_API_H

#include "HsFFI.h"

HsInt sched_setaffinity_single(HsInt core);

#endif
