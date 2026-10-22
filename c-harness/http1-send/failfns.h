/*
 * failfns.h - socket-call failure injection for the http1-send "fail"
 * build variant (see module.mk and common/wf_fail.h).
 *
 * The variant renames send and pread to failpoint-aware fakes (defined
 * in failfns.c), so the send / sendall / sendfile loops can be driven
 * through states a live stream socket cannot produce: send() returning
 * 0 ("peer closed") in the middle of a buffer, a short count followed
 * by an error, and pread() failing with EINTR. Unarmed fakes pass
 * through to the real syscalls, so the ordinary tests run unchanged in
 * the fail variant too.
 *
 * Failpoints arm through the environment (WF_FAIL_<name>, a countdown,
 * see wf_fail.h) from the test translation unit; the fake modes are:
 *
 *   send_short   send at most half the buffer in one call (real bytes,
 *                so what the shim reports as sent still arrives)
 *   send_zero    return 0, as a stream socket reports a closed peer
 *   send_eintr   fail with EINTR (the kernels retry internally)
 *   send_error   fail with EPIPE
 *   pread_eintr  fail with EINTR (the sendfile fallback retries)
 *
 * The send fakes are checked in exactly the order above, so a two-step
 * script arms both names with "1": the earlier mode takes the first
 * call and the later one the next (a short send followed by a closed
 * peer is arm("send_short"); arm("send_zero");).
 *
 * The renames are object-like (module.mk), so the system headers'
 * own prototypes are rewritten in place to declare the fakes; this
 * header therefore declares nothing itself and is force-included
 * first (module.mk) so the guard below applies before any system
 * header. failfns.c starts by #undefing the renames and declaring the
 * real syscalls explicitly, so its pass-through calls reach them.
 */
#ifndef HTTP1_SEND_FAILFNS_H
#define HTTP1_SEND_FAILFNS_H

/* glibc's _FORTIFY_SOURCE headers define inline send / pread wrappers;
 * the renames would turn those into inline definitions of the fakes'
 * names, so the failpoints would never run and failfns.c's definitions
 * would collide. The fail variant is about failure injection, not
 * hardening, so drop fortify for it. */
#ifdef __GLIBC__
#  undef _FORTIFY_SOURCE
#endif

#endif /* HTTP1_SEND_FAILFNS_H */
