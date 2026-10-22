/*
 * failfns.h - library failure injection for the permessage-deflate
 * shim's "fail" build variant.
 *
 * The variant renames these entry points (see module.mk) so wf_pmd.c
 * calls the fakes below instead of zlib. A fake fails when its
 * failpoint fires (common/wf_fail.h) and otherwise passes through to
 * the real function, so a fail-variant binary can run ordinary tests
 * too. Naming follows the renamed entry point, so e.g.
 * setenv("WF_FAIL_inflateInit2", "1", 1) makes the next inflateInit2
 * call fail.
 *
 * zlib 1.3 declares these with direct prototypes, so the renames also
 * rewrite their declarations in the included zlib.h; the explicit
 * declarations below match those (and cover any zlib that still uses
 * the old OF() prototype style, which the rename cannot rewrite).
 */
#ifndef WEBSOCKET_PMD_FAILFNS_H
#define WEBSOCKET_PMD_FAILFNS_H

#include <zlib.h>

int wf_fail_inflateInit2_(z_streamp strm, int windowBits,
                          const char *version, int stream_size);
int wf_fail_deflateInit2_(z_streamp strm, int level, int method,
                          int windowBits, int memLevel, int strategy,
                          const char *version, int stream_size);
int wf_fail_inflateGetDictionary(z_streamp strm, Bytef *dictionary,
                                 uInt *dictLength);
int wf_fail_inflateReset(z_streamp strm);
int wf_fail_deflateReset(z_streamp strm);

/* deflate() itself: "deflate_zmem" makes it report Z_MEM_ERROR (the
 * shim maps that to WF_PMD_OOM), "deflate_zstream" Z_STREAM_ERROR
 * (mapped to WF_PMD_BAD_INPUT). */
int wf_fail_deflate(z_streamp strm, int flush);

#endif /* WEBSOCKET_PMD_FAILFNS_H */
