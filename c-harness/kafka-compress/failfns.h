/*
 * failfns.h - library-constructor failure injection for the kafka
 * codec shims' "fail" build variant.
 *
 * The variant renames these entry points (see module.mk) so the shims
 * call the fakes below instead of the real codec libraries. A fake
 * fails when its failpoint fires (common/wf_fail.h) and otherwise
 * passes through to the real function, so a fail-variant binary can
 * run ordinary tests too. Naming follows the renamed entry point, so
 * e.g. setenv("WF_FAIL_inflateInit2", "1", 1) makes the next
 * inflateInit2 call fail.
 *
 * zlib's inflateInit2_ is declared with the old OF() prototype style,
 * which the rename cannot rewrite, so its fake needs this explicit
 * declaration; the lz4 and snappy declarations are rewritten in place.
 */
#ifndef KAFKA_COMPRESS_FAILFNS_H
#define KAFKA_COMPRESS_FAILFNS_H

#include <lz4frame.h>
#include <snappy-c.h>
#include <zlib.h>
#include <zstd.h>

int wf_fail_inflateInit2_(z_streamp strm, int windowBits,
                          const char *version, int stream_size);

/* LZ4F_decompress is renamed in the fail variant; its fake can also
 * simulate a decoder stuck in place ("LZ4F_decompress_no_progress",
 * consumes nothing, produces nothing, wants more) or one that claims
 * to have filled the whole output buffer without writing
 * ("LZ4F_decompress_full"), which drive the callers' no-progress and
 * buffer-growth guards. */
size_t wf_fail_LZ4F_decompress(LZ4F_dctx *dctx, void *dstBuf, size_t *dstSizePtr,
                               const void *srcBuf, size_t *srcSizePtr,
                               const LZ4F_decompressOptions_t *optionsPtr);

#endif /* KAFKA_COMPRESS_FAILFNS_H */
