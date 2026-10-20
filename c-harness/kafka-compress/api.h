/* Kernel API of the Kafka codec shims, plus reference decoders built on
 * the codec libraries' own streaming APIs.
 *
 * Imports (Haskell -> C):
 *   Kafka/Compression/Snappy.hs: snappy_compress_wrapper,
 *     snappy_decompress_wrapper (ByteString path, Types.decompress).
 *   Kafka/Compression/Lz4.hs: lz4_compress_wrapper_level,
 *     lz4_decompress_wrapper (ByteString path).
 *   Kafka/Compression/Ring.hs (fetch path, RecordBatchWire): wf_gzip_inflate_into,
 *     wf_lz4f_decompress_into, wf_zstd_get_frame_content_size,
 *     wf_zstd_decompress_into, snappy_get_uncompressed_length_wrapper,
 *     snappy_decompress_into.
 *
 * Caller contract (from those call sites):
 *   - Every *decompress* input is untrusted broker bytes: any content,
 *     any length >= 1 (the Haskell short-circuits len <= 0 and empty
 *     ByteStrings), no alignment. The shims must never read outside
 *     [src, src+len) or write outside [dst, dst+cap), and must classify
 *     every input as OK / NEEDS_MORE_OUT / BAD_INPUT (Ring.hs) or
 *     success / 0 (ByteString wrappers).
 *   - Ring.hs retries on NEEDS_MORE_OUT with a doubled buffer and no
 *     size limit, so NEEDS_MORE_OUT must mean "the output buffer was
 *     full"; for truncated input it would loop until allocation fails.
 *   - dst is never NULL (mallocByteString / a magic ring); dst_cap >= 1
 *     except snappy/zstd exact sizing, where it may be 0 with a valid
 *     pointer.
 *   - snappy_get_uncompressed_length_wrapper / wf_zstd_get_frame_content_size
 *     results are allocated by the Haskell before decompressing
 *     (mallocByteString (fromIntegral n)), so they must not report sizes
 *     the input cannot produce; Ring.hs converts the zstd size to Int, so
 *     values >= 2^63 would turn negative and bypass its capacity check.
 *   - snappy_decompress_into is only called with dst_capacity equal to
 *     the length the header declares (exact sizing).
 *   - The ByteString wrappers return 0 for failure, so a valid input
 *     that decompresses to zero bytes is reported as 0 too; they leave
 *     *output == NULL then (nothing to free). On success the caller
 *     frees *output.
 *   - *_compress_* inputs are the application's own data (any length >= 1;
 *     the Haskell short-circuits empty input).
 */
#ifndef KAFKA_COMPRESS_API_H
#define KAFKA_COMPRESS_API_H

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <lz4frame.h>
#include <snappy-c.h>
#include <zlib.h>
#include <zstd.h>

#include "lz4_ffi.h"
#include "snappy_ffi.h"
#include "wireform_decompress.h"

/* Growing byte buffer for references. */
typedef struct {
    uint8_t *p;
    size_t n, cap;
} cbuf;

static inline void cbuf_reserve(cbuf *b, size_t extra)
{
    if (b->n + extra <= b->cap) return;
    size_t c = b->cap ? b->cap : 256;
    while (c < b->n + extra) c *= 2;
    b->p = realloc(b->p, c);
    if (!b->p) abort();
    b->cap = c;
}

/* Reference outcome: 1 = complete stream decoded, 0 = malformed or
 * truncated, -1 = output exceeded `limit` (unknown). out holds whatever
 * the library produced before stopping. */

/* zlib streaming inflate with gzip/zlib auto-detect, one member. */
static inline int ref_gzip(const uint8_t *src, size_t n, size_t limit, cbuf *out)
{
    z_stream zs;
    memset(&zs, 0, sizeof zs);
    if (inflateInit2(&zs, 15 + 32) != Z_OK) abort();
    zs.next_in = (Bytef *)src;
    zs.avail_in = (uInt)n;
    int rc;
    for (;;) {
        cbuf_reserve(out, 4096);
        zs.next_out = out->p + out->n;
        zs.avail_out = 4096;
        rc = inflate(&zs, Z_NO_FLUSH);
        out->n += 4096 - zs.avail_out;
        if (rc == Z_STREAM_END) break;
        if (rc != Z_OK) break;
        if (out->n > limit) break;
    }
    inflateEnd(&zs);
    if (rc == Z_STREAM_END) return out->n > limit ? -1 : 1;
    if (rc == Z_OK && out->n > limit) return -1;
    return 0;
}

/* LZ4F streaming decode of one frame. */
static inline int ref_lz4f(const uint8_t *src, size_t n, size_t limit, cbuf *out,
                           size_t *consumed)
{
    LZ4F_dctx *d;
    if (LZ4F_isError(LZ4F_createDecompressionContext(&d, LZ4F_VERSION))) abort();
    size_t off = 0;
    int res = 0;
    for (;;) {
        cbuf_reserve(out, 4096);
        size_t dn = 4096, sn = n - off;
        size_t hint = LZ4F_decompress(d, out->p + out->n, &dn, src + off, &sn, NULL);
        if (LZ4F_isError(hint)) break;
        off += sn;
        out->n += dn;
        if (hint == 0) {
            res = 1;
            break;
        }
        if (out->n > limit) {
            res = -1;
            break;
        }
        if (sn == 0 && dn == 0) break; /* input exhausted mid-frame */
    }
    LZ4F_freeDecompressionContext(d);
    if (consumed) *consumed = off;
    if (res == 1 && out->n > limit) res = -1;
    return res;
}

/* zstd streaming decode of every frame in the input. */
static inline int ref_zstd(const uint8_t *src, size_t n, size_t limit, cbuf *out)
{
    ZSTD_DStream *ds = ZSTD_createDStream();
    if (!ds) abort();
    ZSTD_inBuffer in = { src, n, 0 };
    size_t last = 1;
    int res = 0;
    for (;;) {
        cbuf_reserve(out, 1 << 16);
        ZSTD_outBuffer o = { out->p + out->n, 1 << 16, 0 };
        last = ZSTD_decompressStream(ds, &o, &in);
        out->n += o.pos;
        if (ZSTD_isError(last)) break;
        if (out->n > limit) {
            res = -1;
            break;
        }
        if (in.pos == n && o.pos < o.size) {
            res = last == 0 ? 1 : 0;
            break;
        }
    }
    ZSTD_freeDStream(ds);
    return res;
}

#endif
