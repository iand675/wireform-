/*
 * wireform_decompress.c
 *
 * Direct-into-buffer decompression wrappers for the Kafka codecs
 * (snappy / gzip / lz4 / zstd).  Each one takes a source pointer +
 * length AND a destination pointer + capacity from the Haskell side
 * — typically a slice of the magic ring's backing memory — and
 * writes the decompressed bytes directly into the destination
 * without going through any intermediate Haskell-side ByteString
 * allocation.
 *
 * The two "sized" codecs (snappy, zstd) can report the expected
 * output length up front; the Haskell side allocates a destination
 * of exactly that size before calling decompress.  The two
 * "streaming" codecs (gzip, lz4-frame) cannot, so we expose them as
 * incremental functions that the Haskell side can drive in a loop
 * against a growing destination ring (when the destination is a
 * magic ring of fixed size, one call is usually enough; the
 * Haskell side checks for "needs more output" and resizes /
 * advances by the produced count).
 *
 * Error handling: every wrapper returns 0 on success and a small
 * negative int on failure, with the produced/consumed counts
 * written to out-pointers so the Haskell side can decide what to
 * do (truncate, retry with bigger buffer, raise a user-visible
 * error, etc.).
 */

#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include <zlib.h>
#include <lz4frame.h>
#include <zstd.h>

/* ------------------------------------------------------------------- *
 *  Return codes used by every entry point in this file.                *
 * ------------------------------------------------------------------- */

#define WF_DECOMPRESS_OK              0
#define WF_DECOMPRESS_NEEDS_MORE_OUT -1  /* dst too small; grow + retry */
#define WF_DECOMPRESS_BAD_INPUT      -2  /* malformed compressed bytes  */
#define WF_DECOMPRESS_OOM            -3  /* libc malloc / alloc failed  */
#define WF_DECOMPRESS_INIT_FAIL      -4  /* lib init returned an error  */

/* zlib counts in 32-bit uInt; larger buffers are fed in chunks of at
 * most this size. Overridable so the test harness can drive the
 * chunking with small buffers. */
#ifndef WF_DECOMPRESS_ZCHUNK
#define WF_DECOMPRESS_ZCHUNK ((size_t)(uInt)-1)
#endif

/* =================================================================== *
 *  Gzip (RFC 1952) via libz                                            *
 * =================================================================== *
 *
 * No public "give me the uncompressed size" API in zlib — the size
 * isn't stored in the gzip header.  We drive 'inflate' against the
 * caller-supplied destination buffer; if 'avail_out' hits zero
 * before the stream ends, return WF_DECOMPRESS_NEEDS_MORE_OUT so
 * the Haskell side can grow + retry against a fresh / larger ring
 * region.
 *
 * For Kafka recv path the gzip-compressed Records section is
 * bounded by the wire frame, so a one-shot call with a destination
 * sized to the connection-level @receive.message.max.bytes@ (the
 * existing magic-ring size) will succeed.
 */

int wf_gzip_inflate_into(const uint8_t *src, size_t src_len,
                         uint8_t *dst, size_t dst_cap,
                         size_t *out_produced) {
    if (out_produced) *out_produced = 0;

    z_stream zs;
    memset(&zs, 0, sizeof(zs));

    /* 15 + 32 enables both gzip wrapper (RFC 1952) and zlib wrapper
     * (RFC 1950) auto-detection.  Kafka uses gzip; the auto-detect
     * adds no per-call overhead. */
    int rc = inflateInit2(&zs, 15 + 32);
    if (rc != Z_OK) {
        return WF_DECOMPRESS_INIT_FAIL;
    }

    /* zlib counts in 32-bit uInt: hand it the buffers in chunks so
     * src_len / dst_cap above 4 GiB are neither truncated nor
     * miscounted. Z_FINISH on the final chunk keeps zlib's one-shot
     * fast path (no sliding window) for everything below 4 GiB. */
    size_t in_left = src_len, out_left = dst_cap;
    zs.next_in   = (Bytef *) src;
    zs.next_out  = (Bytef *) dst;
    zs.avail_in  = 0;
    zs.avail_out = 0;
    for (;;) {
        if (zs.avail_in == 0) {
            size_t n = in_left < WF_DECOMPRESS_ZCHUNK ? in_left : WF_DECOMPRESS_ZCHUNK;
            zs.avail_in = (uInt) n;
            in_left -= n;
        }
        if (zs.avail_out == 0) {
            size_t n = out_left < WF_DECOMPRESS_ZCHUNK ? out_left : WF_DECOMPRESS_ZCHUNK;
            zs.avail_out = (uInt) n;
            out_left -= n;
        }
        rc = inflate(&zs, in_left == 0 && out_left == 0 ? Z_FINISH : Z_NO_FLUSH);
        if (rc != Z_OK && rc != Z_BUF_ERROR) {
            break;
        }
        if (!((zs.avail_in == 0 && in_left != 0) || (zs.avail_out == 0 && out_left != 0))) {
            break;
        }
    }

    size_t produced = dst_cap - out_left - zs.avail_out;
    inflateEnd(&zs);

    if (out_produced) *out_produced = produced;

    if (rc == Z_STREAM_END) {
        return WF_DECOMPRESS_OK;
    } else if (rc == Z_BUF_ERROR || rc == Z_OK) {
        /* Stopped short of the end of the stream. A full destination
         * may be hiding more output: grow + retry. Otherwise all input
         * was consumed mid-stream, i.e. the input is truncated; asking
         * for more output would make the caller double its buffer
         * forever. */
        return zs.avail_out == 0 ? WF_DECOMPRESS_NEEDS_MORE_OUT
                                 : WF_DECOMPRESS_BAD_INPUT;
    } else {
        return WF_DECOMPRESS_BAD_INPUT;
    }
}

/* =================================================================== *
 *  LZ4 frame via liblz4                                                *
 * =================================================================== *
 *
 * The frame header optionally encodes the content size — but Kafka
 * producers (the JVM client, librdkafka) typically omit it.  Use
 * the streaming decompressor.  One call handles the whole stream
 * when the destination buffer fits; if not, return
 * WF_DECOMPRESS_NEEDS_MORE_OUT so the Haskell side can grow / chunk.
 *
 * Allocates a per-call decompression context (tiny — a few hundred
 * bytes — and pooling across calls is a follow-up if it shows up
 * in benchmarks).
 */

int wf_lz4f_decompress_into(const uint8_t *src, size_t src_len,
                            uint8_t *dst, size_t dst_cap,
                            size_t *out_produced, size_t *out_consumed) {
    if (out_produced) *out_produced = 0;
    if (out_consumed) *out_consumed = 0;

    LZ4F_dctx *dctx = NULL;
    LZ4F_errorCode_t err = LZ4F_createDecompressionContext(&dctx, LZ4F_VERSION);
    if (LZ4F_isError(err)) {
        return WF_DECOMPRESS_INIT_FAIL;
    }

    /* LZ4F_decompress returns when the frame ends, src is consumed or
     * dst is full; call again while it made progress and both have
     * bytes left. dst stays one contiguous buffer, so linked blocks
     * that reference earlier output in dst remain valid. */
    size_t in_off = 0, out_off = 0, hint;
    for (;;) {
        size_t dst_size = dst_cap - out_off;
        size_t src_size = src_len - in_off;
        hint = LZ4F_decompress(dctx, dst + out_off, &dst_size,
                               src + in_off, &src_size, NULL);
        if (LZ4F_isError(hint)) {
            break;
        }
        in_off += src_size;
        out_off += dst_size;
        if (hint == 0 || in_off == src_len || out_off == dst_cap
            || (src_size == 0 && dst_size == 0)) {
            break;
        }
    }

    LZ4F_freeDecompressionContext(dctx);

    if (out_produced) *out_produced = out_off;
    if (out_consumed) *out_consumed = in_off;

    if (LZ4F_isError(hint)) {
        return WF_DECOMPRESS_BAD_INPUT;
    }
    if (hint == 0) {
        return WF_DECOMPRESS_OK;
    }
    /* The frame is incomplete. With dst full, more output may follow:
     * grow + retry. With room left, the input ran out mid-frame
     * (truncated); before, this also said NEEDS_MORE_OUT and the
     * Haskell caller doubled its buffer until allocation failed. */
    return out_off == dst_cap ? WF_DECOMPRESS_NEEDS_MORE_OUT
                              : WF_DECOMPRESS_BAD_INPUT;
}

/* =================================================================== *
 *  Zstd via libzstd                                                    *
 * =================================================================== *
 *
 * Zstd frames typically embed the decompressed size in the header.
 * 'wf_zstd_get_frame_content_size' returns it (or
 * (size_t)-1 / -2 sentinels for unknown / error per the
 * upstream conventions); the Haskell side uses that to size the
 * destination buffer exactly, then calls 'wf_zstd_decompress_into'
 * for a single-shot decompress.
 *
 * For frames that don't carry the size up front (rare in Kafka)
 * the Haskell side can fall back to driving 'ZSTD_decompressStream'
 * directly against a growing ring — that path lives in the
 * Haskell module so it can pick the right destination strategy
 * without C↔Haskell round-trips on each grow.
 */

/* Largest block a zstd frame can regenerate (Block_Maximum_Size) and
 * the smallest block that regenerates anything: a 3-byte header plus
 * one RLE byte. */
#define WF_ZSTD_BLOCK_MAX 131072u
#define WF_ZSTD_MIN_BLOCK 4u

#ifndef ZSTD_MAGIC_SKIPPABLE_START
#define ZSTD_MAGIC_SKIPPABLE_START 0x184D2A50U
#endif
#ifndef ZSTD_MAGIC_SKIPPABLE_MASK
#define ZSTD_MAGIC_SKIPPABLE_MASK 0xFFFFFFF0U
#endif

size_t wf_zstd_get_frame_content_size(const uint8_t *src, size_t src_len) {
    /* Returns: actual size, ZSTD_CONTENTSIZE_UNKNOWN (-1ULL), or
     * ZSTD_CONTENTSIZE_ERROR (-2ULL). */
    unsigned long long fcs = ZSTD_getFrameContentSize(src, src_len);
    if (fcs == ZSTD_CONTENTSIZE_UNKNOWN || fcs == ZSTD_CONTENTSIZE_ERROR) {
        return (size_t) fcs;
    }
    /* A leading skippable frame reports 0, which is not the size of
     * the data frames after it; let the caller size the output itself.
     * (A non-error result means zstd parsed at least the 4-byte magic.) */
    uint32_t magic = (uint32_t) src[0] | ((uint32_t) src[1] << 8)
                   | ((uint32_t) src[2] << 16) | ((uint32_t) src[3] << 24);
    if ((magic & ZSTD_MAGIC_SKIPPABLE_MASK) == ZSTD_MAGIC_SKIPPABLE_START) {
        return (size_t) ZSTD_CONTENTSIZE_UNKNOWN;
    }
    /* The header is untrusted: a 13-byte frame can claim 2^64 - 3
     * bytes. The frame cannot regenerate more than one maximum block per
     * 4 bytes of input, and decompression of a frame claiming more fails
     * anyway, so reject the claim before the caller allocates it (or, in
     * the Haskell ring path, converts it to a negative Int that slips
     * past the capacity check). */
    if (fcs / WF_ZSTD_BLOCK_MAX > src_len / WF_ZSTD_MIN_BLOCK + 1) {
        return (size_t) ZSTD_CONTENTSIZE_ERROR;
    }
    return (size_t) fcs;
}

int wf_zstd_decompress_into(const uint8_t *src, size_t src_len,
                            uint8_t *dst, size_t dst_cap,
                            size_t *out_produced) {
    if (out_produced) *out_produced = 0;

    size_t produced = ZSTD_decompress(dst, dst_cap, src, src_len);
    if (ZSTD_isError(produced)) {
        /* String-match against ZSTD_getErrorName because the typed
         * error-code API (ZSTD_getErrorCode / ZSTD_error_*) lives
         * behind the EXPERIMENTAL section of zstd.h and isn't
         * built into the standard libzstd distribution by default. */
        const char *name = ZSTD_getErrorName(produced);
        if (name && strstr(name, "Destination buffer is too small")) {
            return WF_DECOMPRESS_NEEDS_MORE_OUT;
        }
        return WF_DECOMPRESS_BAD_INPUT;
    }
    if (out_produced) *out_produced = produced;
    return WF_DECOMPRESS_OK;
}

/* =================================================================== *
 *  Snappy block format                                                 *
 * =================================================================== *
 *
 * 'snappy_ffi.c' already has 'snappy_decompress_into' +
 * 'snappy_get_uncompressed_length_wrapper'.  Nothing to add here.
 */
