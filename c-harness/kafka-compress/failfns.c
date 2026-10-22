/*
 * failfns.c - see failfns.h. One translation unit per program (via
 * kafka-compress_EXTRA) defines the fakes. The command-line renames
 * are undefined first, so the pass-through calls reach the real
 * library functions under their own names, while every other
 * translation unit (the kernels) keeps the renamed calls.
 */
#undef inflateInit2_
#undef LZ4F_compressFrame
#undef LZ4F_createDecompressionContext
#undef LZ4F_decompress
#undef snappy_compress
#undef ZSTD_getErrorName

#include "failfns.h"
#include "wf_fail.h"

/* The force-included headers above saw the command-line renames, so
 * they declared the fakes' names instead of the real library entry
 * points. Declare the real ones under their own names for the
 * pass-through calls. */
extern int inflateInit2_(z_streamp strm, int windowBits,
                         const char *version, int stream_size);
extern size_t LZ4F_createDecompressionContext(LZ4F_dctx **ctxPtr,
                                              unsigned versionNumber);
extern size_t LZ4F_compressFrame(void *dstBuffer, size_t dstCapacity,
                                 const void *srcBuffer, size_t srcSize,
                                 const LZ4F_preferences_t *preferencesPtr);
extern size_t LZ4F_decompress(LZ4F_dctx *dctx, void *dstBuf, size_t *dstSizePtr,
                              const void *srcBuf, size_t *srcSizePtr,
                              const LZ4F_decompressOptions_t *optionsPtr);
extern snappy_status snappy_compress(const char *input, size_t input_length,
                                     char *compressed, size_t *compressed_length);
extern const char *ZSTD_getErrorName(size_t code);

int wf_fail_inflateInit2_(z_streamp strm, int windowBits,
                          const char *version, int stream_size)
{
    if (wf_failpoint("inflateInit2")) return Z_MEM_ERROR;
    return inflateInit2_(strm, windowBits, version, stream_size);
}

size_t wf_fail_LZ4F_createDecompressionContext(LZ4F_dctx **ctxPtr,
                                               unsigned versionNumber)
{
    if (wf_failpoint("LZ4F_createDecompressionContext")) return (size_t)-1;
    return LZ4F_createDecompressionContext(ctxPtr, versionNumber);
}

size_t wf_fail_LZ4F_compressFrame(void *dstBuffer, size_t dstCapacity,
                                  const void *srcBuffer, size_t srcSize,
                                  const LZ4F_preferences_t *preferencesPtr)
{
    if (wf_failpoint("LZ4F_compressFrame")) return (size_t)-1;
    return LZ4F_compressFrame(dstBuffer, dstCapacity, srcBuffer, srcSize,
                              preferencesPtr);
}

size_t wf_fail_LZ4F_decompress(LZ4F_dctx *dctx, void *dstBuf, size_t *dstSizePtr,
                               const void *srcBuf, size_t *srcSizePtr,
                               const LZ4F_decompressOptions_t *optionsPtr)
{
    if (wf_failpoint("LZ4F_decompress_no_progress")) {
        /* Consume nothing, produce nothing, still wants input. */
        *srcSizePtr = 0;
        *dstSizePtr = 0;
        return 1;
    }
    if (wf_failpoint("LZ4F_decompress_full")) {
        /* Claim to have filled the output buffer without writing it;
         * *dstSizePtr is the capacity on entry and stays that way. */
        *srcSizePtr = 0;
        return 1;
    }
    if (wf_failpoint("LZ4F_decompress_partial_out")) {
        /* Claim to have produced one byte without consuming input:
         * drives the callers' no-progress disjuncts past the "consumed
         * nothing, produced nothing" case. */
        *srcSizePtr = 0;
        *dstSizePtr = 1;
        return 1;
    }
    if (wf_failpoint("LZ4F_decompress_partial_in")) {
        /* Consume one byte, produce nothing, still wants input: the
         * mid-frame "made some progress but is not done" case. */
        *srcSizePtr = 1;
        *dstSizePtr = 0;
        return 1;
    }
    return LZ4F_decompress(dctx, dstBuf, dstSizePtr, srcBuf, srcSizePtr,
                           optionsPtr);
}

snappy_status wf_fail_snappy_compress(const char *input, size_t input_length,
                                      char *compressed, size_t *compressed_length)
{
    if (wf_failpoint("snappy_compress")) return SNAPPY_BUFFER_TOO_SMALL;
    return snappy_compress(input, input_length, compressed, compressed_length);
}

const char *wf_fail_ZSTD_getErrorName(size_t code)
{
    /* libzstd documents a never-NULL result; the callers still guard,
     * and this failpoint drives that guard. */
    if (wf_failpoint("ZSTD_getErrorName")) return NULL;
    return ZSTD_getErrorName(code);
}
