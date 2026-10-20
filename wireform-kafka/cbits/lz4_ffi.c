/*
 * lz4_ffi.c - FFI bindings for LZ4 compression
 *
 * Provides a simple C interface to the LZ4 compression library
 * for use from Haskell. Uses the LZ4 frame format.
 */

#include <lz4frame.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/* Forward declaration */
size_t lz4_compress_wrapper_level(const char* input, size_t input_length,
                                   char** output, size_t* output_length,
                                   int compression_level);

/*
 * Compress data using LZ4 frame format with default compression level.
 * 
 * Returns the compressed size, or 0 on error.
 * The caller must free the output buffer.
 */
size_t lz4_compress_wrapper(const char* input, size_t input_length,
                             char** output, size_t* output_length) {
    return lz4_compress_wrapper_level(input, input_length, output, output_length, 0);
}

/*
 * Compress data using LZ4 frame format with specified compression level.
 * Level 0 = fast mode (default), levels 1-16 = high compression mode.
 * 
 * Returns the compressed size, or 0 on error.
 * The caller must free the output buffer.
 */
size_t lz4_compress_wrapper_level(const char* input, size_t input_length,
                                   char** output, size_t* output_length,
                                   int compression_level) {
    if (!input || !output || !output_length) {
        return 0;
    }
    
    // Setup compression preferences
    LZ4F_preferences_t preferences;
    memset(&preferences, 0, sizeof(preferences));
    
    // Set compression level
    // Level 0 = default fast mode
    // Levels 1-16 = high compression mode (mapped to LZ4HC levels)
    if (compression_level == 0) {
        preferences.compressionLevel = 0;  // Fast mode
    } else if (compression_level >= 1 && compression_level <= 16) {
        preferences.compressionLevel = compression_level;  // High compression
    } else {
        // Invalid level, use default
        preferences.compressionLevel = 0;
    }
    
    // Get the maximum compressed length (includes frame header/footer)
    size_t max_compressed_length = LZ4F_compressFrameBound(input_length, &preferences);
    
    // Allocate output buffer
    *output = (char*)malloc(max_compressed_length);
    if (!*output) {
        return 0;
    }
    
    // Compress the data using the frame API with preferences
    size_t result = LZ4F_compressFrame(*output, max_compressed_length,
                                       input, input_length, &preferences);
    
    if (LZ4F_isError(result)) {
        free(*output);
        *output = NULL;
        return 0;
    }
    
    *output_length = result;
    return result;
}

/*
 * Run one LZ4 frame from src into dst[0, cap). Returns 0 when the frame
 * is complete, 1 when dst filled up first, -1 on malformed or truncated
 * input. *produced is the number of bytes written.
 */
static int lz4f_run(LZ4F_dctx* dctx, char* dst, size_t cap,
                    const char* src, size_t len, size_t* produced) {
    size_t in_off = 0, out_off = 0;
    for (;;) {
        size_t d = cap - out_off, s = len - in_off;
        size_t hint = LZ4F_decompress(dctx, dst + out_off, &d,
                                      src + in_off, &s, NULL);
        if (LZ4F_isError(hint)) {
            return -1;
        }
        in_off += s;
        out_off += d;
        *produced = out_off;
        if (hint == 0) {
            return 0;
        }
        if (out_off == cap) {
            return 1;
        }
        if (in_off == len || (s == 0 && d == 0)) {
            return -1;  /* input ran out mid-frame */
        }
    }
}

/*
 * Decompress LZ4-compressed data (frame format).
 *
 * Returns the decompressed size, or 0 on error. On success *output is a
 * malloc'd buffer the caller must free. A frame that decompresses to
 * zero bytes also returns 0, with *output = NULL (nothing to free).
 *
 * The output buffer starts at max(4 * input, 64 KiB) and doubles until
 * the frame fits. Each attempt restarts the frame in a fresh buffer:
 * with linked blocks the decoder keeps pointers into earlier output, so
 * the buffer cannot be realloc'd under it.
 */
size_t lz4_decompress_wrapper(const char* input, size_t input_length,
                               char** output, size_t* output_length) {
    if (!input || !output || !output_length) {
        return 0;
    }
    *output = NULL;

    LZ4F_dctx* dctx;
    LZ4F_errorCode_t err = LZ4F_createDecompressionContext(&dctx, LZ4F_VERSION);
    if (LZ4F_isError(err)) {
        return 0;
    }

    size_t cap = input_length <= SIZE_MAX / 4 ? input_length * 4 : SIZE_MAX;
    if (cap < 64 * 1024) {
        cap = 64 * 1024;
    }
    char* buf = NULL;
    size_t produced = 0;
    int rc;
    for (;;) {
        buf = (char*)malloc(cap);
        if (!buf) {
            rc = -1;
            break;
        }
        rc = lz4f_run(dctx, buf, cap, input, input_length, &produced);
        if (rc != 1) {
            break;
        }
        free(buf);
        buf = NULL;
        if (cap > SIZE_MAX / 2) {
            rc = -1;
            break;
        }
        cap *= 2;
        LZ4F_resetDecompressionContext(dctx);
    }

    LZ4F_freeDecompressionContext(dctx);

    if (rc != 0 || produced == 0) {
        free(buf);
        return 0;
    }
    *output = buf;
    *output_length = produced;
    return produced;
}

