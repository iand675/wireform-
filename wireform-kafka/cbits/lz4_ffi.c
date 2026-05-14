/*
 * lz4_ffi.c - FFI bindings for LZ4 compression
 *
 * Provides a simple C interface to the LZ4 compression library
 * for use from Haskell. Uses the LZ4 frame format.
 */

#include <lz4frame.h>
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
 * Decompress LZ4-compressed data (frame format).
 *
 * Returns the decompressed size, or 0 on error.
 * The caller must free the output buffer.
 */
size_t lz4_decompress_wrapper(const char* input, size_t input_length,
                               char** output, size_t* output_length) {
    if (!input || !output || !output_length) {
        return 0;
    }

    LZ4F_dctx* dctx;
    LZ4F_errorCode_t err = LZ4F_createDecompressionContext(&dctx, LZ4F_VERSION);
    if (LZ4F_isError(err)) {
        return 0;
    }

    // Start with a reasonable buffer size (try 4x input size as heuristic)
    size_t output_capacity = input_length * 4;
    if (output_capacity < 64 * 1024) {
        output_capacity = 64 * 1024; // At least 64KB
    }

    *output = (char*)malloc(output_capacity);
    if (!*output) {
        LZ4F_freeDecompressionContext(dctx);
        return 0;
    }

    // Decompress
    size_t src_size = input_length;
    size_t dst_size = output_capacity;
    size_t result = LZ4F_decompress(dctx, *output, &dst_size,
                                     input, &src_size, NULL);

    LZ4F_freeDecompressionContext(dctx);

    if (LZ4F_isError(result)) {
        free(*output);
        *output = NULL;
        return 0;
    }

    *output_length = dst_size;
    return dst_size;
}
