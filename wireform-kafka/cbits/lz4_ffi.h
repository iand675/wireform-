/*
 * lz4_ffi.h - FFI bindings for LZ4 compression
 */

#ifndef LZ4_FFI_H
#define LZ4_FFI_H

#include <stddef.h>

/*
 * Compress data using LZ4 frame format with default compression level.
 * Returns the compressed size, or 0 on error.
 * The caller must free the output buffer.
 */
size_t lz4_compress_wrapper(const char* input, size_t input_length,
                             char** output, size_t* output_length);

/*
 * Compress data using LZ4 frame format with specified compression level.
 * Level 0 = fast mode (default), levels 1-16 = high compression mode.
 * Returns the compressed size, or 0 on error.
 * The caller must free the output buffer.
 */
size_t lz4_compress_wrapper_level(const char* input, size_t input_length,
                                   char** output, size_t* output_length,
                                   int compression_level);

/*
 * Decompress LZ4-compressed data (frame format).
 * Returns the decompressed size, or 0 on error.
 * The caller must free the output buffer.
 */
size_t lz4_decompress_wrapper(const char* input, size_t input_length,
                               char** output, size_t* output_length);

#endif /* LZ4_FFI_H */
