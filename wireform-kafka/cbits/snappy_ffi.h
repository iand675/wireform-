/*
 * snappy_ffi.h - FFI bindings for Snappy compression
 */

#ifndef SNAPPY_FFI_H
#define SNAPPY_FFI_H

#include <stddef.h>

/*
 * Compress data using Snappy.
 * Returns the compressed size, or 0 on error.
 * The caller must free the output buffer.
 */
size_t snappy_compress_wrapper(const char* input, size_t input_length,
                                char** output, size_t* output_length);

/*
 * Decompress Snappy-compressed data.
 * Returns the decompressed size, or 0 on error.
 * The caller must free the output buffer.
 */
size_t snappy_decompress_wrapper(const char* input, size_t input_length,
                                  char** output, size_t* output_length);

#endif /* SNAPPY_FFI_H */
