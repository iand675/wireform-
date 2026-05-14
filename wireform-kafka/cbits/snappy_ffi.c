/*
 * snappy_ffi.c - FFI bindings for Snappy compression
 *
 * Provides a simple C interface to the Snappy compression library
 * for use from Haskell.
 */

#include <snappy-c.h>
#include <stdlib.h>
#include <string.h>

/*
 * Compress data using Snappy.
 *
 * Returns the compressed size, or 0 on error.
 * The caller must free the output buffer.
 */
size_t snappy_compress_wrapper(const char* input, size_t input_length,
                                char** output, size_t* output_length) {
    if (!input || !output || !output_length) {
        return 0;
    }

    // Get the maximum compressed length
    size_t max_compressed_length = snappy_max_compressed_length(input_length);

    // Allocate output buffer
    *output = (char*)malloc(max_compressed_length);
    if (!*output) {
        return 0;
    }

    // snappy_compress expects *output_length to be initialised to the
    // capacity of the output buffer; on success it overwrites it with
    // the actual compressed length. Without this initialisation snappy
    // sees output_length = 0 and refuses to write anything (returns
    // SNAPPY_BUFFER_TOO_SMALL).
    *output_length = max_compressed_length;

    // Compress the data
    snappy_status status = snappy_compress(input, input_length,
                                           *output, output_length);

    if (status != SNAPPY_OK) {
        free(*output);
        *output = NULL;
        return 0;
    }

    return *output_length;
}

/*
 * Decompress Snappy-compressed data.
 *
 * Returns the decompressed size, or 0 on error.
 * The caller must free the output buffer.
 */
size_t snappy_decompress_wrapper(const char* input, size_t input_length,
                                  char** output, size_t* output_length) {
    if (!input || !output || !output_length) {
        return 0;
    }

    // Get the uncompressed length
    snappy_status status = snappy_uncompressed_length(input, input_length,
                                                      output_length);
    if (status != SNAPPY_OK) {
        return 0;
    }

    // Allocate output buffer
    *output = (char*)malloc(*output_length);
    if (!*output) {
        return 0;
    }

    // Decompress the data
    status = snappy_uncompress(input, input_length, *output, output_length);

    if (status != SNAPPY_OK) {
        free(*output);
        *output = NULL;
        return 0;
    }

    return *output_length;
}
