#ifndef WIREFORM_HPACK_HUFFMAN_H
#define WIREFORM_HPACK_HUFFMAN_H

#include <stdint.h>
#include <stddef.h>

/* Huffman-codes src[0..len) with EOS-prefix padding. dst needs
 * wireform_hpack_huffman_encode_len(src, len) bytes (4 * len + 1 always
 * suffices); returns the bytes written, equal to that length. */
size_t wireform_hpack_huffman_encode(
    const uint8_t *src, size_t len,
    uint8_t *dst);

/* Exact encoded size of src[0..len) in bytes. */
size_t wireform_hpack_huffman_encode_len(
    const uint8_t *src, size_t len);

/* Decodes src[0..src_len) into dst[0..dst_cap). Returns 0 and the decoded
 * length in *out_len, or -1 on an invalid encoding (EOS in the string,
 * padding longer than 7 bits or not all ones) or when the output would
 * exceed dst_cap. Never writes past dst_cap; always sets *out_len. A valid
 * input decodes to at most floor(8 * src_len / 5) bytes. */
int wireform_hpack_huffman_decode(
    const uint8_t *src, size_t src_len,
    uint8_t *dst, size_t dst_cap,
    size_t *out_len);

#endif
