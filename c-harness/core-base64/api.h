/* Kernel API as wireform-core/src/Wireform/Base64.hs imports it, plus the
 * scalar reference implementations the tests compare against.
 *
 * Caller contract (from Wireform.Base64):
 *
 *   hs_base64_encoded_length(n)  pure. n < 0 -> 0. For
 *       0 <= n <= WF_B64_MAX_ENCODE_INPUT returns 4 * ceil(n / 3); larger n
 *       (whose encoding does not fit in an int) -> -1.
 *       encodeBase64 refuses inputs above the limit before calling.
 *   hs_base64_decoded_max_length(n)  pure. n < 0 -> 0, else (n / 4) * 3.
 *   hs_base64_encode(in, n, out)  encodeBase64: `in` holds n readable
 *       bytes (the ByteString payload, any alignment, may end at an
 *       arbitrary address), `out` holds exactly hs_base64_encoded_length(n)
 *       writable bytes (BSI.create). Writes exactly that many bytes and
 *       returns the count. n <= 0 -> 0 and touches nothing; n above
 *       WF_B64_MAX_ENCODE_INPUT -> -1 and touches nothing.
 *   hs_base64_decode(in, n, out)  decodeBase64: `in` holds n readable bytes
 *       of untrusted text, `out` holds exactly hs_base64_decoded_max_length(n)
 *       writable bytes (mallocByteString). Strict RFC 4648 sec 4, standard
 *       alphabet, no whitespace: n must be a multiple of 4, every byte must be
 *       in the alphabet except '=' as "xx==" or "xxx=" in the final quartet.
 *       Returns the decoded length, or -1 on malformed input (out may then
 *       hold partial output). Never writes past the decoded length, which is
 *       <= hs_base64_decoded_max_length(n). Non-zero pad bits in the final
 *       quartet ("QR==") are accepted (RFC 4648 sec 3.5 makes rejecting them
 *       optional); the reference below mirrors that. n == 0 -> 0, n < 0 or
 *       n % 4 != 0 -> -1, both without touching memory.
 *   The Haskell wrappers never pass n > INT_MAX: decodeBase64 rejects
 *   longer inputs and encodeBase64 inputs above WF_B64_MAX_ENCODE_INPUT.
 */
#ifndef CORE_BASE64_API_H
#define CORE_BASE64_API_H

#include <limits.h>
#include <stdint.h>
#include <string.h>

/* 3 * (INT_MAX / 4): the largest input whose padded encoding fits an int. */
#define WF_B64_MAX_ENCODE_INPUT (3 * (INT_MAX / 4))

int hs_base64_encoded_length(int in_len);
int hs_base64_decoded_max_length(int in_len);
int hs_base64_encode(const uint8_t *in, int in_len, uint8_t *out);
int hs_base64_decode(const uint8_t *in, int in_len, uint8_t *out);

static const char ref_b64_alpha[] =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

static inline long long ref_b64_encoded_length(long long n)
{
    return n < 0 ? 0 : (n + 2) / 3 * 4;
}

/* Bit-serial encoder: emit one sextet per 6 bits of the input bit stream,
 * zero-filled at the end, then '=' up to a multiple of 4 characters. */
static inline size_t ref_b64_encode(const uint8_t *in, size_t n, uint8_t *out)
{
    size_t bits = n * 8, o = 0;
    for (size_t b = 0; b < bits; b += 6) {
        unsigned v = 0;
        for (size_t k = 0; k < 6; k++) {
            size_t bit = b + k;
            unsigned x = bit < bits ? (in[bit / 8] >> (7 - bit % 8)) & 1u : 0u;
            v = (v << 1) | x;
        }
        out[o++] = (uint8_t)ref_b64_alpha[v];
    }
    while (o % 4) out[o++] = '=';
    return o;
}

/* Sextet value of c, or -1. */
static inline int ref_b64_value(uint8_t c)
{
    if (c == 0) return -1;
    const char *p = memchr(ref_b64_alpha, c, 64);
    return p ? (int)(p - ref_b64_alpha) : -1;
}

/* Strict decoder written from the spec, independent of the kernel's
 * structure: validate the whole text first, then decode bit-serially.
 * Returns the decoded length or -1. */
static inline long ref_b64_decode(const uint8_t *in, long n, uint8_t *out)
{
    if (n < 0 || n % 4) return -1;
    if (n == 0) return 0;
    long pad = 0;
    if (in[n - 1] == '=') pad = (in[n - 2] == '=') ? 2 : 1;
    for (long i = 0; i < n - pad; i++) {
        if (ref_b64_value(in[i]) < 0) return -1;
    }
    long nsext = n - pad;
    long outn = nsext * 6 / 8;
    memset(out, 0, (size_t)outn);
    for (long s = 0; s < nsext; s++) {
        int v = ref_b64_value(in[s]);
        for (int k = 0; k < 6; k++) {
            long bit = s * 6 + k;
            if (bit / 8 >= outn) break;   /* pad bits: ignored */
            if ((v >> (5 - k)) & 1) out[bit / 8] |= (uint8_t)(0x80u >> (bit % 8));
        }
    }
    return outn;
}

#endif
