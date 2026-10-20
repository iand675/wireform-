/* SIMD-accelerated scanners for HTTP/1.x message parsing.
 *
 * Each scanner returns the absolute offset (from the buffer base) of the
 * first byte satisfying the predicate, or `len` if none was found within
 * `[offset, len)`. This is the same convention used by
 * wireform-core / wireform-html.
 *
 * Build with -O3. On x86 we use SSE2 (universally available since 2003);
 * the scalar fallback is correct on every platform.
 *
 * Caller contract for the scanners: 0 <= offset and 0 <= len <= INT_MAX;
 * offset may exceed len (a miss: returns len without reading). Only
 * buf[offset .. len) is read; buf has no alignment requirement.
 */
#ifndef WIREFORM_HTTP1_SCAN_H
#define WIREFORM_HTTP1_SCAN_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Scan for CR (\r) starting at offset. Returns absolute offset or len. */
int hs_http1_find_cr(const void *buf, int offset, int len);

/* Scan for LF (\n) starting at offset. Returns absolute offset or len. */
int hs_http1_find_lf(const void *buf, int offset, int len);

/* Scan for the first byte that is NOT a valid HTTP token character
 * (RFC 9110 § 5.6.2 tchar). Used to locate the end of method, header
 * field name, and Transfer-Encoding coding name.
 *
 *   tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*"
 *         / "+" / "-" / "." / "^" / "_" / "`" / "|" / "~"
 *         / DIGIT / ALPHA
 *
 * Returns absolute offset of the first non-tchar byte, or len.
 */
int hs_http1_find_non_token(const void *buf, int offset, int len);

/* Scan for the first byte that is NOT a valid field-value byte
 * (RFC 9110 § 5.5 field-vchar / SP / HTAB). Anything else (NUL, CR, LF,
 * or other control char) terminates the field-value.
 *
 *   field-vchar = VCHAR / obs-text   (i.e. 0x21..0x7E / 0x80..0xFF)
 *
 * SP (0x20) and HTAB (0x09) are also permitted within the value.
 * Returns absolute offset of the first disallowed byte, or len.
 */
int hs_http1_find_non_fieldvalue(const void *buf, int offset, int len);

/* ASCII-lowercase a buffer into a caller-supplied destination. Used for
 * normalising HTTP header field names. SIMD-accelerated. src and dst are
 * `len` bytes each and either the same pointer (in place) or disjoint;
 * len <= 0 is a no-op. */
void hs_http1_to_lower_ascii(const void *src, void *dst, int len);

/* Compare two buffers for ASCII case-insensitive equality. Returns 1
 * on match, 0 otherwise. Both buffers are exactly `len` bytes; len <= 0
 * returns 1. */
int hs_http1_ascii_ieq(const void *a, const void *b, int len);

/* Parse hex digits at buf[offset .. len), stopping at the first non-hex
 * byte or at len.
 *
 *   Returns 1 when 1..16 digits were read: *out is the value and
 *   *consumed the digit count.
 *   Returns 0 when buf[offset] is not a hex digit (or offset >= len):
 *   *out = 0, *consumed = 0.
 *   Returns -1 when a 17th digit follows (leading zeros count too):
 *   *consumed = 16 and *out is left untouched.
 */
int hs_http1_parse_hex(const void *buf, int offset, int len, uint64_t *out, int *consumed);

#ifdef __cplusplus
}
#endif

#endif /* WIREFORM_HTTP1_SCAN_H */
