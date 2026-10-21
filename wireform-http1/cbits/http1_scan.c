/* SIMD-accelerated scanners for HTTP/1.x message parsing.
 *
 * The hot path here is "find the next delimiter" — CR, LF, end-of-token,
 * end-of-field-value — over a recv buffer. We use SSE2 to scan 16 bytes
 * at a time on x86, and fall back to a SWAR-style scalar loop otherwise.
 *
 * All return values use the "absolute offset or len-on-miss" convention.
 *
 * Offsets and the absolute end position `len` are ptrdiff_t; pure byte
 * counts (hs_http1_to_lower_ascii, hs_http1_ascii_ieq) are size_t. A
 * ByteString or recv ring larger than 2 GiB therefore scans correctly.
 * The hex parser's `consumed` is 0..16 and stays int.
 *
 * Block loops test `len - i >= 16`, not `i + 16 <= len`: with 0 <= i <= len
 * the difference cannot overflow (and `i + 16` can overflow ptrdiff_t for
 * a buffer near PTRDIFF_MAX, at which -O3 the wrapped sum would pass and
 * the next block be read past len).
 */

#include "http1_scan.h"

#include <stdint.h>
#include <string.h>

#if defined(__x86_64__) || defined(_M_X64) || defined(__i386__) || defined(_M_IX86)
#  include <emmintrin.h>
#  define WIREFORM_HTTP1_HAS_SSE2 1
#endif

/* ---------------------------------------------------------------------- *
 *  Byte-class tables                                                     *
 *                                                                        *
 *  is_tchar[b]   == 1 iff b is an RFC 9110 token char.                   *
 *  is_fv_ok[b]   == 1 iff b is a permitted field-value byte (vchar /     *
 *                  obs-text / SP / HTAB). NUL, CR, LF, and the other     *
 *                  controls are 0.                                       *
 *                                                                        *
 *  We index these on the scalar fallback path. The SSE2 path uses        *
 *  algebraic predicates instead so we don't pay for table loads in the   *
 *  hot loop.                                                             *
 * ---------------------------------------------------------------------- */

static const uint8_t is_tchar[256] = {
  /* 0x00..0x1F */
  0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
  0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
  /* 0x20..0x2F : SP ! " # $ % & ' ( ) * + , - . / */
  0,1,0,1, 1,1,1,1, 0,0,1,1, 0,1,1,0,
  /* 0x30..0x3F : 0-9 : ; < = > ? */
  1,1,1,1, 1,1,1,1, 1,1,0,0, 0,0,0,0,
  /* 0x40..0x4F : @ A-O */
  0,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,
  /* 0x50..0x5F : P-Z [ \ ] ^ _ */
  1,1,1,1, 1,1,1,1, 1,1,1,0, 0,0,1,1,
  /* 0x60..0x6F : ` a-o */
  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,
  /* 0x70..0x7F : p-z { | } ~ DEL */
  1,1,1,1, 1,1,1,1, 1,1,1,0, 1,0,1,0,
  /* 0x80..0xFF : not tchar (out of US-ASCII) */
  0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
  0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
  0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
  0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
  0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
  0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
  0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
  0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
};

static const uint8_t is_fv_ok[256] = {
  /* 0x00..0x08 : NUL .. BS — all bad */
  0,0,0,0, 0,0,0,0, 0,
  /* 0x09 HTAB OK; 0x0A LF bad; 0x0B 0x0C bad; 0x0D CR bad; 0x0E 0x0F bad */
  1, 0,0,0, 0,0,0,
  /* 0x10..0x1F — controls, all bad */
  0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
  /* 0x20..0x7E : SP + VCHAR, all OK; 0x7F DEL is bad (last entry) */
  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,
  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,
  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,
  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,
  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,
  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,0,
  /* 0x80..0xFF : obs-text — all OK */
  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,
  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,
  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,
  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,
  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,
  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,
  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,
  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,
};

/* ---------------------------------------------------------------------- *
 *  find_byte: single-target SSE2 scan (CR, LF).                          *
 * ---------------------------------------------------------------------- */

static inline ptrdiff_t find_single_byte_scalar(const uint8_t *p, ptrdiff_t offset, ptrdiff_t len, uint8_t needle) {
  for (ptrdiff_t i = offset; i < len; i++) {
    if (p[i] == needle) return i;
  }
  return len;
}

static inline ptrdiff_t find_single_byte(const uint8_t *p, ptrdiff_t offset, ptrdiff_t len, uint8_t needle) {
  ptrdiff_t i = offset;
#if WIREFORM_HTTP1_HAS_SSE2
  /* 16-byte SSE2 scan. */
  __m128i n = _mm_set1_epi8((char)needle);
  for (; len - i >= 16; i += 16) {
    __m128i v = _mm_loadu_si128((const __m128i *)(p + i));
    __m128i eq = _mm_cmpeq_epi8(v, n);
    int mask = _mm_movemask_epi8(eq);
    if (mask) return i + __builtin_ctz((unsigned)mask);
  }
#endif
  return find_single_byte_scalar(p, i, len, needle);
}

ptrdiff_t hs_http1_find_cr(const void *buf, ptrdiff_t offset, ptrdiff_t len) {
  return find_single_byte((const uint8_t *)buf, offset, len, 0x0d);
}

ptrdiff_t hs_http1_find_lf(const void *buf, ptrdiff_t offset, ptrdiff_t len) {
  return find_single_byte((const uint8_t *)buf, offset, len, 0x0a);
}

/* ---------------------------------------------------------------------- *
 *  find_non_token: stop at the first non-RFC9110-tchar byte.             *
 *                                                                        *
 *  We can express the non-tchar predicate as a small union of byte       *
 *  ranges, but the cleanest fast path is "load 16 bytes, classify each   *
 *  via the table, return the first 0". With 32-byte tables we could do   *
 *  this via PSHUFB; with 256-entry tables we have to use scalar loads.   *
 *  At 16 bytes that's still much better than 1 byte at a time because    *
 *  the inner loop is fully unrolled and branch-free.                     *
 * ---------------------------------------------------------------------- */

static inline ptrdiff_t find_non_token_scalar(const uint8_t *p, ptrdiff_t offset, ptrdiff_t len) {
  for (ptrdiff_t i = offset; i < len; i++) {
    if (!is_tchar[p[i]]) return i;
  }
  return len;
}

ptrdiff_t hs_http1_find_non_token(const void *buf, ptrdiff_t offset, ptrdiff_t len) {
  const uint8_t *p = (const uint8_t *)buf;
  ptrdiff_t i = offset;
  /* 16-byte unrolled scan over the byte-class table. */
  for (; len - i >= 16; i += 16) {
    uint8_t acc = 1;
    #pragma GCC unroll 16
    for (int j = 0; j < 16; j++) acc &= is_tchar[p[i + j]];
    if (!acc) {
      for (int j = 0; j < 16; j++) if (!is_tchar[p[i + j]]) return i + j;
    }
  }
  return find_non_token_scalar(p, i, len);
}

/* ---------------------------------------------------------------------- *
 *  find_non_fieldvalue: stop at NUL / CR / LF / other forbidden control. *
 *                                                                        *
 *  RFC 9110 § 5.5 says field-content = field-vchar [(SP/HTAB/field-vchar)
 *  field-vchar]. We treat HTAB and SP as legal; anything else < 0x20      *
 *  (other than HTAB) and 0x7F (DEL) is forbidden. 0x80+ is obs-text and  *
 *  legal.                                                                 *
 *                                                                        *
 *  This is the predicate that protects us from header smuggling via CR / *
 *  LF / NUL injection (RFC 9112 § 5.2).                                  *
 * ---------------------------------------------------------------------- */

static inline ptrdiff_t find_non_fv_scalar(const uint8_t *p, ptrdiff_t offset, ptrdiff_t len) {
  for (ptrdiff_t i = offset; i < len; i++) {
    if (!is_fv_ok[p[i]]) return i;
  }
  return len;
}

ptrdiff_t hs_http1_find_non_fieldvalue(const void *buf, ptrdiff_t offset, ptrdiff_t len) {
  const uint8_t *p = (const uint8_t *)buf;
  ptrdiff_t i = offset;
#if WIREFORM_HTTP1_HAS_SSE2
  /* SSE2 fast path: every forbidden byte is in [0..0x1F]\{0x09} or == 0x7F.
   * We test (b < 0x20 && b != 0x09) || b == 0x7F:
   *   forbidden_low  = b <u 0x20 && b != 0x09
   *   forbidden_del  = b == 0x7F
   *   forbidden      = forbidden_low | forbidden_del
   *
   * SSE2 has no unsigned cmplt, so we shift the comparison range via XOR
   * with 0x80 and use signed _mm_cmplt_epi8.
   */
  const __m128i v_2080  = _mm_set1_epi8((char)(0x80 ^ 0x20)); /* 0xa0 */
  const __m128i v_tab   = _mm_set1_epi8(0x09);
  const __m128i v_del   = _mm_set1_epi8(0x7f);
  const __m128i v_sign  = _mm_set1_epi8((char)0x80);
  for (; len - i >= 16; i += 16) {
    __m128i v = _mm_loadu_si128((const __m128i *)(p + i));
    __m128i v_signed = _mm_xor_si128(v, v_sign);                 /* signed-space */
    __m128i lt_20 = _mm_cmplt_epi8(v_signed, v_2080);            /* b <u 0x20 */
    __m128i not_tab = _mm_xor_si128(_mm_cmpeq_epi8(v, v_tab),
                                    _mm_set1_epi8((char)0xff));  /* b != 0x09 */
    __m128i forbid_low = _mm_and_si128(lt_20, not_tab);
    __m128i forbid_del = _mm_cmpeq_epi8(v, v_del);
    __m128i forbid     = _mm_or_si128(forbid_low, forbid_del);
    int mask = _mm_movemask_epi8(forbid);
    if (mask) return i + __builtin_ctz((unsigned)mask);
  }
#endif
  return find_non_fv_scalar(p, i, len);
}

/* ---------------------------------------------------------------------- *
 *  ASCII case-fold helpers.                                              *
 * ---------------------------------------------------------------------- */

void hs_http1_to_lower_ascii(const void *src_v, void *dst_v, size_t len) {
  const uint8_t *src = (const uint8_t *)src_v;
  uint8_t *dst = (uint8_t *)dst_v;
  size_t i = 0;
#if WIREFORM_HTTP1_HAS_SSE2
  /* For each byte, if it's in [A..Z] (0x41..0x5A) OR with 0x20 to lower.
   * SSE2: mask = (b >= 'A') & (b <= 'Z'); dst = src | (mask & 0x20). */
  const __m128i v_A = _mm_set1_epi8(0x41 - 0x80 - 1);     /* used with signed cmpgt: b > 0x40 ⇔ b >= 'A' */
  const __m128i v_Z = _mm_set1_epi8(0x5a - 0x80 + 1);     /* used with signed cmpgt: 0x5b > b ⇔ b <= 'Z' */
  const __m128i v_sign = _mm_set1_epi8((char)0x80);
  const __m128i v_diff = _mm_set1_epi8(0x20);
  for (; len - i >= 16; i += 16) {
    __m128i v = _mm_loadu_si128((const __m128i *)(src + i));
    __m128i vs = _mm_xor_si128(v, v_sign);
    __m128i ge_A = _mm_cmpgt_epi8(vs, v_A);
    __m128i le_Z = _mm_cmpgt_epi8(v_Z, vs);
    __m128i is_upper = _mm_and_si128(ge_A, le_Z);
    __m128i add = _mm_and_si128(is_upper, v_diff);
    __m128i res = _mm_or_si128(v, add);
    _mm_storeu_si128((__m128i *)(dst + i), res);
  }
#endif
  for (; i < len; i++) {
    uint8_t b = src[i];
    if (b >= 0x41 && b <= 0x5a) b |= 0x20;
    dst[i] = b;
  }
}

int hs_http1_ascii_ieq(const void *a_v, const void *b_v, size_t len) {
  const uint8_t *a = (const uint8_t *)a_v;
  const uint8_t *b = (const uint8_t *)b_v;
  size_t i = 0;
#if WIREFORM_HTTP1_HAS_SSE2
  /* Compare lowercased(a) == lowercased(b) in 16-byte chunks.
   * Lowercasing a byte:
   *   lo = b | ((b in [A..Z]) ? 0x20 : 0x00)
   */
  const __m128i v_A     = _mm_set1_epi8(0x41 - 0x80 - 1);
  const __m128i v_Z     = _mm_set1_epi8(0x5a - 0x80 + 1);
  const __m128i v_sign  = _mm_set1_epi8((char)0x80);
  const __m128i v_diff  = _mm_set1_epi8(0x20);
  for (; len - i >= 16; i += 16) {
    __m128i va = _mm_loadu_si128((const __m128i *)(a + i));
    __m128i vb = _mm_loadu_si128((const __m128i *)(b + i));
    __m128i vas = _mm_xor_si128(va, v_sign);
    __m128i vbs = _mm_xor_si128(vb, v_sign);
    __m128i ua = _mm_and_si128(_mm_and_si128(_mm_cmpgt_epi8(vas, v_A), _mm_cmpgt_epi8(v_Z, vas)), v_diff);
    __m128i ub = _mm_and_si128(_mm_and_si128(_mm_cmpgt_epi8(vbs, v_A), _mm_cmpgt_epi8(v_Z, vbs)), v_diff);
    __m128i la = _mm_or_si128(va, ua);
    __m128i lb = _mm_or_si128(vb, ub);
    __m128i eq = _mm_cmpeq_epi8(la, lb);
    int mask = _mm_movemask_epi8(eq);
    if (mask != 0xffff) return 0;
  }
#endif
  for (; i < len; i++) {
    uint8_t aa = a[i], bb = b[i];
    if (aa >= 0x41 && aa <= 0x5a) aa |= 0x20;
    if (bb >= 0x41 && bb <= 0x5a) bb |= 0x20;
    if (aa != bb) return 0;
  }
  return 1;
}

/* ---------------------------------------------------------------------- *
 *  Hex parser for chunked transfer-encoding chunk sizes.                 *
 *                                                                        *
 *  Reads up to 16 hex digits (Word64) from buf[offset..len). Stops at    *
 *  the first non-hex byte. Returns 1 on success (>= 1 digit consumed),   *
 *  0 if no hex digit at the start, -1 on overflow.                       *
 *                                                                        *
 *  RFC 9112 § 7.1: chunk-size = 1*HEXDIG.                                *
 * ---------------------------------------------------------------------- */

int hs_http1_parse_hex(const void *buf_v, ptrdiff_t offset, ptrdiff_t len, uint64_t *out, int *consumed) {
  const uint8_t *buf = (const uint8_t *)buf_v;
  uint64_t value = 0;
  ptrdiff_t i = offset;
  int n = 0;
  while (i < len) {
    uint8_t b = buf[i];
    uint8_t d;
    if      (b >= '0' && b <= '9') d = b - '0';
    else if (b >= 'a' && b <= 'f') d = b - 'a' + 10;
    else if (b >= 'A' && b <= 'F') d = b - 'A' + 10;
    else break;
    if (n >= 16) { *consumed = n; return -1; }
    value = (value << 4) | d;
    i++;
    n++;
  }
  *consumed = n;
  *out = value;
  return n > 0 ? 1 : 0;
}
