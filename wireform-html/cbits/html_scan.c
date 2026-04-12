#include <stdint.h>
#include <stddef.h>

#if defined(__aarch64__) || defined(_M_ARM64)
#include <arm_neon.h>
#define USE_NEON 1
#elif defined(__SSE2__)
#include <emmintrin.h>
#define USE_SSE2 1
#endif

/* Scan forward from buf+off looking for <, &, \0, or \r.
   Returns the byte offset of the first match, or 'end' if none found.
   Precondition: off <= end, end <= buflen. */
ptrdiff_t wireform_scan_text(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end) {
    const uint8_t *p = buf + off;
    const uint8_t *pend = buf + end;

#if USE_NEON
    {
        const uint8x16_t v_lt = vdupq_n_u8(0x3C);   /* < */
        const uint8x16_t v_amp = vdupq_n_u8(0x26);   /* & */
        const uint8x16_t v_nul = vdupq_n_u8(0x00);   /* \0 */
        const uint8x16_t v_cr = vdupq_n_u8(0x0D);    /* \r */

        while (p + 16 <= pend) {
            uint8x16_t chunk = vld1q_u8(p);
            uint8x16_t hit = vorrq_u8(
                vorrq_u8(vceqq_u8(chunk, v_lt), vceqq_u8(chunk, v_amp)),
                vorrq_u8(vceqq_u8(chunk, v_nul), vceqq_u8(chunk, v_cr)));
            uint64x2_t hit64 = vreinterpretq_u64_u8(hit);
            uint64_t lo = vgetq_lane_u64(hit64, 0);
            uint64_t hi = vgetq_lane_u64(hit64, 1);
            if (lo) {
                return (ptrdiff_t)(p - buf) + (__builtin_ctzll(lo) >> 3);
            }
            if (hi) {
                return (ptrdiff_t)(p - buf) + 8 + (__builtin_ctzll(hi) >> 3);
            }
            p += 16;
        }
    }
#elif USE_SSE2
    {
        const __m128i v_lt = _mm_set1_epi8(0x3C);
        const __m128i v_amp = _mm_set1_epi8(0x26);
        const __m128i v_nul = _mm_setzero_si128();
        const __m128i v_cr = _mm_set1_epi8(0x0D);

        while (p + 16 <= pend) {
            __m128i chunk = _mm_loadu_si128((const __m128i *)p);
            __m128i hit = _mm_or_si128(
                _mm_or_si128(_mm_cmpeq_epi8(chunk, v_lt), _mm_cmpeq_epi8(chunk, v_amp)),
                _mm_or_si128(_mm_cmpeq_epi8(chunk, v_nul), _mm_cmpeq_epi8(chunk, v_cr)));
            int mask = _mm_movemask_epi8(hit);
            if (mask) {
                return (ptrdiff_t)(p - buf) + __builtin_ctz(mask);
            }
            p += 16;
        }
    }
#endif

    /* Scalar tail */
    while (p < pend) {
        uint8_t b = *p;
        if (b == 0x3C || b == 0x26 || b == 0x00 || b == 0x0D)
            return (ptrdiff_t)(p - buf);
        p++;
    }
    return end;
}

/* Fused scan_text + is_all_ascii.
   Returns (end_pos << 1) | all_ascii_bit.
   Scans for <, &, \0, \r while simultaneously tracking high-byte presence.
   The ASCII flag is conservative: may report non-ASCII if a high byte
   appears anywhere in the same SIMD chunk as the terminator. */
ptrdiff_t wireform_scan_text_ascii(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end) {
    const uint8_t *p = buf + off;
    const uint8_t *pend = buf + end;
    int all_ascii = 1;

#if USE_NEON
    {
        const uint8x16_t v_lt  = vdupq_n_u8(0x3C);
        const uint8x16_t v_amp = vdupq_n_u8(0x26);
        const uint8x16_t v_nul = vdupq_n_u8(0x00);
        const uint8x16_t v_cr  = vdupq_n_u8(0x0D);
        const uint8x16_t v_hi  = vdupq_n_u8(0x80);
        uint8x16_t hi_acc = vdupq_n_u8(0);

        while (p + 16 <= pend) {
            uint8x16_t chunk = vld1q_u8(p);
            hi_acc = vorrq_u8(hi_acc, vandq_u8(chunk, v_hi));
            uint8x16_t hit = vorrq_u8(
                vorrq_u8(vceqq_u8(chunk, v_lt), vceqq_u8(chunk, v_amp)),
                vorrq_u8(vceqq_u8(chunk, v_nul), vceqq_u8(chunk, v_cr)));
            uint64x2_t hit64 = vreinterpretq_u64_u8(hit);
            uint64_t lo = vgetq_lane_u64(hit64, 0);
            uint64_t hi = vgetq_lane_u64(hit64, 1);
            if (lo) {
                ptrdiff_t pos = (ptrdiff_t)(p - buf) + (__builtin_ctzll(lo) >> 3);
                if (vmaxvq_u8(hi_acc) != 0) all_ascii = 0;
                return (pos << 1) | all_ascii;
            }
            if (hi) {
                ptrdiff_t pos = (ptrdiff_t)(p - buf) + 8 + (__builtin_ctzll(hi) >> 3);
                if (vmaxvq_u8(hi_acc) != 0) all_ascii = 0;
                return (pos << 1) | all_ascii;
            }
            p += 16;
        }
        if (vmaxvq_u8(hi_acc) != 0) all_ascii = 0;
    }
#elif USE_SSE2
    {
        const __m128i v_lt  = _mm_set1_epi8(0x3C);
        const __m128i v_amp = _mm_set1_epi8(0x26);
        const __m128i v_nul = _mm_setzero_si128();
        const __m128i v_cr  = _mm_set1_epi8(0x0D);
        __m128i hi_acc = _mm_setzero_si128();

        while (p + 16 <= pend) {
            __m128i chunk = _mm_loadu_si128((const __m128i *)p);
            hi_acc = _mm_or_si128(hi_acc, chunk);
            __m128i hit = _mm_or_si128(
                _mm_or_si128(_mm_cmpeq_epi8(chunk, v_lt), _mm_cmpeq_epi8(chunk, v_amp)),
                _mm_or_si128(_mm_cmpeq_epi8(chunk, v_nul), _mm_cmpeq_epi8(chunk, v_cr)));
            int mask = _mm_movemask_epi8(hit);
            if (mask) {
                ptrdiff_t pos = (ptrdiff_t)(p - buf) + __builtin_ctz(mask);
                if (_mm_movemask_epi8(hi_acc)) all_ascii = 0;
                return (pos << 1) | all_ascii;
            }
            p += 16;
        }
        if (_mm_movemask_epi8(hi_acc)) all_ascii = 0;
    }
#endif

    while (p < pend) {
        uint8_t b = *p;
        if (b == 0x3C || b == 0x26 || b == 0x00 || b == 0x0D) {
            return ((ptrdiff_t)(p - buf) << 1) | all_ascii;
        }
        if (b >= 0x80) all_ascii = 0;
        p++;
    }
    return (end << 1) | all_ascii;
}

/* Scan for end of tag name. Returns offset of first non-tagname byte. */
ptrdiff_t wireform_scan_tagname(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end) {
    const uint8_t *p = buf + off;
    const uint8_t *pend = buf + end;
    while (p < pend) {
        uint8_t b = *p;
        if ((b >= 'a' && b <= 'z') || (b >= 'A' && b <= 'Z')
            || (b >= '0' && b <= '9')
            || b == '-' || b == '_' || b == ':' || b == '.' || b == '<') {
            p++;
        } else {
            break;
        }
    }
    return (ptrdiff_t)(p - buf);
}

/* Scan for closing double-quote or ampersand in an attribute value.
   Returns offset of first " or & found, or 'end' if none. */
ptrdiff_t wireform_scan_dquote(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end) {
    const uint8_t *p = buf + off;
    const uint8_t *pend = buf + end;

#if USE_NEON
    {
        const uint8x16_t v_dq  = vdupq_n_u8(0x22);
        const uint8x16_t v_amp = vdupq_n_u8(0x26);
        while (p + 16 <= pend) {
            uint8x16_t chunk = vld1q_u8(p);
            uint8x16_t hit = vorrq_u8(vceqq_u8(chunk, v_dq), vceqq_u8(chunk, v_amp));
            uint64x2_t hit64 = vreinterpretq_u64_u8(hit);
            uint64_t lo = vgetq_lane_u64(hit64, 0);
            uint64_t hi = vgetq_lane_u64(hit64, 1);
            if (lo) return (ptrdiff_t)(p - buf) + (__builtin_ctzll(lo) >> 3);
            if (hi) return (ptrdiff_t)(p - buf) + 8 + (__builtin_ctzll(hi) >> 3);
            p += 16;
        }
    }
#elif USE_SSE2
    {
        const __m128i v_dq  = _mm_set1_epi8(0x22);
        const __m128i v_amp = _mm_set1_epi8(0x26);
        while (p + 16 <= pend) {
            __m128i chunk = _mm_loadu_si128((const __m128i *)p);
            __m128i hit = _mm_or_si128(_mm_cmpeq_epi8(chunk, v_dq), _mm_cmpeq_epi8(chunk, v_amp));
            int mask = _mm_movemask_epi8(hit);
            if (mask) return (ptrdiff_t)(p - buf) + __builtin_ctz(mask);
            p += 16;
        }
    }
#endif

    while (p < pend) {
        if (*p == 0x22 || *p == 0x26) return (ptrdiff_t)(p - buf);
        p++;
    }
    return end;
}

/* Fused scan_dquote + ASCII check. Returns (pos << 1) | ascii_flag. */
ptrdiff_t wireform_scan_dquote_ascii(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end) {
    const uint8_t *p = buf + off;
    const uint8_t *pend = buf + end;
    int all_ascii = 1;

#if USE_NEON
    {
        const uint8x16_t v_dq  = vdupq_n_u8(0x22);
        const uint8x16_t v_amp = vdupq_n_u8(0x26);
        const uint8x16_t v_hi  = vdupq_n_u8(0x80);
        uint8x16_t hi_acc = vdupq_n_u8(0);
        while (p + 16 <= pend) {
            uint8x16_t chunk = vld1q_u8(p);
            hi_acc = vorrq_u8(hi_acc, vandq_u8(chunk, v_hi));
            uint8x16_t hit = vorrq_u8(vceqq_u8(chunk, v_dq), vceqq_u8(chunk, v_amp));
            uint64x2_t hit64 = vreinterpretq_u64_u8(hit);
            uint64_t lo = vgetq_lane_u64(hit64, 0);
            uint64_t hi = vgetq_lane_u64(hit64, 1);
            if (lo) { ptrdiff_t pos = (ptrdiff_t)(p - buf) + (__builtin_ctzll(lo) >> 3);
                      if (vmaxvq_u8(hi_acc) != 0) all_ascii = 0;
                      return (pos << 1) | all_ascii; }
            if (hi) { ptrdiff_t pos = (ptrdiff_t)(p - buf) + 8 + (__builtin_ctzll(hi) >> 3);
                      if (vmaxvq_u8(hi_acc) != 0) all_ascii = 0;
                      return (pos << 1) | all_ascii; }
            p += 16;
        }
        if (vmaxvq_u8(hi_acc) != 0) all_ascii = 0;
    }
#elif USE_SSE2
    {
        const __m128i v_dq  = _mm_set1_epi8(0x22);
        const __m128i v_amp = _mm_set1_epi8(0x26);
        __m128i hi_acc = _mm_setzero_si128();
        while (p + 16 <= pend) {
            __m128i chunk = _mm_loadu_si128((const __m128i *)p);
            hi_acc = _mm_or_si128(hi_acc, chunk);
            __m128i hit = _mm_or_si128(_mm_cmpeq_epi8(chunk, v_dq), _mm_cmpeq_epi8(chunk, v_amp));
            int mask = _mm_movemask_epi8(hit);
            if (mask) { ptrdiff_t pos = (ptrdiff_t)(p - buf) + __builtin_ctz(mask);
                        if (_mm_movemask_epi8(hi_acc)) all_ascii = 0;
                        return (pos << 1) | all_ascii; }
            p += 16;
        }
        if (_mm_movemask_epi8(hi_acc)) all_ascii = 0;
    }
#endif

    while (p < pend) {
        uint8_t b = *p;
        if (b == 0x22 || b == 0x26) return ((ptrdiff_t)(p - buf) << 1) | all_ascii;
        if (b >= 0x80) all_ascii = 0;
        p++;
    }
    return (end << 1) | all_ascii;
}

/* Scan for closing single-quote or ampersand in an attribute value.
   Returns offset of first ' or & found, or 'end' if none. */
ptrdiff_t wireform_scan_squote(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end) {
    const uint8_t *p = buf + off;
    const uint8_t *pend = buf + end;

#if USE_NEON
    {
        const uint8x16_t v_sq  = vdupq_n_u8(0x27);
        const uint8x16_t v_amp = vdupq_n_u8(0x26);
        while (p + 16 <= pend) {
            uint8x16_t chunk = vld1q_u8(p);
            uint8x16_t hit = vorrq_u8(vceqq_u8(chunk, v_sq), vceqq_u8(chunk, v_amp));
            uint64x2_t hit64 = vreinterpretq_u64_u8(hit);
            uint64_t lo = vgetq_lane_u64(hit64, 0);
            uint64_t hi = vgetq_lane_u64(hit64, 1);
            if (lo) return (ptrdiff_t)(p - buf) + (__builtin_ctzll(lo) >> 3);
            if (hi) return (ptrdiff_t)(p - buf) + 8 + (__builtin_ctzll(hi) >> 3);
            p += 16;
        }
    }
#elif USE_SSE2
    {
        const __m128i v_sq  = _mm_set1_epi8(0x27);
        const __m128i v_amp = _mm_set1_epi8(0x26);
        while (p + 16 <= pend) {
            __m128i chunk = _mm_loadu_si128((const __m128i *)p);
            __m128i hit = _mm_or_si128(_mm_cmpeq_epi8(chunk, v_sq), _mm_cmpeq_epi8(chunk, v_amp));
            int mask = _mm_movemask_epi8(hit);
            if (mask) return (ptrdiff_t)(p - buf) + __builtin_ctz(mask);
            p += 16;
        }
    }
#endif

    while (p < pend) {
        if (*p == 0x27 || *p == 0x26) return (ptrdiff_t)(p - buf);
        p++;
    }
    return end;
}

/* Fused scan_squote + ASCII check. Returns (pos << 1) | ascii_flag. */
ptrdiff_t wireform_scan_squote_ascii(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end) {
    const uint8_t *p = buf + off;
    const uint8_t *pend = buf + end;
    int all_ascii = 1;

#if USE_NEON
    {
        const uint8x16_t v_sq  = vdupq_n_u8(0x27);
        const uint8x16_t v_amp = vdupq_n_u8(0x26);
        const uint8x16_t v_hi  = vdupq_n_u8(0x80);
        uint8x16_t hi_acc = vdupq_n_u8(0);
        while (p + 16 <= pend) {
            uint8x16_t chunk = vld1q_u8(p);
            hi_acc = vorrq_u8(hi_acc, vandq_u8(chunk, v_hi));
            uint8x16_t hit = vorrq_u8(vceqq_u8(chunk, v_sq), vceqq_u8(chunk, v_amp));
            uint64x2_t hit64 = vreinterpretq_u64_u8(hit);
            uint64_t lo = vgetq_lane_u64(hit64, 0);
            uint64_t hi = vgetq_lane_u64(hit64, 1);
            if (lo) { ptrdiff_t pos = (ptrdiff_t)(p - buf) + (__builtin_ctzll(lo) >> 3);
                      if (vmaxvq_u8(hi_acc) != 0) all_ascii = 0;
                      return (pos << 1) | all_ascii; }
            if (hi) { ptrdiff_t pos = (ptrdiff_t)(p - buf) + 8 + (__builtin_ctzll(hi) >> 3);
                      if (vmaxvq_u8(hi_acc) != 0) all_ascii = 0;
                      return (pos << 1) | all_ascii; }
            p += 16;
        }
        if (vmaxvq_u8(hi_acc) != 0) all_ascii = 0;
    }
#elif USE_SSE2
    {
        const __m128i v_sq  = _mm_set1_epi8(0x27);
        const __m128i v_amp = _mm_set1_epi8(0x26);
        __m128i hi_acc = _mm_setzero_si128();
        while (p + 16 <= pend) {
            __m128i chunk = _mm_loadu_si128((const __m128i *)p);
            hi_acc = _mm_or_si128(hi_acc, chunk);
            __m128i hit = _mm_or_si128(_mm_cmpeq_epi8(chunk, v_sq), _mm_cmpeq_epi8(chunk, v_amp));
            int mask = _mm_movemask_epi8(hit);
            if (mask) { ptrdiff_t pos = (ptrdiff_t)(p - buf) + __builtin_ctz(mask);
                        if (_mm_movemask_epi8(hi_acc)) all_ascii = 0;
                        return (pos << 1) | all_ascii; }
            p += 16;
        }
        if (_mm_movemask_epi8(hi_acc)) all_ascii = 0;
    }
#endif

    while (p < pend) {
        uint8_t b = *p;
        if (b == 0x27 || b == 0x26) return ((ptrdiff_t)(p - buf) << 1) | all_ascii;
        if (b >= 0x80) all_ascii = 0;
        p++;
    }
    return (end << 1) | all_ascii;
}

/* Scan for closing double-quote only (no ampersand stop).
   Returns offset of first " found, or 'end' if none. */
ptrdiff_t wireform_skip_to_dquote(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end) {
    const uint8_t *p = buf + off;
    const uint8_t *pend = buf + end;

#if USE_NEON
    {
        const uint8x16_t v_dq = vdupq_n_u8(0x22);
        while (p + 16 <= pend) {
            uint8x16_t chunk = vld1q_u8(p);
            uint8x16_t hit = vceqq_u8(chunk, v_dq);
            uint64x2_t hit64 = vreinterpretq_u64_u8(hit);
            uint64_t lo = vgetq_lane_u64(hit64, 0);
            uint64_t hi = vgetq_lane_u64(hit64, 1);
            if (lo) return (ptrdiff_t)(p - buf) + (__builtin_ctzll(lo) >> 3);
            if (hi) return (ptrdiff_t)(p - buf) + 8 + (__builtin_ctzll(hi) >> 3);
            p += 16;
        }
    }
#elif USE_SSE2
    {
        const __m128i v_dq = _mm_set1_epi8(0x22);
        while (p + 16 <= pend) {
            __m128i chunk = _mm_loadu_si128((const __m128i *)p);
            __m128i hit = _mm_cmpeq_epi8(chunk, v_dq);
            int mask = _mm_movemask_epi8(hit);
            if (mask) return (ptrdiff_t)(p - buf) + __builtin_ctz(mask);
            p += 16;
        }
    }
#endif

    while (p < pend) {
        if (*p == 0x22) return (ptrdiff_t)(p - buf);
        p++;
    }
    return end;
}

/* Scan for closing single-quote only (no ampersand stop).
   Returns offset of first ' found, or 'end' if none. */
ptrdiff_t wireform_skip_to_squote(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end) {
    const uint8_t *p = buf + off;
    const uint8_t *pend = buf + end;

#if USE_NEON
    {
        const uint8x16_t v_sq = vdupq_n_u8(0x27);
        while (p + 16 <= pend) {
            uint8x16_t chunk = vld1q_u8(p);
            uint8x16_t hit = vceqq_u8(chunk, v_sq);
            uint64x2_t hit64 = vreinterpretq_u64_u8(hit);
            uint64_t lo = vgetq_lane_u64(hit64, 0);
            uint64_t hi = vgetq_lane_u64(hit64, 1);
            if (lo) return (ptrdiff_t)(p - buf) + (__builtin_ctzll(lo) >> 3);
            if (hi) return (ptrdiff_t)(p - buf) + 8 + (__builtin_ctzll(hi) >> 3);
            p += 16;
        }
    }
#elif USE_SSE2
    {
        const __m128i v_sq = _mm_set1_epi8(0x27);
        while (p + 16 <= pend) {
            __m128i chunk = _mm_loadu_si128((const __m128i *)p);
            __m128i hit = _mm_cmpeq_epi8(chunk, v_sq);
            int mask = _mm_movemask_epi8(hit);
            if (mask) return (ptrdiff_t)(p - buf) + __builtin_ctz(mask);
            p += 16;
        }
    }
#endif

    while (p < pend) {
        if (*p == 0x27) return (ptrdiff_t)(p - buf);
        p++;
    }
    return end;
}

/* Skip past all attributes in a tag, handling quoted sections.
   Scans for > or />, skipping over "..." and '...' sections.
   Returns (offset_past_gt << 1) | self_close_bit.
   offset_past_gt is end+1 if no > found. */
ptrdiff_t wireform_skip_attrs(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end) {
    const uint8_t *p = buf + off;
    const uint8_t *pend = buf + end;

    while (p < pend) {
        uint8_t b = *p;
        switch (b) {
        case 0x3E: /* > */
            return ((ptrdiff_t)(p - buf) + 1) << 1;
        case 0x2F: /* / */
            if (p + 1 < pend && p[1] == 0x3E) {
                return (((ptrdiff_t)(p - buf) + 2) << 1) | 1;
            }
            p++;
            break;
        case 0x22: { /* " */
            ptrdiff_t qend = wireform_skip_to_dquote(buf, (ptrdiff_t)(p - buf) + 1, end);
            p = buf + (qend < end ? qend + 1 : qend);
            break;
        }
        case 0x27: { /* ' */
            ptrdiff_t qend = wireform_skip_to_squote(buf, (ptrdiff_t)(p - buf) + 1, end);
            p = buf + (qend < end ? qend + 1 : qend);
            break;
        }
        default:
            p++;
            break;
        }
    }
    return (end + 1) << 1;
}

/* Scan for end of unquoted attribute value: >, whitespace, or &.
   Returns offset of first delimiter found, or 'end' if none. */
ptrdiff_t wireform_scan_unquoted(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end) {
    const uint8_t *p = buf + off;
    const uint8_t *pend = buf + end;

    while (p < pend) {
        uint8_t b = *p;
        if (b == 0x3E || b == 0x26 || b == 0x20 || b == 0x09
            || b == 0x0A || b == 0x0D || b == 0x0C)
            return (ptrdiff_t)(p - buf);
        p++;
    }
    return end;
}

/* Scan for '>' character. Returns offset of '>' + 1, or 'end' if none found. */
ptrdiff_t wireform_scan_gt(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end) {
    const uint8_t *p = buf + off;
    const uint8_t *pend = buf + end;

#if USE_NEON
    {
        const uint8x16_t v_gt = vdupq_n_u8(0x3E);
        while (p + 16 <= pend) {
            uint8x16_t chunk = vld1q_u8(p);
            uint8x16_t hit = vceqq_u8(chunk, v_gt);
            uint64x2_t hit64 = vreinterpretq_u64_u8(hit);
            uint64_t lo = vgetq_lane_u64(hit64, 0);
            uint64_t hi = vgetq_lane_u64(hit64, 1);
            if (lo) return (ptrdiff_t)(p - buf) + (__builtin_ctzll(lo) >> 3) + 1;
            if (hi) return (ptrdiff_t)(p - buf) + 8 + (__builtin_ctzll(hi) >> 3) + 1;
            p += 16;
        }
    }
#elif USE_SSE2
    {
        const __m128i v_gt = _mm_set1_epi8(0x3E);
        while (p + 16 <= pend) {
            __m128i chunk = _mm_loadu_si128((const __m128i *)p);
            __m128i hit = _mm_cmpeq_epi8(chunk, v_gt);
            int mask = _mm_movemask_epi8(hit);
            if (mask) return (ptrdiff_t)(p - buf) + __builtin_ctz(mask) + 1;
            p += 16;
        }
    }
#endif

    while (p < pend) {
        if (*p == 0x3E) return (ptrdiff_t)(p - buf) + 1;
        p++;
    }
    return end;
}

/* Check if all bytes in [off, end) are ASCII (< 0x80). */
int wireform_is_all_ascii(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end) {
    const uint8_t *p = buf + off;
    const uint8_t *pend = buf + end;

#if USE_NEON
    {
        const uint8x16_t v_hi = vdupq_n_u8(0x80);
        while (p + 16 <= pend) {
            uint8x16_t chunk = vld1q_u8(p);
            if (vmaxvq_u8(vandq_u8(chunk, v_hi)) != 0) return 0;
            p += 16;
        }
    }
#elif USE_SSE2
    {
        while (p + 16 <= pend) {
            __m128i chunk = _mm_loadu_si128((const __m128i *)p);
            int mask = _mm_movemask_epi8(chunk);
            if (mask) return 0;
            p += 16;
        }
    }
#endif

    while (p < pend) {
        if (*p >= 0x80) return 0;
        p++;
    }
    return 1;
}

/* HTML text escape: scan for '<', '>', '&'.
   Returns offset of first match from 'offset', or 'len' if none found. */
int hs_html_find_text_escape(const uint8_t *buf, int offset, int len)
{
    const uint8_t *p    = buf + offset;
    const uint8_t *pend = buf + len;

#if USE_NEON
    {
        const uint8x16_t v_lt  = vdupq_n_u8('<');
        const uint8x16_t v_gt  = vdupq_n_u8('>');
        const uint8x16_t v_amp = vdupq_n_u8('&');
        while (p + 16 <= pend) {
            uint8x16_t chunk = vld1q_u8(p);
            uint8x16_t hit = vorrq_u8(
                vorrq_u8(vceqq_u8(chunk, v_lt), vceqq_u8(chunk, v_gt)),
                vceqq_u8(chunk, v_amp));
            uint64x2_t hit64 = vreinterpretq_u64_u8(hit);
            uint64_t lo = vgetq_lane_u64(hit64, 0);
            uint64_t hi = vgetq_lane_u64(hit64, 1);
            if (lo) return (int)(p - buf) + (__builtin_ctzll(lo) >> 3);
            if (hi) return (int)(p - buf) + 8 + (__builtin_ctzll(hi) >> 3);
            p += 16;
        }
    }
#elif USE_SSE2
    {
        const __m128i v_lt  = _mm_set1_epi8('<');
        const __m128i v_gt  = _mm_set1_epi8('>');
        const __m128i v_amp = _mm_set1_epi8('&');
        while (p + 16 <= pend) {
            __m128i chunk = _mm_loadu_si128((const __m128i *)p);
            int m1 = _mm_movemask_epi8(_mm_cmpeq_epi8(chunk, v_lt));
            int m2 = _mm_movemask_epi8(_mm_cmpeq_epi8(chunk, v_gt));
            int m3 = _mm_movemask_epi8(_mm_cmpeq_epi8(chunk, v_amp));
            int mask = m1 | m2 | m3;
            if (mask) return (int)(p - buf) + __builtin_ctz(mask);
            p += 16;
        }
    }
#endif

    while (p < pend) {
        uint8_t b = *p;
        if (b == '<' || b == '>' || b == '&') return (int)(p - buf);
        p++;
    }
    return len;
}

/* HTML attr escape: scan for '"', '&', '<', '>' in attribute values.
   Returns offset of first match from 'offset', or 'len' if none found. */
int hs_html_find_attr_escape(const uint8_t *buf, int offset, int len)
{
    const uint8_t *p    = buf + offset;
    const uint8_t *pend = buf + len;

#if USE_NEON
    {
        const uint8x16_t v_dq  = vdupq_n_u8('"');
        const uint8x16_t v_amp = vdupq_n_u8('&');
        const uint8x16_t v_lt  = vdupq_n_u8('<');
        const uint8x16_t v_gt  = vdupq_n_u8('>');
        while (p + 16 <= pend) {
            uint8x16_t chunk = vld1q_u8(p);
            uint8x16_t hit = vorrq_u8(
                vorrq_u8(vceqq_u8(chunk, v_dq), vceqq_u8(chunk, v_amp)),
                vorrq_u8(vceqq_u8(chunk, v_lt), vceqq_u8(chunk, v_gt)));
            uint64x2_t hit64 = vreinterpretq_u64_u8(hit);
            uint64_t lo = vgetq_lane_u64(hit64, 0);
            uint64_t hi = vgetq_lane_u64(hit64, 1);
            if (lo) return (int)(p - buf) + (__builtin_ctzll(lo) >> 3);
            if (hi) return (int)(p - buf) + 8 + (__builtin_ctzll(hi) >> 3);
            p += 16;
        }
    }
#elif USE_SSE2
    {
        const __m128i v_dq  = _mm_set1_epi8('"');
        const __m128i v_amp = _mm_set1_epi8('&');
        const __m128i v_lt  = _mm_set1_epi8('<');
        const __m128i v_gt  = _mm_set1_epi8('>');
        while (p + 16 <= pend) {
            __m128i chunk = _mm_loadu_si128((const __m128i *)p);
            int m1 = _mm_movemask_epi8(_mm_cmpeq_epi8(chunk, v_dq));
            int m2 = _mm_movemask_epi8(_mm_cmpeq_epi8(chunk, v_amp));
            int m3 = _mm_movemask_epi8(_mm_cmpeq_epi8(chunk, v_lt));
            int m4 = _mm_movemask_epi8(_mm_cmpeq_epi8(chunk, v_gt));
            int mask = m1 | m2 | m3 | m4;
            if (mask) return (int)(p - buf) + __builtin_ctz(mask);
            p += 16;
        }
    }
#endif

    while (p < pend) {
        uint8_t b = *p;
        if (b == '"' || b == '&' || b == '<' || b == '>') return (int)(p - buf);
        p++;
    }
    return len;
}
