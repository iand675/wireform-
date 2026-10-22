/* Kernel API of wireform-core/cbits/fast_decode.c, plus the slow scalar
 * reference implementations the tests and fuzzers compare against.
 *
 * Prototypes use the C definitions' parameter types. The Haskell imports
 * in wireform-core/src/Wireform/FFI.hs pass `Ptr ()` (void *), `Word8`,
 * `Word64`, `Int64` and, for everything sized, `CSize` (size_t) or
 * `CPtrdiff` (ptrdiff_t), which are ABI-identical to the C types.
 * Functions marked [FFI] are imported there; the others are exported but
 * have no Haskell caller today (they are tested to the same standard
 * because any future import inherits their contract, and they are still
 * int-typed: buffer-sized indices only, no FFI truncation to guard).
 *
 * Widths, chosen per value (the split wireform-html and wireform-http1
 * use):
 *   - positions into a buffer, write cursors, and scanner results
 *     (including the -1 miss) are ptrdiff_t;
 *   - byte counts, capacities and reported counts are size_t;
 *   - genuinely small values (booleans, bytes consumed, the 32-bit bounds
 *     bitmask) stay int.
 * Nothing a Haskell caller passes narrows to CInt, so a ByteString or
 * encode buffer over 2 GiB is neither truncated nor overflowed.
 *
 * General contract, from every Haskell call site: `len`/`count` are the
 * true length of a ByteString or Storable vector (the widths above), and
 * `buf` may be NULL when len == 0 (an empty ByteString is
 * `BS nullForeignPtr 0`, which unsafeUseAsCStringLen hands over as a NULL
 * pointer). Offsets are non-negative; counts are non-negative (a negative
 * Int becomes a huge size_t, so a caller must not pass one).
 */
#ifndef CORE_FAST_DECODE_API_H
#define CORE_FAST_DECODE_API_H

#include <stddef.h>
#include <stdint.h>
#include <string.h>

/* ------------------------------------------------------------------ */
/* Varint decoding                                                     */
/* ------------------------------------------------------------------ */

/* Harness-only, int-typed: the six entry points that still take `int
 * len`/`int offset` are hs_proto_decode_varint_fast, _decode_tag_fast,
 * _skip_varint, _decode_packed_varints, _decode_packed_single_byte_varints
 * and _skip_field. None of them is imported by Haskell (FFI.hs imports
 * only the [FFI] kernels), so nothing is narrowed across the FFI here and
 * there is no CInt truncation to fix; the harness bounds them to the int
 * range. If one ever gains a Haskell caller, widen it in the same way the
 * [FFI] kernels in this file already are (size_t byte counts, ptrdiff_t
 * positions) before importing it.
 */

/* Decode one varint from buf[offset..len). Returns bytes consumed (1..10)
 * and stores the value, or 0 (and leaves *out alone) if the varint is
 * unterminated within len or within 10 bytes. Like most protobuf
 * decoders it is lenient about the 10th byte: bits above 2^64 are
 * dropped. Requires 0 <= offset; offset >= len returns 0. */
int hs_proto_decode_varint_fast(const uint8_t *buf, int len, int offset, uint64_t *out);

/* decode_varint_fast, then field = (int)(tag >> 3), wire = tag & 7. */
int hs_proto_decode_tag_fast(const uint8_t *buf, int len, int offset,
                             int *field_number, int *wire_type);

/* [FFI] decodeVarintSWAR. Branch-free decode from one 8-byte load.
 * REQUIRES 8 readable bytes at buf+offset (Proto.Dynamic only calls it
 * when off + 8 <= length). offset is a position (ptrdiff_t). Returns 1..8
 * (a small count, int) and stores the value, or 0 when none of the 8
 * bytes terminates the varint (9- and 10-byte varints and malformed
 * input: the caller falls back to its slow path). */
int hs_proto_decode_varint_swar(const uint8_t *buf, ptrdiff_t offset, uint64_t *out);

/* [FFI] relocatePageBoundary. If buf+len lies within 8 bytes of a 4 KiB
 * boundary (or on it), copies buf into out_buf followed by 7 zero bytes
 * and returns 1 (-1 if out_len < len + 7); otherwise returns 0 and writes
 * nothing. len == 0 returns 0. Both are byte counts, not positions: len
 * bytes are copied and out_len is the destination capacity (size_t, so a
 * >2 GiB ByteString or a negative capacity cannot be expressed). The
 * wrapper always passes out_len = len+7. */
int hs_proto_relocate_page_boundary(const uint8_t *buf, size_t len, uint8_t *out_buf, size_t out_len);

/* Length of the varint at buf[offset..len) (1..10), 0 if unterminated
 * within len or 10 bytes. Requires 0 <= offset. */
int hs_proto_skip_varint(const uint8_t *buf, int len, int offset);

/* Decode a packed varint run of exactly len bytes into out[0..max_out).
 * Returns the count, or -1 if a varint is malformed or the values do not
 * fit in max_out. out[] may be partially written on -1. out is an
 * ordinary (8-byte aligned) uint64_t array; buf has no alignment. */
int hs_proto_decode_packed_varints(const uint8_t *buf, int len, uint64_t *out, int max_out);

/* [FFI] countPackedVarints: number of bytes < 0x80 in buf[0..len). Both
 * the byte count and the varint count are size_t: a packed run over 2 GiB
 * is ordinary, and its count can exceed INT_MAX. */
size_t hs_proto_count_packed_varints(const uint8_t *buf, size_t len);

/* [FFI] packedAllSingleByte: 1 iff every byte of buf[0..len) is < 0x80.
 * len is a byte count (size_t); the answer is a boolean (int). */
int hs_proto_packed_all_single_byte(const uint8_t *buf, size_t len);

/* out[i] = buf[i] for i < len; returns len. out holds len uint64s and is
 * 8-byte aligned (it is a uint64_t array).
 * Meant for buffers that passed packed_all_single_byte, but defined for
 * any bytes. */
int hs_proto_decode_packed_single_byte_varints(const uint8_t *buf, int len, uint64_t *out);

/* ------------------------------------------------------------------ */
/* UTF-8                                                               */
/* ------------------------------------------------------------------ */

/* [FFI] validateUtf8SWAR, used by Proto.Internal.Wire.Decode on every
 * string field from the wire. 1 iff buf[0..len) is well-formed UTF-8
 * (RFC 3629: no overlongs, no surrogates, nothing above U+10FFFF). len is
 * a byte count (size_t): a >2 GiB field is validated over its whole
 * length, not a truncated prefix. */
int hs_proto_validate_utf8_fast(const uint8_t *buf, size_t len);

/* ------------------------------------------------------------------ */
/* Encoding                                                            */
/* ------------------------------------------------------------------ */

/* Writes varint(value) at buf+offset (up to 10 bytes); returns its size.
 * The caller guarantees the space. offset is the write cursor into a
 * direct-encode buffer (Encode.Direct allocates one ByteString and fills
 * it front to back), so it and the returned byte count are size_t: both
 * can pass INT_MAX. */
size_t hs_proto_encode_varint(uint8_t *buf, size_t offset, uint64_t value);

/* [FFI] encodeLengthDelimitedC: tag, varint(len), data[0..len) at
 * buf+offset; returns 1 + varint size + len. The caller (Encode.Direct)
 * has sized buf; data may be NULL when len == 0 (empty ByteString/Text);
 * data and the destination do not overlap. offset (write cursor) and len
 * (byte count) are size_t, and so is the result, since a single field
 * over 2 GiB encodes to more than INT_MAX bytes. */
size_t hs_proto_encode_length_delimited(uint8_t *buf, size_t offset, uint8_t tag,
                                        const uint8_t *data, size_t len);

/* [FFI] encodeVarintFieldC: tag then varint(value); returns 1 + size.
 * offset is the write cursor (size_t). */
size_t hs_proto_encode_varint_field(uint8_t *buf, size_t offset, uint8_t tag, uint64_t value);

/* [FFI] encodeBoolFieldC: tag then 0/1 (any non-zero value is 1); 2.
 * offset is the write cursor (size_t). */
size_t hs_proto_encode_bool_field(uint8_t *buf, size_t offset, uint8_t tag, int value);

/* ------------------------------------------------------------------ */
/* Fixed-width loads and stores (4/8/2 bytes at buf+offset, unaligned)  */
/* ------------------------------------------------------------------ */

uint32_t hs_proto_decode_fixed32(const uint8_t *buf, int offset);
uint64_t hs_proto_decode_fixed64(const uint8_t *buf, int offset);
uint16_t hs_proto_read_be16(const uint8_t *buf, int offset);
uint32_t hs_proto_read_be32(const uint8_t *buf, int offset);
uint64_t hs_proto_read_be64(const uint8_t *buf, int offset);
void hs_proto_write_be16(uint8_t *buf, int offset, uint16_t val);
void hs_proto_write_be32(uint8_t *buf, int offset, uint32_t val);
void hs_proto_write_be64(uint8_t *buf, int offset, uint64_t val);
uint16_t hs_proto_read_le16(const uint8_t *buf, int offset);
uint32_t hs_proto_read_le32(const uint8_t *buf, int offset);
uint64_t hs_proto_read_le64(const uint8_t *buf, int offset);
void hs_proto_write_le16(uint8_t *buf, int offset, uint16_t val);
void hs_proto_write_le32(uint8_t *buf, int offset, uint32_t val);
void hs_proto_write_le64(uint8_t *buf, int offset, uint64_t val);

/* Offset just past the field value of wire type wire_type starting at
 * buf[offset..len), or -1 if it is truncated, malformed, declares a
 * length past len, or the wire type is not 0/1/2/5. Requires
 * 0 <= offset <= len. */
int hs_proto_skip_field(const uint8_t *buf, int len, int offset, int wire_type);

/* ------------------------------------------------------------------ */
/* Scanners                                                            */
/* ------------------------------------------------------------------ */

/* [FFI] findNul / findNulBS (BSON cstrings, offset from the parser):
 * first i in [offset, len) with buf[i] == 0, else -1. offset >= len
 * returns -1 without reading. Requires 0 <= offset. offset is the start
 * position and len the buffer's length (the exclusive end), and the
 * result is a position, so all three are ptrdiff_t: -1 survives the
 * widening and neither end truncates past 2 GiB. */
ptrdiff_t hs_proto_find_nul(const uint8_t *buf, ptrdiff_t offset, ptrdiff_t len);

/* [FFI] isAscii / isAsciiBS: 1 iff buf[offset..offset+len) are all < 0x80.
 * Note len counts from offset (a position delta), so it is ptrdiff_t
 * alongside the offset: offset + len is the end position and can pass
 * INT_MAX. len <= 0 returns 1. */
int hs_proto_is_ascii(const uint8_t *buf, ptrdiff_t offset, ptrdiff_t len);

/* [FFI] findJsonEscape / findJsonEscapeBS (len = bytes after offset):
 * offset + first i < len with buf[offset+i] in {'"', '\\', 0x00..0x1F},
 * else offset + len. offset is a position and len a position delta, both
 * ptrdiff_t; the result (a position, possibly offset + len past INT_MAX)
 * too. len <= 0 returns offset + len without reading. */
ptrdiff_t hs_proto_find_json_escape(const uint8_t *buf, ptrdiff_t offset, ptrdiff_t len);

/* [FFI] skipWhitespace / skipWhitespaceBS (EDN): first i >= offset that is
 * not space, tab, LF, CR or ',' and not inside a ';' comment (which runs
 * to, and not including, the next LF). Returns len if everything up to
 * len is skipped, and offset unchanged if offset >= len. offset is the
 * start position and len the buffer's length (the exclusive end), so all
 * three are ptrdiff_t. Requires 0 <= offset. */
ptrdiff_t hs_proto_skip_ws(const uint8_t *buf, ptrdiff_t offset, ptrdiff_t len);

/* [FFI] compareBounds / compareBoundsBS (Iceberg filterBoundsMask).
 * bounds holds count*width bytes, search holds width bytes (documented
 * precondition of filterBoundsMask). Bit i (i < 32) of the result is set
 * iff bound i <= search: for width 4 as little-endian signed int32, for
 * any other width as memcmp. count and width are counts, not positions,
 * so size_t; width == 0 means empty bounds (they compare equal, nothing
 * is read). Bounds past the 32nd are ignored; bit 31 makes the result
 * negative as a C int, which is why the mask stays int. */
int hs_proto_compare_bounds(const uint8_t *bounds, size_t count, size_t width,
                            const uint8_t *search);

/* [FFI] validateArrowBuffers: count (offset, length) int64 pairs. 1 iff
 * every offset and length is >= 0, offset + length <= body_length, and
 * each non-empty buffer starts at or after the end of every earlier one
 * (Arrow writers emit buffers in order; out-of-order is rejected).
 * count is a count of pairs (size_t); a negative Int would be a caller
 * bug, since it becomes a huge count.
 * buf_pairs is 8-byte aligned: Arrow.Read.Columns passes VS.unsafeWith
 * of a Storable Buffer vector, whose instance declares alignment 8. */
int hs_proto_validate_arrow_buffers(const int64_t *buf_pairs, size_t count,
                                    int64_t body_length);

/* ------------------------------------------------------------------ */
/* Signature pinning                                                   */
/* ------------------------------------------------------------------ */
/* The kernels that touch every byte they are given (packed counts, UTF-8,
 * the encoders, bounds, Arrow) cannot be exercised at >2 GiB in a test
 * without allocating gigabytes, so pin their widths at compile time. The
 * scanners get a sparse runtime case instead (scanners_beyond_2gib in
 * test_scan.c). */
#define WF_SIG(fn, type) _Generic(&(fn), type: 1, default: 0)

_Static_assert(
    WF_SIG(hs_proto_count_packed_varints,
           size_t (*)(const uint8_t *, size_t)),
    "hs_proto_count_packed_varints must take and return size_t");
_Static_assert(
    WF_SIG(hs_proto_validate_utf8_fast, int (*)(const uint8_t *, size_t)),
    "hs_proto_validate_utf8_fast must take a size_t byte count");
_Static_assert(
    WF_SIG(hs_proto_decode_varint_swar,
           int (*)(const uint8_t *, ptrdiff_t, uint64_t *)),
    "hs_proto_decode_varint_swar must take a ptrdiff_t offset");
_Static_assert(
    WF_SIG(hs_proto_relocate_page_boundary,
           int (*)(const uint8_t *, size_t, uint8_t *, size_t)),
    "hs_proto_relocate_page_boundary must take size_t lengths");
_Static_assert(
    WF_SIG(hs_proto_encode_varint, size_t (*)(uint8_t *, size_t, uint64_t)),
    "hs_proto_encode_varint must take a size_t write cursor");
_Static_assert(
    WF_SIG(hs_proto_encode_length_delimited,
           size_t (*)(uint8_t *, size_t, uint8_t, const uint8_t *, size_t)),
    "hs_proto_encode_length_delimited must take size_t offset and length");
_Static_assert(
    WF_SIG(hs_proto_compare_bounds,
           int (*)(const uint8_t *, size_t, size_t, const uint8_t *)),
    "hs_proto_compare_bounds must take size_t count and width");
_Static_assert(
    WF_SIG(hs_proto_validate_arrow_buffers,
           int (*)(const int64_t *, size_t, int64_t)),
    "hs_proto_validate_arrow_buffers must take a size_t pair count");

/* ------------------------------------------------------------------ */
/* Reference implementations                                           */
/* ------------------------------------------------------------------ */

static inline int ref_decode_varint(const uint8_t *buf, int len, int off, uint64_t *out)
{
    uint64_t v = 0;
    for (int k = 0; k < 10; k++) {
        if ((long long)off + k >= len) return 0;
        uint8_t b = buf[off + k];
        v |= (uint64_t)(b & 0x7F) << (7 * k); /* 7*9 = 63: in range */
        if (b < 0x80) {
            *out = v;
            return k + 1;
        }
    }
    return 0;
}

/* SWAR semantics: like ref_decode_varint over an 8-byte window. */
static inline int ref_decode_varint8(const uint8_t *buf, int off, uint64_t *out)
{
    return ref_decode_varint(buf, off + 8, off, out);
}

static inline int ref_skip_varint(const uint8_t *buf, int len, int off)
{
    uint64_t v;
    return ref_decode_varint(buf, len, off, &v);
}

static inline int ref_decode_packed(const uint8_t *buf, int len, uint64_t *out, int max_out)
{
    int pos = 0, count = 0;
    while (pos < len) {
        if (count >= max_out) return -1;
        uint64_t v;
        int n = ref_decode_varint(buf, len, pos, &v);
        if (n == 0) return -1;
        out[count++] = v;
        pos += n;
    }
    return count;
}

static inline size_t ref_count_packed(const uint8_t *buf, size_t len)
{
    size_t c = 0;
    for (size_t i = 0; i < len; i++) c += buf[i] < 0x80;
    return c;
}

static inline int ref_all_single_byte(const uint8_t *buf, size_t len)
{
    for (size_t i = 0; i < len; i++) {
        if (buf[i] >= 0x80) return 0;
    }
    return 1;
}

/* Unicode 15, table 3-7 (well-formed UTF-8 byte sequences), byte ranges
 * written out literally: an independent formulation from the kernel's
 * decode-then-range-check. */
static inline int ref_utf8_valid(const uint8_t *s, int len)
{
    int i = 0;
    while (i < len) {
        uint8_t a = s[i];
        int n;
        uint8_t lo = 0x80, hi = 0xBF; /* range of the second byte */
        if (a <= 0x7F) { i++; continue; }
        else if (a >= 0xC2 && a <= 0xDF) n = 2;
        else if (a == 0xE0) { n = 3; lo = 0xA0; }
        else if (a >= 0xE1 && a <= 0xEC) n = 3;
        else if (a == 0xED) { n = 3; hi = 0x9F; }
        else if (a >= 0xEE && a <= 0xEF) n = 3;
        else if (a == 0xF0) { n = 4; lo = 0x90; }
        else if (a >= 0xF1 && a <= 0xF3) n = 4;
        else if (a == 0xF4) { n = 4; hi = 0x8F; }
        else return 0;
        if (len - i < n) return 0;
        if (s[i + 1] < lo || s[i + 1] > hi) return 0;
        for (int k = 2; k < n; k++) {
            if (s[i + k] < 0x80 || s[i + k] > 0xBF) return 0;
        }
        i += n;
    }
    return 1;
}

/* Encode code point cp (any value < 2^21) with the generic UTF-8 bit
 * layout, without rejecting surrogates: used to build test inputs. */
static inline int ref_utf8_encode(uint32_t cp, uint8_t *o)
{
    if (cp < 0x80) { o[0] = (uint8_t)cp; return 1; }
    if (cp < 0x800) {
        o[0] = (uint8_t)(0xC0 | (cp >> 6));
        o[1] = (uint8_t)(0x80 | (cp & 0x3F));
        return 2;
    }
    if (cp < 0x10000) {
        o[0] = (uint8_t)(0xE0 | (cp >> 12));
        o[1] = (uint8_t)(0x80 | ((cp >> 6) & 0x3F));
        o[2] = (uint8_t)(0x80 | (cp & 0x3F));
        return 3;
    }
    o[0] = (uint8_t)(0xF0 | (cp >> 18));
    o[1] = (uint8_t)(0x80 | ((cp >> 12) & 0x3F));
    o[2] = (uint8_t)(0x80 | ((cp >> 6) & 0x3F));
    o[3] = (uint8_t)(0x80 | (cp & 0x3F));
    return 4;
}

static inline int ref_encode_varint(uint8_t *o, uint64_t v)
{
    int n = 0;
    do {
        uint8_t b = (uint8_t)(v & 0x7F);
        v >>= 7;
        o[n++] = (uint8_t)(b | (v ? 0x80 : 0));
    } while (v);
    return n;
}

static inline int ref_varint_size(uint64_t v)
{
    int n = 1;
    while (v >= 0x80) {
        v >>= 7;
        n++;
    }
    return n;
}

static inline int ref_skip_field(const uint8_t *buf, int len, int off, int wt)
{
    long long room = (long long)len - off;
    switch (wt) {
    case 0: {
        int n = ref_skip_varint(buf, len, off);
        return n ? off + n : -1;
    }
    case 1: return room >= 8 ? off + 8 : -1;
    case 5: return room >= 4 ? off + 4 : -1;
    case 2: {
        uint64_t v;
        int n = ref_decode_varint(buf, len, off, &v);
        if (n == 0) return -1;
        if (v > (uint64_t)(room - n)) return -1;
        return off + n + (int)v;
    }
    default: return -1;
    }
}

static inline ptrdiff_t ref_find_nul(const uint8_t *buf, ptrdiff_t off, ptrdiff_t len)
{
    for (ptrdiff_t i = off; i < len; i++) {
        if (buf[i] == 0) return i;
    }
    return -1;
}

static inline int ref_is_ascii(const uint8_t *buf, ptrdiff_t off, ptrdiff_t len)
{
    for (ptrdiff_t i = 0; i < len; i++) {
        if (buf[off + i] >= 0x80) return 0;
    }
    return 1;
}

static inline ptrdiff_t ref_find_json_escape(const uint8_t *buf, ptrdiff_t off,
                                             ptrdiff_t len)
{
    for (ptrdiff_t i = 0; i < len; i++) {
        uint8_t b = buf[off + i];
        if (b == '"' || b == '\\' || b < 0x20) return off + i;
    }
    return off + len;
}

static inline ptrdiff_t ref_skip_ws(const uint8_t *buf, ptrdiff_t off, ptrdiff_t len)
{
    ptrdiff_t i = off;
    int in_comment = 0;
    for (; i < len; i++) {
        uint8_t b = buf[i];
        if (in_comment) {
            if (b == '\n') in_comment = 0;
            continue;
        }
        if (b == ';') { in_comment = 1; continue; }
        if (b == ' ' || b == '\t' || b == '\n' || b == '\r' || b == ',') continue;
        return i;
    }
    return off > len ? off : len;
}

static inline int ref_compare_bounds(const uint8_t *bounds, size_t count, size_t width,
                                     const uint8_t *search)
{
    uint32_t r = 0;
    for (size_t i = 0; i < count && i < 32; i++) {
        int le;
        if (width == 0) {
            le = 1; /* empty bounds compare equal; no NULL + 0 */
        } else if (width == 4) {
            const uint8_t *b = bounds + 4 * i;
            int32_t x = (int32_t)((uint32_t)b[0] | (uint32_t)b[1] << 8
                                  | (uint32_t)b[2] << 16 | (uint32_t)b[3] << 24);
            int32_t s = (int32_t)((uint32_t)search[0] | (uint32_t)search[1] << 8
                                  | (uint32_t)search[2] << 16 | (uint32_t)search[3] << 24);
            le = x <= s;
        } else {
            const uint8_t *b = bounds + i * width;
            le = 1;
            for (size_t k = 0; k < width; k++) {
                if (b[k] != search[k]) {
                    le = b[k] < search[k];
                    break;
                }
            }
        }
        if (le) r |= 1u << i;
    }
    return (int)r;
}

static inline int ref_validate_arrow(const int64_t *p, size_t count, int64_t body)
{
    if (body < 0) return 0;
    int64_t prev_end = 0;
    for (size_t i = 0; i < count; i++) {
        int64_t off = p[2 * i], len = p[2 * i + 1];
        if (off < 0 || len < 0) return 0;
        /* exact in 128 bits: no overflow possible */
        if ((__int128)off + len > body) return 0;
        if (len > 0 && off < prev_end) return 0;
        if (off + len > prev_end) prev_end = off + len;
    }
    return 1;
}

static inline uint64_t ref_load_le(const uint8_t *p, int n)
{
    uint64_t v = 0;
    for (int k = n - 1; k >= 0; k--) v = (v << 8) | p[k];
    return v;
}

static inline uint64_t ref_load_be(const uint8_t *p, int n)
{
    uint64_t v = 0;
    for (int k = 0; k < n; k++) v = (v << 8) | p[k];
    return v;
}

#endif
