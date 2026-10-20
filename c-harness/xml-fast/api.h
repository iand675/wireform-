/* Kernel API of wireform-xml/cbits/fast_xml.c, plus the scalar reference
 * implementations the tests and fuzzers compare against.
 *
 * Haskell imports (wireform-xml/src/XML/FastDOM.hs and XML/SAX.hs), all
 * `foreign import ccall unsafe`:
 *
 *   hs_xml_find_lt        :: Ptr Word8 -> CInt -> CInt -> IO CInt
 *   hs_xml_find_byte      :: Ptr Word8 -> CInt -> CInt -> CUChar -> IO CInt
 *   hs_xml_find_attr_end  :: Ptr Word8 -> CInt -> CInt -> CUChar -> IO CInt
 *   hs_xml_find_text_end  :: Ptr Word8 -> CInt -> CInt -> IO CInt
 *   hs_xml_find_cdata_end :: Ptr Word8 -> CInt -> CInt -> IO CInt
 *   hs_xml_find_comment_end :: Ptr Word8 -> CInt -> CInt -> IO CInt
 *
 * Caller contract for those (findLtP / findByteP / ... in both modules):
 * `buf` is the base of the whole document ByteString, `len` is its full
 * length, and `offset` is a parser position derived from earlier scanner
 * results (`off + 4`, `valStart = afterEq + 1`, `i + 1`, ...). So
 * 0 <= offset, 0 <= len, and offset may equal len (the parser reached
 * EOF) or exceed it by a small amount; the kernels then return their
 * miss value without reading memory. Only buf[offset .. len) is ever
 * read. len is the CInt-truncated ByteString length, so documents of up
 * to INT_MAX bytes reach the C side; the kernels' index arithmetic must
 * not overflow even with len == INT_MAX (see the regression tests).
 *
 * Miss values: find_lt, find_byte, find_attr_end, find_cdata_end and
 * find_comment_end return -1; find_text_end and html_find_text_end
 * return len. cdata_end / comment_end return the index of the first
 * byte of the three-byte terminator, which must lie wholly inside
 * [offset, len).
 *
 * Exported but not imported by any Haskell module today (wireform-html
 * ships its own scanners); tested against their C comments:
 *
 *   hs_html_find_text_end(buf, offset, len): like find_text_end, also
 *     stops at NUL and CR.
 *   hs_xml_find_escape(buf, offset, len): NOTE different convention,
 *     `len` is the number of bytes to scan starting at buf + offset (not
 *     an end index). Returns the absolute index of the first of
 *     < > & " ', or offset + len. Requires offset >= 0, len >= 0 and
 *     buf[offset .. offset + len) readable.
 *   hs_xml_scan_structural(buf, len, indices, max_indices): writes the
 *     indices of every < > / = " ' & in buf[0 .. len), in order, to
 *     indices[0 .. max_indices) and returns how many it wrote (stops
 *     once max_indices are written). indices must hold max_indices
 *     uint32_t; max_indices <= 0 writes nothing.
 */
#ifndef XML_FAST_API_H
#define XML_FAST_API_H

#include <stdint.h>

int hs_xml_scan_structural(const uint8_t *buf, int len, uint32_t *indices, int max_indices);
int hs_xml_find_lt(const uint8_t *buf, int offset, int len);
int hs_xml_find_byte(const uint8_t *buf, int offset, int len, uint8_t target_byte);
int hs_xml_find_attr_end(const uint8_t *buf, int offset, int len, uint8_t quote_char);
int hs_xml_find_text_end(const uint8_t *buf, int offset, int len);
int hs_xml_find_cdata_end(const uint8_t *buf, int offset, int len);
int hs_xml_find_comment_end(const uint8_t *buf, int offset, int len);
int hs_html_find_text_end(const uint8_t *buf, int offset, int len);
int hs_xml_find_escape(const uint8_t *buf, int offset, int len);

/* Exact-size uint32_t index array for hs_xml_scan_structural. wf_alloc
 * deliberately misaligns byte buffers; a uint32_t * must be 4-aligned
 * (the C type guarantees it; Haskell would pass a mallocBytes /
 * allocaArray pointer), so this keeps the alignment and still ends
 * exactly at the ASan redzone or a guard page. Needs harness.h. */
typedef struct {
    uint32_t *p;
    wf_guarded g;
} idx_buf;

static inline idx_buf idx_alloc(int count)
{
    idx_buf b;
    size_t n = (size_t)count * sizeof(uint32_t);
#if WF_SANITIZED_HEAP
    memset(&b.g, 0, sizeof b.g);
    b.p = malloc(n ? n : 1);
    if (!b.p) abort();
#else
    b.g = wf_guard_tail(n);
    b.p = (uint32_t *)(void *)b.g.p;
#endif
    return b;
}

static inline void idx_free(idx_buf b)
{
#if WF_SANITIZED_HEAP
    free(b.p);
#else
    wf_guard_free(b.g);
#endif
}

/* References. They index with long long so they stay correct for the
 * "virtual base" calls the tests make with offsets near INT_MAX. */

static inline int ref_xml_find_byte(const uint8_t *buf, int offset, int len, uint8_t t)
{
    for (long long i = offset; i < len; i++) {
        if (buf[i] == t) return (int)i;
    }
    return -1;
}

static inline int ref_xml_find_lt(const uint8_t *buf, int offset, int len)
{
    return ref_xml_find_byte(buf, offset, len, '<');
}

static inline int ref_is_text_end(uint8_t b) { return b == '<' || b == '&'; }
static inline int ref_is_html_text_end(uint8_t b)
{
    return b == '<' || b == '&' || b == '\0' || b == '\r';
}
static inline int ref_is_escape(uint8_t b)
{
    return b == '<' || b == '>' || b == '&' || b == '"' || b == '\'';
}
static inline int ref_is_structural(uint8_t b)
{
    return b == '<' || b == '>' || b == '/' || b == '=' || b == '"' || b == '\''
        || b == '&';
}

static inline int ref_xml_find_text_end(const uint8_t *buf, int offset, int len)
{
    for (long long i = offset; i < len; i++) {
        if (ref_is_text_end(buf[i])) return (int)i;
    }
    return len;
}

static inline int ref_html_find_text_end(const uint8_t *buf, int offset, int len)
{
    for (long long i = offset; i < len; i++) {
        if (ref_is_html_text_end(buf[i])) return (int)i;
    }
    return len;
}

/* First i in [offset, len - 3] with buf[i..i+3) == a a b. */
static inline int ref_find_triple(const uint8_t *buf, int offset, int len, uint8_t a, uint8_t b)
{
    for (long long i = offset; i + 3 <= len; i++) {
        if (buf[i] == a && buf[i + 1] == a && buf[i + 2] == b) return (int)i;
    }
    return -1;
}

static inline int ref_xml_find_cdata_end(const uint8_t *buf, int offset, int len)
{
    return ref_find_triple(buf, offset, len, ']', '>');
}

static inline int ref_xml_find_comment_end(const uint8_t *buf, int offset, int len)
{
    return ref_find_triple(buf, offset, len, '-', '>');
}

static inline int ref_xml_find_escape(const uint8_t *buf, int offset, int len)
{
    for (long long i = 0; i < len; i++) {
        if (ref_is_escape(buf[offset + i])) return (int)(offset + i);
    }
    return offset + len;
}

static inline int ref_xml_scan_structural(const uint8_t *buf, int len, uint32_t *idx, int max)
{
    int count = 0;
    for (int i = 0; i < len && count < max; i++) {
        if (ref_is_structural(buf[i])) idx[count++] = (uint32_t)i;
    }
    return count;
}

#endif
