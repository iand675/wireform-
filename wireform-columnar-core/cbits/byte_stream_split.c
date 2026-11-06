/* Portable SIMD byte-lane transposes for BYTE_STREAM_SPLIT and other
 * fixed-width columnar buffers. Bytes retain their order within a value;
 * the caller handles host endianness. All pointers may be unaligned.
 *
 * Source and destination are disjoint, each covering exactly width*n
 * bytes, with n <= SIZE_MAX/width. At n == 0 NULL pointers are allowed.
 * The array entry points additionally require off+width*n addressable
 * source bytes; Haskell restricts them to bounded, unsafe FFI calls.
 */
#include <stddef.h>
#include <stdint.h>
#include <simde/arm/neon.h>

/* Caller validates n <= SIZE_MAX / width and non-overlapping buffers of
   exactly width*n bytes. All loads stay within a full 16-value block. */
void hs_columnar_split_bytes4(const uint8_t *src, uint8_t *dst, size_t n) {
    size_t i = 0;
    for (; n - i >= 16; i += 16) {
        simde_uint8x16x4_t lanes = simde_vld4q_u8(src + 4*i);
        for (size_t j = 0; j < 4; j++) simde_vst1q_u8(dst + j*n + i, lanes.val[j]);
    }
    for (; i < n; i++) for (size_t j = 0; j < 4; j++) dst[j*n+i] = src[4*i+j];
}
void hs_columnar_join_bytes4(const uint8_t *src, uint8_t *dst, size_t n) {
    size_t i = 0;
    for (; n - i >= 16; i += 16) {
        simde_uint8x16x4_t lanes;
        for (size_t j = 0; j < 4; j++) lanes.val[j] = simde_vld1q_u8(src + j*n + i);
        simde_vst4q_u8(dst + 4*i, lanes);
    }
    for (; i < n; i++) for (size_t j = 0; j < 4; j++) dst[4*i+j] = src[j*n+i];
}
void hs_columnar_split_bytes8(const uint8_t *src, uint8_t *dst, size_t n) {
    size_t i = 0;
    for (; n - i >= 16; i += 16) {
        simde_uint8x16x4_t lo = simde_vld4q_u8(src + 8*i);
        simde_uint8x16x4_t hi = simde_vld4q_u8(src + 8*i + 64);
        for (size_t j = 0; j < 4; j++) {
            simde_vst1q_u8(dst + j*n + i, simde_vuzp1q_u8(lo.val[j], hi.val[j]));
            simde_vst1q_u8(dst + (j+4)*n + i, simde_vuzp2q_u8(lo.val[j], hi.val[j]));
        }
    }
    for (; i < n; i++) for (size_t j = 0; j < 8; j++) dst[j*n+i] = src[8*i+j];
}
void hs_columnar_join_bytes8(const uint8_t *src, uint8_t *dst, size_t n) {
    size_t i = 0;
    for (; n - i >= 16; i += 16) {
        simde_uint8x16x4_t lo, hi;
        for (size_t j = 0; j < 4; j++) {
            simde_uint8x16_t a = simde_vld1q_u8(src + j*n + i);
            simde_uint8x16_t b = simde_vld1q_u8(src + (j+4)*n + i);
            lo.val[j] = simde_vzip1q_u8(a,b);
            hi.val[j] = simde_vzip2q_u8(a,b);
        }
        simde_vst4q_u8(dst + 8*i, lo);
        simde_vst4q_u8(dst + 8*i + 64, hi);
    }
    for (; i < n; i++) for (size_t j = 0; j < 8; j++) dst[8*i+j] = src[j*n+i];
}

/* Only used via bounded unsafe FFI calls; the GC cannot move the array
   during the call. Larger Haskell arrays use a pinned input copy. */
void hs_columnar_split_bytes4_array(const uint8_t *base,size_t off,uint8_t *dst,size_t n){if(n)hs_columnar_split_bytes4(base+off,dst,n);}
void hs_columnar_split_bytes8_array(const uint8_t *base,size_t off,uint8_t *dst,size_t n){if(n)hs_columnar_split_bytes8(base+off,dst,n);}
