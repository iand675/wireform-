#ifndef WIREFORM_LZO_API_H
#define WIREFORM_LZO_API_H
#include <stddef.h>
#include <stdint.h>
#include <string.h>
/* Raw LZO1X, disjoint buffers, arbitrary alignment. All byte counts are
 * size_t. Bound returns zero for overflow or inputs beyond UINT32_MAX.
 * Compression requires the worst-case capacity even if fewer bytes fit.
 * Measurement reads input only and rejects output above its explicit limit.
 * A NULL destination is valid only for zero-capacity decompression.
 * Source may be NULL only when its length is zero. written may be NULL:
 * this is an error, with no source/destination access. Errors may leave a
 * prefix in output, but never outside cap. Status0 means complete success;
 * 1=input short,2=output budget,3=history,4=end/trailing,5=length overflow.
 */
size_t hs_columnar_lzo1x_compress_bound(size_t);
int hs_columnar_lzo1x_compress(const uint8_t*,size_t,uint8_t*,size_t,size_t*);
int hs_columnar_lzo1x_measure(const uint8_t*,size_t,size_t,size_t*);
int hs_columnar_lzo1x_decompress(const uint8_t*,size_t,uint8_t*,size_t,size_t*);
/* A deliberately literal-only, independent format encoder. */
static size_t ref_literals(const uint8_t *src,size_t n,uint8_t *dst) {
 size_t at=0;
 if(n && n<=238)dst[at++]=(uint8_t)(n+17);
 else if(n){dst[at++]=0;size_t extra=n-18;while(extra>255){dst[at++]=0;extra-=255;}dst[at++]=(uint8_t)extra;}
 if(n)memcpy(dst+at,src,n);at+=n;dst[at++]=17;dst[at++]=0;dst[at++]=0;return at;
}
static void payload(uint8_t *dst,size_t n,unsigned kind,uint32_t seed) {
 for(size_t i=0;i<n;i++){seed=seed*UINT32_C(1664525)+UINT32_C(1013904223);dst[i]=kind==0?0:kind==1?(uint8_t)i:kind==2?(uint8_t)(i%7):(uint8_t)(seed>>24);}
}
struct ref_vector {size_t size;unsigned kind;uint32_t seed;size_t packed_size;const uint8_t *packed;};
#endif
