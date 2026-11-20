/* Format-independent packed-integer/FOR to native IEEE kernels.
 * The caller validates extents, bit width and scatter positions. Both sources
 * and destinations permit arbitrary alignment and must be disjoint. All byte
 * counts, offsets and row positions use size_t. Zero count permits NULL.
 *
 * Unsigned frame addition wraps at the integer width, then the signed value
 * is converted to float/double and multiplied by factor and scale separately.
 * Do not enable fast-math/reassociate these multiplications. Packed input and
 * scatter values are little-endian; output is native-endian. Every read stays
 * within the caller's packed section, including SIMD tails.
 */
#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <simde/arm/neon.h>

static inline uint64_t little64(const uint8_t *src, size_t available) {
  uint64_t value = 0;
  if (available >= 8) memcpy(&value, src, 8);
  else memcpy(&value, src, available);
#if defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  value = __builtin_bswap64(value);
#endif
  return value;
}
static inline uint64_t unpack(const uint8_t *src, size_t len, size_t bit, unsigned width) {
  const size_t byte = bit / 8;
  const unsigned shift = bit % 8;
  uint64_t value = little64(src + byte, len - byte) >> shift;
  if (shift != 0 && width > 64 - shift)
    value |= (uint64_t)src[byte + 8] << (64 - shift);
  return value & (width == 64 ? UINT64_MAX : ((UINT64_C(1) << width) - 1));
}
static inline float value32(uint32_t raw, float factor, float scale) {
  int32_t value; memcpy(&value, &raw, 4);
  float decoded = (float)value;
  decoded = decoded * factor;
  return decoded * scale;
}
static inline double value64(uint64_t raw, double factor, double scale) {
  int64_t value; memcpy(&value, &raw, 8);
  double decoded = (double)value;
  decoded = decoded * factor;
  return decoded * scale;
}
#define WIDTHS32(M) M(1) M(2) M(3) M(4) M(5) M(6) M(7) M(8) M(9) M(10) M(11) M(12) M(13) M(14) M(15) M(16) M(17) M(18) M(19) M(20) M(21) M(22) M(23) M(24) M(25) M(26) M(27) M(28) M(29) M(30) M(31) M(32)
#define WIDTHS64(M) WIDTHS32(M) M(33) M(34) M(35) M(36) M(37) M(38) M(39) M(40) M(41) M(42) M(43) M(44) M(45) M(46) M(47) M(48) M(49) M(50) M(51) M(52) M(53) M(54) M(55) M(56) M(57) M(58) M(59) M(60) M(61) M(62) M(63) M(64)
static inline uint64_t block_delta(const uint64_t words[8],unsigned bit,unsigned width) {
  unsigned word=bit/64,shift=bit%64;
  uint64_t value=words[word]>>shift;
  if(shift && width>64-shift) value|=words[word+1]<<(64-shift);
  return value & (width==64?UINT64_MAX:((UINT64_C(1)<<width)-1));
}
#define WORD_AT(W,J) ((W)>(J)*8?little64(src+byte+(J)*8,(W)-(J)*8>=8?8:(W)-(J)*8):0)
#define LOAD_BLOCK(W) const size_t byte=first*(W)/8; const uint64_t words[8]={WORD_AT(W,0),WORD_AT(W,1),WORD_AT(W,2),WORD_AT(W,3),WORD_AT(W,4),WORD_AT(W,5),WORD_AT(W,6),WORD_AT(W,7)}
#define DELTAS(W) block_delta(words,0*(W),(W)),block_delta(words,1*(W),(W)),block_delta(words,2*(W),(W)),block_delta(words,3*(W),(W)),block_delta(words,4*(W),(W)),block_delta(words,5*(W),(W)),block_delta(words,6*(W),(W)),block_delta(words,7*(W),(W))
/* Four overlapping little-endian windows are gathered in one byte shuffle.
 * Lanes outside the packed section are zeroed before the shuffle, then masked.
 */
#if defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#define WORD_LANES32(x) simde_vreinterpretq_u32_u8(simde_vrev32q_u8(x))
#else
#define WORD_LANES32(x) simde_vreinterpretq_u32_u8(x)
#endif
#define WINDOW(W,I) ((I)*(W)/8+0),((I)*(W)/8+1),((I)*(W)/8+2),((I)*(W)/8+3)
static inline __attribute__((always_inline)) void decode32_small8(const uint8_t *src,size_t available,uint8_t *dst,unsigned w,uint32_t frame,float factor,float scale){
 simde_uint8x16_t input;
 if(available>=16)input=simde_vld1q_u8(src);
 else{uint8_t tail[16]={0};memcpy(tail,src,available);input=simde_vld1q_u8(tail);}
 const uint8_t indices0[16]={WINDOW(w,0),WINDOW(w,1),WINDOW(w,2),WINDOW(w,3)};
 const uint8_t indices1[16]={WINDOW(w,4),WINDOW(w,5),WINDOW(w,6),WINDOW(w,7)};
 const int32_t shifts0[4]={-(int32_t)(0*w%8),-(int32_t)(1*w%8),-(int32_t)(2*w%8),-(int32_t)(3*w%8)};
 const int32_t shifts1[4]={-(int32_t)(4*w%8),-(int32_t)(5*w%8),-(int32_t)(6*w%8),-(int32_t)(7*w%8)};
 simde_uint32x4_t low=WORD_LANES32(simde_vqtbl1q_u8(input,simde_vld1q_u8(indices0)));
 simde_uint32x4_t high=WORD_LANES32(simde_vqtbl1q_u8(input,simde_vld1q_u8(indices1)));
 const simde_uint32x4_t mask=simde_vdupq_n_u32((UINT32_C(1)<<w)-1),base=simde_vdupq_n_u32(frame);
 low=simde_vandq_u32(simde_vshlq_u32(low,simde_vld1q_s32(shifts0)),mask);
 high=simde_vandq_u32(simde_vshlq_u32(high,simde_vld1q_s32(shifts1)),mask);
 simde_float32x4_t a=simde_vcvtq_f32_s32(simde_vreinterpretq_s32_u32(simde_vaddq_u32(low,base)));
 simde_float32x4_t b=simde_vcvtq_f32_s32(simde_vreinterpretq_s32_u32(simde_vaddq_u32(high,base)));
 a=simde_vmulq_n_f32(a,factor);a=simde_vmulq_n_f32(a,scale);
 b=simde_vmulq_n_f32(b,factor);b=simde_vmulq_n_f32(b,scale);
 simde_vst1q_u8(dst,simde_vreinterpretq_u8_f32(a));simde_vst1q_u8(dst+16,simde_vreinterpretq_u8_f32(b));
}
#define DEFINE32(W) \
static void decode32_##W(const uint8_t *src,size_t len,uint8_t *dst,size_t first,size_t n,uint32_t frame,float factor,float scale) { \
  const simde_uint32x4_t base=simde_vdupq_n_u32(frame); \
  for(;n&&(first%8);n--,first++,dst+=4) { const float v=value32(frame+(uint32_t)unpack(src,len,first*(W),(W)),factor,scale); memcpy(dst,&v,4); } \
  for (;n>=8;n-=8,first+=8,dst+=32) { \
    if((W)<=16){ const size_t byte=first*(W)/8;decode32_small8(src+byte,len-byte,dst,(W),frame,factor,scale); } \
    else { LOAD_BLOCK(W); const uint32_t deltas[8]={DELTAS(W)}; \
    for (unsigned j=0;j<8;j+=4) { \
      simde_float32x4_t v=simde_vcvtq_f32_s32(simde_vreinterpretq_s32_u32(simde_vaddq_u32(simde_vld1q_u32(deltas+j),base))); \
      v=simde_vmulq_n_f32(v,factor); v=simde_vmulq_n_f32(v,scale); simde_vst1q_u8(dst+j*4,simde_vreinterpretq_u8_f32(v)); \
    } } \
  } \
  for (;n;n--,first++,dst+=4) { const float v=value32(frame+(uint32_t)unpack(src,len,first*(W),(W)),factor,scale); memcpy(dst,&v,4); } \
}
#define DEFINE64(W) \
static void decode64_##W(const uint8_t *src,size_t len,uint8_t *dst,size_t first,size_t n,uint64_t frame,double factor,double scale) { \
  const simde_uint64x2_t base=simde_vdupq_n_u64(frame); \
  for(;n&&(first%8);n--,first++,dst+=8) { const double v=value64(frame+unpack(src,len,first*(W),(W)),factor,scale); memcpy(dst,&v,8); } \
  for (;n>=8;n-=8,first+=8,dst+=64) { \
    LOAD_BLOCK(W); const uint64_t deltas[8]={DELTAS(W)}; \
    for (unsigned j=0;j<8;j+=2) { \
      simde_float64x2_t v=simde_vcvtq_f64_s64(simde_vreinterpretq_s64_u64(simde_vaddq_u64(simde_vld1q_u64(deltas+j),base))); \
      v=simde_vmulq_n_f64(v,factor); v=simde_vmulq_n_f64(v,scale); simde_vst1q_u8(dst+j*8,simde_vreinterpretq_u8_f64(v)); \
    } \
  } \
  for (;n;n--,first++,dst+=8) { const double v=value64(frame+unpack(src,len,first*(W),(W)),factor,scale); memcpy(dst,&v,8); } \
}
WIDTHS32(DEFINE32)
WIDTHS64(DEFINE64)

void hs_columnar_unpack_for_f32(const uint8_t *src,size_t len,uint8_t *output,size_t offset,size_t first,size_t n,unsigned width,uint32_t frame,float factor,float scale) {
  if (!n) return;
  uint8_t *dst=output+offset;
  switch(width) {
    case 0: { const float value=value32(frame,factor,scale);const simde_uint8x16_t fill=simde_vreinterpretq_u8_f32(simde_vdupq_n_f32(value));size_t i=0;for(;n-i>=4;i+=4)simde_vst1q_u8(dst+i*4,fill);for(;i<n;i++)memcpy(dst+i*4,&value,4);return; }
#define CALL32(W) case W: decode32_##W(src,len,dst,first,n,frame,factor,scale); return;
    WIDTHS32(CALL32)
  }
}
void hs_columnar_unpack_for_f64(const uint8_t *src,size_t len,uint8_t *output,size_t offset,size_t first,size_t n,unsigned width,uint64_t frame,double factor,double scale) {
  if (!n) return;
  uint8_t *dst=output+offset;
  switch(width) {
    case 0: { const double value=value64(frame,factor,scale);const simde_uint8x16_t fill=simde_vreinterpretq_u8_f64(simde_vdupq_n_f64(value));size_t i=0;for(;n-i>=2;i+=2)simde_vst1q_u8(dst+i*8,fill);for(;i<n;i++)memcpy(dst+i*8,&value,8);return; }
#define CALL64(W) case W: decode64_##W(src,len,dst,first,n,frame,factor,scale); return;
    WIDTHS64(CALL64)
  }
}
void hs_columnar_scatter_le_f32(const uint8_t *positions,const uint8_t *values,uint8_t *output,size_t offset,size_t n) {
  if (!n) return;
  for(size_t i=0;i<n;i++) { const size_t pos=(size_t)positions[i*2]|((size_t)positions[i*2+1]<<8); const uint32_t bits=(uint32_t)little64(values+i*4,4); memcpy(output+offset+pos*4,&bits,4); }
}
void hs_columnar_scatter_le_f64(const uint8_t *positions,const uint8_t *values,uint8_t *output,size_t offset,size_t n) {
  if (!n) return;
  for(size_t i=0;i<n;i++) { const size_t pos=(size_t)positions[i*2]|((size_t)positions[i*2+1]<<8); const uint64_t bits=little64(values+i*8,8); memcpy(output+offset+pos*8,&bits,8); }
}
