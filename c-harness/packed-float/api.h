#ifndef PACKED_FLOAT_API_H
#define PACKED_FLOAT_API_H
#include <stddef.h>
#include <stdint.h>
#include <string.h>
/* Generic packed integer/FOR kernels. Width<=32/64; packed_len covers
   ceil((first+n)*width/8), destination covers offset+n*4/8. Scaling uses
   two separate operations in the output precision. Sources/destination
   are disjoint and permit any alignment. Count0 permits NULL.
   Scatter uses validated uint16 LE positions and LE IEEE values. */
void hs_columnar_unpack_for_f32(const uint8_t *,size_t,uint8_t *,size_t,size_t,size_t,unsigned,uint32_t,float,float);
void hs_columnar_unpack_for_f64(const uint8_t *,size_t,uint8_t *,size_t,size_t,size_t,unsigned,uint64_t,double,double);
void hs_columnar_scatter_le_f32(const uint8_t *,const uint8_t *,uint8_t *,size_t,size_t);
void hs_columnar_scatter_le_f64(const uint8_t *,const uint8_t *,uint8_t *,size_t,size_t);
static uint64_t ref_delta(const uint8_t *src,size_t row,unsigned width){
 uint64_t result=0;for(unsigned bit=0;bit<width;bit++){size_t index=row*width+bit;if(src[index/8]&(1u<<(index%8)))result|=UINT64_C(1)<<bit;}return result;
}
static const float ref_pos32[]={1e0f,1e1f,1e2f,1e3f,1e4f,1e5f,1e6f,1e7f,1e8f,1e9f,1e10f};
static const float ref_neg32[]={1e0f,1e-1f,1e-2f,1e-3f,1e-4f,1e-5f,1e-6f,1e-7f,1e-8f,1e-9f,1e-10f};
static const double ref_pos64[]={1e0,1e1,1e2,1e3,1e4,1e5,1e6,1e7,1e8,1e9,1e10,1e11,1e12,1e13,1e14,1e15,1e16,1e17,1e18};
static const double ref_neg64[]={1e0,1e-1,1e-2,1e-3,1e-4,1e-5,1e-6,1e-7,1e-8,1e-9,1e-10,1e-11,1e-12,1e-13,1e-14,1e-15,1e-16,1e-17,1e-18};
static void ref_values(size_t physical,const uint8_t *src,uint8_t *dst,size_t first,size_t n,unsigned width,uint64_t frame,unsigned e,unsigned f){
 for(size_t i=0;i<n;i++){
  if(physical==4){uint32_t bits=(uint32_t)frame+(uint32_t)ref_delta(src,first+i,width);int32_t integer;memcpy(&integer,&bits,4);volatile float a=(float)integer;volatile float b=a*ref_pos32[f];float value=b*ref_neg32[e];memcpy(dst+i*4,&value,4);}
  else{uint64_t bits=frame+ref_delta(src,first+i,width);int64_t integer;memcpy(&integer,&bits,8);volatile double a=(double)integer;volatile double b=a*ref_pos64[f];double value=b*ref_neg64[e];memcpy(dst+i*8,&value,8);}
 }
}
static void ref_patch(size_t physical,const uint8_t *positions,const uint8_t *values,uint8_t *output,size_t offset,size_t n){
 for(size_t i=0;i<n;i++){size_t pos=(size_t)positions[2*i]|((size_t)positions[2*i+1]<<8);uint64_t bits=0;for(size_t j=0;j<physical;j++)bits|=(uint64_t)values[i*physical+j]<<(j*8);if(physical==4){uint32_t narrow=(uint32_t)bits;memcpy(output+offset+pos*physical,&narrow,4);}else memcpy(output+offset+pos*physical,&bits,8);}
}
#endif
