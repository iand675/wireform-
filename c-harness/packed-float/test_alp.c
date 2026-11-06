#include "harness.h"
#include "api.h"
static uint8_t *tail(wf_guarded g,size_t n,size_t gap){return g.p+g.n-n-gap;}
static void call(size_t physical,const uint8_t *src,size_t bytes,uint8_t *dst,size_t off,size_t first,size_t n,unsigned width,uint64_t frame,unsigned e,unsigned f){
 if(physical==4)hs_columnar_unpack_for_f32(src,bytes,dst,off,first,n,width,(uint32_t)frame,ref_pos32[f],ref_neg32[e]);else hs_columnar_unpack_for_f64(src,bytes,dst,off,first,n,width,frame,ref_pos64[f],ref_neg64[e]);
}
static void values(void){
 wf_rng rng=wf_rng_from_env();const size_t maximum=32769,maxbytes=maximum*8;
 wf_guarded sg=wf_guard_tail(maxbytes+32),dg=wf_guard_tail(maxbytes+96);
 uint8_t *want=malloc(maxbytes),*original=malloc(maxbytes);
 const size_t counts[]={0,1,2,3,7,8,9,15,16,17,31,32,33,63,64,65,255,256,257,1023,1024,1025,8191,8192,8193,16383,16384,16385,32767,32768};
 for(size_t physical=4;physical<=8;physical+=4)for(unsigned width=0;width<=physical*8;width++)for(size_t sample=0;sample<sizeof(counts)/sizeof(counts[0]);sample++){
  size_t n=counts[sample],first=sample<19?sample%8:0,bytes=((first+n)*width+7)/8,out=n*physical;wf_rng_fill(&rng,original,bytes);
  unsigned e=physical==4?10:18,f=sample%(e+1);uint64_t frame=sample%2?UINT64_C(0x8000000000000000):UINT64_C(0x80000000);ref_values(physical,original,want,first,n,width,frame,e,f);
  size_t alignments=n<258?32:3;
  for(size_t gap=0;gap<alignments;gap++){
   size_t off=(gap*5+3)%32,dgap=(gap*7)%32;uint8_t *src=tail(sg,bytes,gap),*dst=tail(dg,out+off,dgap);memcpy(src,original,bytes);memset(dst,0x5a,out+off+dgap);
   call(physical,src,bytes,dst,off,first,n,width,frame,e,f);WF_CHECK_MEM(dst+off,want,out);WF_CHECK_MEM(src,original,bytes);for(size_t i=0;i<off;i++)WF_CHECK(dst[i]==0x5a);for(size_t i=0;i<dgap;i++)WF_CHECK(dst[off+out+i]==0x5a);
  }
 }
 for(size_t physical=4;physical<=8;physical+=4)for(unsigned e=0;e<=(physical==4?10u:18u);e++)for(unsigned f=0;f<=e;f++){
  size_t n=19,bytes=n*physical;wf_rng_fill(&rng,original,bytes);ref_values(physical,original,want,0,n,(unsigned)physical*8,UINT64_C(0x8000000080000000),e,f);call(physical,original,bytes,dg.p,0,0,n,(unsigned)physical*8,UINT64_C(0x8000000080000000),e,f);WF_CHECK_MEM(dg.p,want,bytes);
 }
 call(4,NULL,0,NULL,0,0,0,0,0,0,0);call(8,NULL,0,NULL,0,0,0,0,0,0,0);
 free(want);free(original);wf_guard_free(sg);wf_guard_free(dg);
}
static void patches(void){
 wf_rng rng=wf_rng_from_env();const size_t rows=257;uint8_t *want=malloc(rows*8+32);
 for(size_t physical=4;physical<=8;physical+=4)for(size_t n=0;n<258;n++){
  wf_guarded pg=wf_guard_tail(n*2+32),vg=wf_guard_tail(n*physical+32),dg=wf_guard_tail(rows*physical+64);
  for(size_t gap=0;gap<32;gap++){
   size_t off=(gap*5+1)%32;uint8_t *positions=tail(pg,n*2,gap),*v=tail(vg,n*physical,gap),*dst=tail(dg,rows*physical+off,gap);wf_rng_fill(&rng,v,n*physical);
   for(size_t i=0;i<n;i++){size_t p=(i*71)%rows;positions[2*i]=(uint8_t)p;positions[2*i+1]=(uint8_t)(p>>8);}
   memset(dst,0x6b,rows*physical+off+gap);memset(want,0x6b,rows*physical+off);ref_patch(physical,positions,v,want,off,n);
   if(physical==4)hs_columnar_scatter_le_f32(positions,v,dst,off,n);else hs_columnar_scatter_le_f64(positions,v,dst,off,n);WF_CHECK_MEM(dst,want,rows*physical+off);for(size_t i=0;i<gap;i++)WF_CHECK(dst[rows*physical+off+i]==0x6b);
  }
  wf_guard_free(pg);wf_guard_free(vg);wf_guard_free(dg);
 }
 hs_columnar_scatter_le_f32(NULL,NULL,NULL,0,0);hs_columnar_scatter_le_f64(NULL,NULL,NULL,0,0);free(want);
}
int main(void){WF_RUN(values);WF_RUN(patches);return wf_finish("packed-float test_alp");}
