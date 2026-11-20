#include "harness.h"
#include "api.h"
int LLVMFuzzerTestOneInput(const uint8_t *data,size_t size){
 hs_columnar_unpack_for_f32(NULL,0,NULL,0,0,0,0,0,1,1);hs_columnar_unpack_for_f64(NULL,0,NULL,0,0,0,0,0,1,1);hs_columnar_scatter_le_f32(NULL,NULL,NULL,0,0);hs_columnar_scatter_le_f64(NULL,NULL,NULL,0,0);
 if(size<10)return 0;
 size_t physical=(data[0]&1)?8:4,n=((size_t)data[2]|((size_t)data[3]<<8))%512,first=data[4]&15,off=data[7]&31;
 unsigned width=data[1]%(physical*8+1),e=data[5]%(physical==4?11:19),f=data[6]%(e+1);
 size_t bytes=((first+n)*width+7)/8,out=off+n*physical+31;
 uint64_t frame=0;for(unsigned j=0;j<8;j++)frame|=(uint64_t)data[(j+2)%size]<<(j*8);
 uint8_t *src=wf_alloc(bytes),*dst=wf_alloc(out),*want=wf_alloc(out);
 for(size_t j=0;j<bytes;j++)src[j]=data[(j+10)%size];memset(dst,0x9b,out);memset(want,0x9b,out);
 ref_values(physical,src,want+off,first,n,width,frame,e,f);
 if(physical==4)hs_columnar_unpack_for_f32(src,bytes,dst,off,first,n,width,(uint32_t)frame,ref_pos32[f],ref_neg32[e]);else hs_columnar_unpack_for_f64(src,bytes,dst,off,first,n,width,frame,ref_pos64[f],ref_neg64[e]);
 WF_FUZZ_ASSERT(memcmp(dst,want,out)==0);for(size_t j=0;j<bytes;j++)WF_FUZZ_ASSERT(src[j]==data[(j+10)%size]);
 size_t patches=n?data[8]%65:0;uint8_t *positions=wf_alloc(patches*2),*values=wf_alloc(patches*physical);
 for(size_t j=0;j<patches;j++){size_t pos=((size_t)data[(j+1)%size]*257+j)%n;positions[2*j]=(uint8_t)pos;positions[2*j+1]=(uint8_t)(pos>>8);}
 for(size_t j=0;j<patches*physical;j++)values[j]=data[(j+3)%size];ref_patch(physical,positions,values,want,off,patches);
 if(physical==4)hs_columnar_scatter_le_f32(positions,values,dst,off,patches);else hs_columnar_scatter_le_f64(positions,values,dst,off,patches);
 WF_FUZZ_ASSERT(memcmp(dst,want,out)==0);for(size_t j=0;j<patches*physical;j++)WF_FUZZ_ASSERT(values[j]==data[(j+3)%size]);
 wf_free(src,bytes);wf_free(dst,out);wf_free(want,out);wf_free(positions,patches*2);wf_free(values,patches*physical);return 0;
}
