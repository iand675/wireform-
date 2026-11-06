#include "harness.h"
#include "api.h"
int LLVMFuzzerTestOneInput(const uint8_t *data,size_t size) {
 if(size<2)return 0;
 size_t cap=(size_t)data[0]|((size_t)data[1]<<8),n=size-2,a=0,b=0;
 uint8_t *src=wf_dup(data+2,n),*out=wf_alloc(cap+32);memset(out,0xa5,cap+32);
 int measure=hs_columnar_lzo1x_measure(src,n,cap,&a);
 int decode=hs_columnar_lzo1x_decompress(src,n,out+16,cap,&b);
 WF_FUZZ_ASSERT(measure==decode && a==b && b<=cap);
 WF_FUZZ_ASSERT(memcmp(src,data+2,n)==0);
 for(size_t i=0;i<16;i++){WF_FUZZ_ASSERT(out[i]==0xa5);WF_FUZZ_ASSERT(out[16+cap+i]==0xa5);}
 if(decode==0){size_t bound=hs_columnar_lzo1x_compress_bound(b),written=0;uint8_t *packed=wf_alloc(bound),*again=wf_alloc(cap);
  WF_FUZZ_ASSERT(hs_columnar_lzo1x_compress(out+16,b,packed,bound,&written)==0 && written<=bound);
  size_t actual=0;WF_FUZZ_ASSERT(hs_columnar_lzo1x_decompress(packed,written,again,cap,&actual)==0 && actual==b);
  WF_FUZZ_ASSERT(memcmp(again,out+16,b)==0);wf_free(packed,bound);wf_free(again,cap);
 }
 wf_free(src,n);wf_free(out,cap+32);return 0;
}
