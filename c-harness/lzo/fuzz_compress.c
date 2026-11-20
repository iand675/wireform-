#include "harness.h"
#include "api.h"
int LLVMFuzzerTestOneInput(const uint8_t *data,size_t size) {
 size_t bound=hs_columnar_lzo1x_compress_bound(size);if(!bound)return 0;
 uint8_t *src=wf_dup(data,size),*packed=wf_alloc(bound+32),*out=wf_alloc(size+32),*literal=wf_alloc(bound);
 memset(packed,0xa5,bound+32);memset(out,0x5a,size+32);size_t n=0,decoded=0;
 WF_FUZZ_ASSERT(hs_columnar_lzo1x_compress(src,size,packed+16,bound,&n)==0 && n<=bound);
 WF_FUZZ_ASSERT(memcmp(src,data,size)==0);
 WF_FUZZ_ASSERT(hs_columnar_lzo1x_measure(packed+16,n,size,&decoded)==0 && decoded==size);
 WF_FUZZ_ASSERT(hs_columnar_lzo1x_decompress(packed+16,n,out+16,size,&decoded)==0 && decoded==size);
 WF_FUZZ_ASSERT(memcmp(out+16,data,size)==0);
 for(size_t i=0;i<16;i++){WF_FUZZ_ASSERT(packed[i]==0xa5 && packed[16+bound+i]==0xa5);WF_FUZZ_ASSERT(out[i]==0x5a && out[16+size+i]==0x5a);}
 size_t literal_size=ref_literals(src,size,literal);
 WF_FUZZ_ASSERT(hs_columnar_lzo1x_decompress(literal,literal_size,out+16,size,&decoded)==0 && decoded==size);
 WF_FUZZ_ASSERT(memcmp(out+16,data,size)==0);
 if(size)WF_FUZZ_ASSERT(hs_columnar_lzo1x_measure(packed+16,n,size-1,&decoded)!=0);
 for(size_t cut=n>3?n-3:0;cut<n;cut++)WF_FUZZ_ASSERT(hs_columnar_lzo1x_measure(packed+16,cut,size,&decoded)!=0);
 size_t bad=999;WF_FUZZ_ASSERT(hs_columnar_lzo1x_compress(src,size,packed+16,bound-1,&bad)!=0 && bad==0);
 wf_free(src,size);wf_free(packed,bound+32);wf_free(out,size+32);wf_free(literal,bound);return 0;
}
