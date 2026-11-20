#include "harness.h"
#include "api.h"
int LLVMFuzzerTestOneInput(const uint8_t *data,size_t size){
    if(size<2){
        hs_columnar_split_bytes4(NULL,NULL,0);hs_columnar_join_bytes4(NULL,NULL,0);
        hs_columnar_split_bytes8(NULL,NULL,0);hs_columnar_join_bytes8(NULL,NULL,0);
        hs_columnar_split_bytes4_array(NULL,0,NULL,0);hs_columnar_split_bytes8_array(NULL,0,NULL,0);
        return 0;
    }
    size_t width=(data[0]&1)?8:4,off=data[1]&31,n=(size-2)/width,bytes=width*n;
    uint8_t *base=wf_alloc(off+bytes),*encoded=wf_alloc(bytes),*decoded=wf_alloc(bytes),*want=wf_alloc(bytes);
    memset(base,0xa5,off);memcpy(base+off,data+2,bytes);ref_split_bytes(base+off,want,width,n);
    if(width==4){hs_columnar_split_bytes4(base+off,encoded,n);hs_columnar_join_bytes4(want,decoded,n);}
    else{hs_columnar_split_bytes8(base+off,encoded,n);hs_columnar_join_bytes8(want,decoded,n);}
    WF_FUZZ_ASSERT(memcmp(encoded,want,bytes)==0);WF_FUZZ_ASSERT(memcmp(decoded,base+off,bytes)==0);
    memset(encoded,0x5a,bytes);
    if(width==4)hs_columnar_split_bytes4_array(base,off,encoded,n);else hs_columnar_split_bytes8_array(base,off,encoded,n);
    WF_FUZZ_ASSERT(memcmp(encoded,want,bytes)==0);WF_FUZZ_ASSERT(memcmp(base+off,data+2,bytes)==0);
    for(size_t i=0;i<off;i++)WF_FUZZ_ASSERT(base[i]==0xa5);
    wf_free(base,off+bytes);wf_free(encoded,bytes);wf_free(decoded,bytes);wf_free(want,bytes);return 0;
}
