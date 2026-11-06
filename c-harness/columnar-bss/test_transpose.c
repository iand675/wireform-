#include "harness.h"
#include "api.h"
static uint8_t *gap_of(wf_guarded g,size_t n,size_t gap,uint8_t canary){
    uint8_t *p=g.p+(g.n-n-gap);memset(p+n,canary,gap);return p;
}
static int gap_intact(const uint8_t *p,size_t n,size_t gap,uint8_t canary){
    for(size_t i=0;i<gap;i++)if(p[n+i]!=canary)return 0;return 1;
}
typedef void (*kernel)(const uint8_t *,uint8_t *,size_t);
static void check_width(size_t width,kernel enc,kernel dec) {
    const size_t maxn=65537,maxbytes=width*maxn;
    wf_rng rng=wf_rng_from_env();
    wf_guarded st=wf_guard_tail(maxbytes+32),dt=wf_guard_tail(maxbytes+32);
    wf_guarded sh=wf_guard_head(maxbytes),dh=wf_guard_head(maxbytes);
    uint8_t *expected=malloc(maxbytes),*original=malloc(maxbytes);
    const size_t extra[]={1023,1024,1025,8191,8192,8193,16383,16384,16385,65535,65536,65537};
    for(size_t sample=0;sample<259+sizeof(extra)/sizeof(extra[0]);sample++) {
        size_t n=sample<259?sample:extra[sample-259];
        size_t bytes=width*n;
        wf_rng_fill(&rng,original,bytes);
        ref_split_bytes(original,expected,width,n);
        for(size_t sg=0;sg<32;sg++) {
            size_t dg=(sg*7+11)%32;
            uint8_t *src=gap_of(st,bytes,sg,0x5a),*dst=gap_of(dt,bytes,dg,0xa5);
            memcpy(src,original,bytes);enc(src,dst,n);WF_CHECK_MEM(dst,expected,bytes);WF_CHECK(gap_intact(dst,bytes,dg,0xa5));WF_CHECK_MEM(src,original,bytes);
            memcpy(src,expected,bytes);memset(dst,0x33,bytes);dec(src,dst,n);WF_CHECK_MEM(dst,original,bytes);WF_CHECK(gap_intact(dst,bytes,dg,0xa5));WF_CHECK_MEM(src,expected,bytes);
        }
        memcpy(sh.p,original,bytes);enc(sh.p,dh.p,n);WF_CHECK_MEM(dh.p,expected,bytes);
        memcpy(sh.p,expected,bytes);dec(sh.p,dh.p,n);WF_CHECK_MEM(dh.p,original,bytes);
    }
    enc(NULL,NULL,0);dec(NULL,NULL,0);
    free(expected);free(original);wf_guard_free(st);wf_guard_free(dt);wf_guard_free(sh);wf_guard_free(dh);
}
static void four(void){check_width(4,hs_columnar_split_bytes4,hs_columnar_join_bytes4);}
static void eight(void){check_width(8,hs_columnar_split_bytes8,hs_columnar_join_bytes8);}
static void arrays(void){
    wf_rng rng=wf_rng_from_env();
    for(size_t width=4;width<=8;width+=4){
        for(size_t n=0;n<259;n++) for(size_t off=0;off<32;off++){
            size_t bytes=width*n;
            uint8_t *input=wf_alloc(bytes+off),*output=wf_alloc(bytes),*expected=malloc(bytes?bytes:1);
            wf_rng_fill(&rng,input,bytes+off);
            ref_split_bytes(input+off,expected,width,n);
            if(width==4)hs_columnar_split_bytes4_array(input,off,output,n);else hs_columnar_split_bytes8_array(input,off,output,n);
            WF_CHECK_MEM(output,expected,bytes);free(expected);wf_free(input,bytes+off);wf_free(output,bytes);
        }
    }
    hs_columnar_split_bytes4_array(NULL,0,NULL,0);hs_columnar_split_bytes8_array(NULL,0,NULL,0);
}
int main(void){WF_RUN(four);WF_RUN(eight);WF_RUN(arrays);return wf_finish("columnar-bss test_transpose");}
