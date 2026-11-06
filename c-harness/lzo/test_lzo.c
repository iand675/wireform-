#include "harness.h"
#include "api.h"
#include "reference_vectors.h"
static void decode_case(const uint8_t *packed,size_t pn,const uint8_t *plain,size_t n) {
 wf_guarded sg=wf_guard_tail(pn+31),dg=wf_guard_tail(n+63);
 for(size_t gap=0;gap<32;gap++){
  uint8_t *src=sg.p+sg.n-pn-gap,*dst=dg.p+dg.n-n-gap;memcpy(src,packed,pn);memset(dst-31,0xa5,n+gap+31);
  size_t a=0,b=0;WF_CHECK(hs_columnar_lzo1x_measure(src,pn,n,&a)==0 && a==n);
  WF_CHECK(hs_columnar_lzo1x_decompress(src,pn,dst,n,&b)==0 && b==n);WF_CHECK_MEM(dst,plain,n);WF_CHECK_MEM(src,packed,pn);
  for(size_t i=0;i<31;i++)WF_CHECK(dst[-(ptrdiff_t)i-1]==0xa5);
  for(size_t i=0;i<gap;i++)WF_CHECK(dst[n+i]==0xa5);
  if(n){memset(dst,0xa5,n+gap);WF_CHECK(hs_columnar_lzo1x_decompress(src,pn,dst,n-1,&b)!=0 && b<=n-1);WF_CHECK(dst[n-1]==0xa5);}
 }
 /* Exact advertised input extents, so every truncation boundary is guarded. */
 for(size_t cut=0;cut<pn;cut++){
  if(pn>128 && cut>=8 && cut<pn-16)continue;
  wf_guarded short_src=wf_guard_tail(cut);memcpy(short_src.p,packed,cut);size_t a=0,b=0;
  int ma=hs_columnar_lzo1x_measure(short_src.p,cut,n,&a),de=hs_columnar_lzo1x_decompress(short_src.p,cut,dg.p,n,&b);
  WF_CHECK(ma!=0 && ma==de && a==b && b<=n);wf_guard_free(short_src);
 }
 wf_guard_free(sg);wf_guard_free(dg);
}
static void reference_vectors(void){
 for(size_t i=0;i<sizeof(ref_vectors)/sizeof(ref_vectors[0]);i++){
  const struct ref_vector *r=&ref_vectors[i];uint8_t *plain=malloc(r->size?r->size:1);payload(plain,r->size,r->kind,r->seed);decode_case(r->packed,r->packed_size,plain,r->size);free(plain);
 }
}
static void roundtrips(void){
 const size_t lengths[]={0,1,2,3,4,5,7,8,9,15,16,17,18,19,31,32,33,127,128,129,238,239,255,256,257,1023,1024,1025,4095,4096,4097,65535,65536,65537};
 for(size_t k=0;k<sizeof(lengths)/sizeof(lengths[0]);k++)for(unsigned kind=0;kind<4;kind++){
  size_t n=lengths[k],bound=hs_columnar_lzo1x_compress_bound(n);uint8_t *plain=malloc(n?n:1),*literal=malloc(bound);payload(plain,n,kind,12345);
  size_t rn=ref_literals(plain,n,literal);decode_case(literal,rn,plain,n);
  wf_guarded sg=wf_guard_tail(n+31),cg=wf_guard_tail(bound+63);
  for(size_t gap=0;gap<32;gap++){
   uint8_t *src=sg.p+sg.n-n-gap,*packed=cg.p+cg.n-bound-gap;memcpy(src,plain,n);memset(packed-31,0xa5,bound+gap+31);size_t written=0;
   WF_CHECK(hs_columnar_lzo1x_compress(src,n,packed,bound,&written)==0 && written<=bound);WF_CHECK_MEM(src,plain,n);
   size_t actual=0;uint8_t *out=malloc(n?n:1);WF_CHECK(hs_columnar_lzo1x_decompress(packed,written,out,n,&actual)==0 && actual==n);WF_CHECK_MEM(out,plain,n);free(out);
   for(size_t i=0;i<31;i++)WF_CHECK(packed[-(ptrdiff_t)i-1]==0xa5);
   for(size_t i=0;i<gap;i++)WF_CHECK(packed[bound+i]==0xa5);
  }
  wf_guard_free(sg);wf_guard_free(cg);free(plain);free(literal);
 }
}
static size_t extension(uint8_t *p,size_t at,size_t value){while(value>255){p[at++]=0;value-=255;}p[at++]=(uint8_t)value;return at;}
static void match_copy_boundaries(void){
 const size_t distances[]={1,2,3,4,7,8,9,15,16,17,31,32,33};
 const size_t lengths[]={3,4,7,8,9,15,16,17,31,32,33,63,64,65,255,256,257};
 for(size_t d=0;d<sizeof(distances)/sizeof(*distances);d++)for(size_t k=0;k<sizeof(lengths)/sizeof(*lengths);k++){
  size_t distance=distances[d],length=lengths[k],prefix=distance<4?4:distance,n=prefix+length;
  uint8_t *plain=malloc(n),*packed=malloc(n+n/255+67);payload(plain,prefix,3,12345);
  size_t at=ref_literals(plain,prefix,packed)-3;
  packed[at++]=(uint8_t)(length<=33?32+length-2:32);
  if(length>33)at=extension(packed,at,length-33);
  size_t word=(distance-1)<<2;packed[at++]=(uint8_t)word;packed[at++]=(uint8_t)(word>>8);
  packed[at++]=17;packed[at++]=0;packed[at++]=0;
  for(size_t i=0;i<length;i++)plain[prefix+i]=plain[prefix+i-distance];
  decode_case(packed,at,plain,n);free(plain);free(packed);
 }
}
static void match_prefix_boundaries(void){
 for(size_t length=4;length<64;length++){
  uint8_t plain[128],packed[128+128/16+67],out[128];payload(plain,sizeof(plain),3,12345);
  for(size_t i=0;i<length;i++)plain[16+i]=plain[i];
  plain[16+length]=(uint8_t)(plain[length]^1);
  size_t packed_size=0,actual=0;
  WF_CHECK(hs_columnar_lzo1x_compress(plain,sizeof(plain),packed,sizeof(packed),&packed_size)==0);
  WF_CHECK(hs_columnar_lzo1x_decompress(packed,packed_size,out,sizeof(out),&actual)==0 && actual==sizeof(plain));
  WF_CHECK_MEM(out,plain,sizeof(plain));
 }
}
static void instruction_classes(void){
 const size_t lengths[]={3,4,5,8,9,10,33,34,264,2048,65537};
 for(unsigned kind=1;kind<=4;kind++)for(unsigned tail=0;tail<4;tail++)for(size_t k=0;k<sizeof(lengths)/sizeof(lengths[0]);k++){
  size_t length=lengths[k];if((kind==1&&length!=3)||(kind==2&&length>8))continue;
  size_t prefix=kind==1?4096:kind==2?2048:kind==3?16384:49151;
  size_t distance=kind==1?2049+tail*100:kind==2?1+tail*600:kind==3?16384:16385+tail*10922;
  size_t n=prefix+length+tail;uint8_t *plain=malloc(n),*packed=malloc(n+n/255+512);payload(plain,prefix,3,12345);
  size_t at=ref_literals(plain,prefix,packed)-3;
  if(kind==1){size_t d=distance-2049;packed[at++]=(uint8_t)(((d&3)<<2)|tail);packed[at++]=(uint8_t)(d>>2);}
  else if(kind==2){size_t d=distance-1;packed[at++]=(uint8_t)(((length-1)<<5)|((d&7)<<2)|tail);packed[at++]=(uint8_t)(d>>3);}
  else{size_t d=distance-(kind==3?1:16384),direct=kind==3?33:9;unsigned op=kind==3?32:16|((d>>14)<<3);if(length<=direct)op|=(unsigned)(length-2);packed[at++]=(uint8_t)op;if(length>direct)at=extension(packed,at,length-direct);unsigned word=(unsigned)((d&0x3fff)<<2)|tail;packed[at++]=(uint8_t)word;packed[at++]=(uint8_t)(word>>8);}
  for(size_t i=0;i<length;i++)plain[prefix+i]=plain[prefix+i-distance];
  for(unsigned i=0;i<tail;i++){plain[prefix+length+i]=(uint8_t)(128+i);packed[at++]=(uint8_t)(128+i);}
  packed[at++]=17;packed[at++]=0;packed[at++]=0;decode_case(packed,at,plain,n);free(plain);free(packed);
 }
 for(unsigned prefix=1;prefix<4;prefix++)for(unsigned tail=0;tail<4;tail++){
  uint8_t plain[8],packed[16];size_t at=0;packed[at++]=(uint8_t)(17+prefix);
  for(unsigned i=0;i<prefix;i++){plain[i]=(uint8_t)(40+i);packed[at++]=plain[i];}
  packed[at++]=(uint8_t)tail;packed[at++]=0;plain[prefix]=plain[prefix-1];plain[prefix+1]=plain[prefix-1];
  for(unsigned i=0;i<tail;i++){plain[prefix+2+i]=(uint8_t)i;packed[at++]=(uint8_t)i;}
  packed[at++]=17;packed[at++]=0;packed[at++]=0;decode_case(packed,at,plain,prefix+2+tail);
 }
}
static void boundaries(void){
 size_t result=999;uint8_t byte=0;
 WF_CHECK(hs_columnar_lzo1x_compress_bound(0)==67);
 WF_CHECK(hs_columnar_lzo1x_compress_bound(SIZE_MAX)==0);
 if(SIZE_MAX>UINT32_MAX){size_t maximum=UINT32_MAX;WF_CHECK(hs_columnar_lzo1x_compress_bound(maximum)==maximum+maximum/16+67);WF_CHECK(hs_columnar_lzo1x_compress_bound(maximum+1)==0);}
 WF_CHECK(hs_columnar_lzo1x_measure(NULL,0,SIZE_MAX,&result)==1 && result==0);
 WF_CHECK(hs_columnar_lzo1x_measure(NULL,SIZE_MAX,0,NULL)==2);
 WF_CHECK(hs_columnar_lzo1x_decompress(NULL,SIZE_MAX,NULL,1,&result)==2 && result==0);
 WF_CHECK(hs_columnar_lzo1x_compress(NULL,SIZE_MAX,NULL,0,&result)==5 && result==0);
 WF_CHECK(hs_columnar_lzo1x_compress(NULL,1,NULL,0,&result)==2 && result==0);
 WF_CHECK(hs_columnar_lzo1x_compress(NULL,SIZE_MAX,NULL,SIZE_MAX,NULL)==2);
 /* Invalid empty stream produced by lzokay: retain the independent finding. */
 const uint8_t bad_empty[]={17,17,0,0},end[]={17,0,0},trailing[]={17,0,0,0};
 WF_CHECK(hs_columnar_lzo1x_measure(bad_empty,sizeof(bad_empty),0,&result)!=0);
 WF_CHECK(hs_columnar_lzo1x_measure(end,sizeof(end),SIZE_MAX,&result)==0 && result==0);
 WF_CHECK(hs_columnar_lzo1x_decompress(end,sizeof(end),NULL,0,&result)==0 && result==0);
 WF_CHECK(hs_columnar_lzo1x_measure(trailing,sizeof(trailing),0,&result)==4);
 WF_CHECK(hs_columnar_lzo1x_compress(NULL,0,&byte,0,&result)==2 && result==0 && byte==0);
}
int main(void){WF_RUN(boundaries);WF_RUN(reference_vectors);WF_RUN(instruction_classes);WF_RUN(match_copy_boundaries);WF_RUN(match_prefix_boundaries);WF_RUN(roundtrips);return wf_finish("lzo test_lzo");}
