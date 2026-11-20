/* Portable raw LZO1X codec, with checked size_t offsets and explicit budgets.
 * Offset-based rewrite informed by the MIT lzokay decoder, with no pointer
 * formation until the corresponding integer range has been checked.
 *
 * Copyright (c) 2018 Jack Andersen
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
#include <stddef.h>
#include <stdint.h>
#include <string.h>

enum { LZO_OK=0, LZO_INPUT=1, LZO_OUTPUT=2, LZO_HISTORY=3, LZO_END=4,
       LZO_OVERFLOW=5 };

#if defined(__GNUC__) || defined(__clang__)
#define LZO_INLINE static inline __attribute__((always_inline))
#else
#define LZO_INLINE static inline
#endif

/* Short runs dominate mixed-data pages. Fixed-width memcpy calls become
 * unaligned-safe loads/stores without a library call or any over-read.
 * The head/tail copies can cover some bytes twice, always within len. */
LZO_INLINE void copy_disjoint(uint8_t *dst,const uint8_t *src,size_t len) {
  if(len>16) {
    memcpy(dst,src,len);
  } else if(len>=8) {
    uint64_t head,tail;
    memcpy(&head,src,8);memcpy(&tail,src+len-8,8);
    memcpy(dst,&head,8);memcpy(dst+len-8,&tail,8);
  } else if(len>=4) {
    uint32_t head,tail;
    memcpy(&head,src,4);memcpy(&tail,src+len-4,4);
    memcpy(dst,&head,4);memcpy(dst+len-4,&tail,4);
  } else if(len>=2) {
    uint16_t head;memcpy(&head,src,2);memcpy(dst,&head,2);
    dst[len-1]=src[len-1];
  } else if(len) dst[0]=src[0];
}

/* All counters remain <= their extents. The mode dst==NULL validates and
 * measures without producing data or dereferencing history. No allocations.
 * cap is an explicit output budget, including in the measuring pass.
 */
static int length_extension(const uint8_t *src, size_t n, size_t *ip,
                            size_t base, size_t *length) {
  size_t value=base;
  for (;;) {
    if (*ip==n) return LZO_INPUT;
    unsigned b=src[(*ip)++];
    size_t add=b ? b : 255;
    if (value>SIZE_MAX-add) return LZO_OVERFLOW;
    value+=add;
    if (b) { *length=value; return LZO_OK; }
  }
}
static int literals(const uint8_t *src,size_t n,size_t *ip,
                    uint8_t *dst,size_t cap,size_t *op,size_t len) {
  if(len>n-*ip) return LZO_INPUT;
  if(len>cap-*op) return LZO_OUTPUT;
  if(dst && len) copy_disjoint(dst+*op,src+*ip,len);
  *ip+=len; *op+=len; return LZO_OK;
}
static unsigned le16(const uint8_t *p) {
  return (unsigned)p[0] | ((unsigned)p[1]<<8);
}
/* Specialize validation (dst==NULL) separately from decoding so measuring
 * never branches on an output pointer inside the instruction loop. */
LZO_INLINE int decode_lzo1x(const uint8_t *src,size_t n,uint8_t *dst,size_t cap,
                size_t *written) {
  size_t ip=0,op=0,len,dist; unsigned state=0,next=0; int rc;
  if(!written) return LZO_OUTPUT;
  *written=0;
  if(n<3) return LZO_INPUT;
  if(src[0]>17) {
    len=(size_t)src[ip++]-17;
    rc=literals(src,n,&ip,dst,cap,&op,len);
    if(rc) goto fail;
    state=len<4 ? (unsigned)len : 4;
  }
  for(;;) {
    if(ip==n) { rc=LZO_INPUT; goto fail; }
    unsigned inst=src[ip++];
    if(inst>=64) {
      if(ip==n) { rc=LZO_INPUT; goto fail; }
      dist=((size_t)src[ip++]<<3)+((inst>>2)&7)+1;
      len=(inst>>5)+1; next=inst&3;
    } else if(inst>=32) {
      len=(inst&31)+2;
      if(len==2) {
        rc=length_extension(src,n,&ip,33,&len); if(rc) goto fail;
      }
      if(n-ip<2) { rc=LZO_INPUT; goto fail; }
      unsigned word=le16(src+ip); ip+=2;
      dist=(word>>2)+1; next=word&3;
    } else if(inst>=16) {
      len=(inst&7)+2;
      if(len==2) {
        rc=length_extension(src,n,&ip,9,&len); if(rc) goto fail;
      }
      if(n-ip<2) { rc=LZO_INPUT; goto fail; }
      unsigned word=le16(src+ip); ip+=2;
      dist=((size_t)(inst&8)<<11)+(word>>2); next=word&3;
      if(dist==0) {
        *written=op;
        return len==3 && ip==n ? LZO_OK : LZO_END;
      }
      dist+=16384;
    } else if(state==0) {
      len=inst+3;
      if(inst==0) {
        rc=length_extension(src,n,&ip,18,&len); if(rc) goto fail;
      }
      rc=literals(src,n,&ip,dst,cap,&op,len); if(rc) goto fail;
      state=4; continue;
    } else {
      if(ip==n) { rc=LZO_INPUT; goto fail; }
      dist=(inst>>2)+((size_t)src[ip++]<<2)+(state==4 ? 2049 : 1);
      len=state==4 ? 3 : 2; next=inst&3;
    }
    if(dist>op) { rc=LZO_HISTORY; goto fail; }
    if(len>cap-op) { rc=LZO_OUTPUT; goto fail; }
    if(dst) {
      size_t from=op-dist;
      if(dist>=len) {
        copy_disjoint(dst+op,dst+from,len);
      } else if(dist==1) {
        memset(dst+op,dst[from],len);
      } else if(len>=16) {
        /* Seed one disjoint period, then double the valid prefix. Every
         * memcpy has non-overlapping, fully initialized source/dest ranges;
         * no word loads extend beyond the advertised output buffer. */
        copy_disjoint(dst+op,dst+from,dist);
        size_t copied=dist;
        while(copied<len) {
          size_t chunk=len-copied;
          if(chunk>copied) chunk=copied;
          copy_disjoint(dst+op+copied,dst+op,chunk);
          copied+=chunk;
        }
      } else {
        for(size_t j=0;j<len;j++) dst[op+j]=dst[from+j];
      }
    }
    op+=len;
    /* Most matches have no trailing literals. Avoid two redundant bounds
     * checks and offset updates in both validation and decoding in that case. */
    if(next) {
      rc=literals(src,n,&ip,dst,cap,&op,next); if(rc) goto fail;
    }
    state=next;
  }
fail:
  *written=op; return rc;
}

/* The encoder emits ordinary LZO1X M3 matches only.
 * Four-byte minimum matches pay for literal-run headers; the standard LZO
 * worst-case budget n+n/16+67 is therefore conservative. The caller provides
 * disjoint input/output and at least that budget; positions are bounded to
 * UINT32_MAX because Parquet's page size is an int32. No allocator or global
 * mutable state: the per-call dictionary occupies 32 KiB on the C stack.
 */
static unsigned literal_hash(const uint8_t *p) {
  /* An eight-byte prefix keeps short periodic prefixes from evicting
   * longer matches. Callers leave the final seven bytes unhashed. */
  uint32_t key=(uint32_t)p[0]|((uint32_t)p[1]<<8)|((uint32_t)p[2]<<16)|((uint32_t)p[3]<<24);
  uint32_t high=(uint32_t)p[4]|((uint32_t)p[5]<<8)|((uint32_t)p[6]<<16)|((uint32_t)p[7]<<24);
  key^=high*UINT32_C(0x9e3779b1);
  return (key*UINT32_C(0x1e35a7bd))>>18;
}
static void put_extension(uint8_t *dst,size_t *op,size_t value) {
  while(value>255) { dst[(*op)++]=0; value-=255; }
  dst[(*op)++]=(uint8_t)value;
}
LZO_INLINE void put_literals(const uint8_t *src,size_t start,size_t length,
                         uint8_t *dst,size_t *op,size_t last_word) {
  if(length==0) return;
  if(last_word!=SIZE_MAX && length<=3) {
    dst[last_word]|=(uint8_t)length;
  } else if(*op==0 && length<=238) {
    dst[(*op)++]=(uint8_t)(17+length);
  } else if(length<=18) {
    dst[(*op)++]=(uint8_t)(length-3);
  } else {
    dst[(*op)++]=0;put_extension(dst,op,length-18);
  }
  copy_disjoint(dst+*op,src+start,length);*op+=length;
}
/* Locate the first differing byte without scalar comparisons on every
 * matched byte. Every word load is guarded by the remaining input length. */
LZO_INLINE size_t matching_bytes(const uint8_t *a,const uint8_t *b,size_t limit) {
  size_t length=0;
#if (defined(__GNUC__) || defined(__clang__)) && defined(__BYTE_ORDER__) && \
    (__BYTE_ORDER__==__ORDER_LITTLE_ENDIAN__ || __BYTE_ORDER__==__ORDER_BIG_ENDIAN__)
#if __BYTE_ORDER__==__ORDER_LITTLE_ENDIAN__
#define LZO_DIFFERING_BYTE(word) ((size_t)__builtin_ctzll(word)/8)
#else
#define LZO_DIFFERING_BYTE(word) ((size_t)__builtin_clzll(word)/8)
#endif
  while(limit-length>=16) {
    uint64_t a0,a1,b0,b1;
    memcpy(&a0,a+length,8);memcpy(&a1,a+length+8,8);
    memcpy(&b0,b+length,8);memcpy(&b1,b+length+8,8);
    uint64_t first=a0^b0,second=a1^b1;
    if(first|second) return length+(first ? LZO_DIFFERING_BYTE(first) : 8+LZO_DIFFERING_BYTE(second));
    length+=16;
  }
  while(limit-length>=8) {
    uint64_t x,y;memcpy(&x,a+length,8);memcpy(&y,b+length,8);
    uint64_t different=x^y;
    if(different) {
      return length+LZO_DIFFERING_BYTE(different);
    }
    length+=8;
  }
#undef LZO_DIFFERING_BYTE
#endif
  while(length<limit && a[length]==b[length]) length++;
  return length;
}
size_t hs_columnar_lzo1x_compress_bound(size_t n) {
  if(n>UINT32_MAX || n>SIZE_MAX-67 || n/16>SIZE_MAX-67-n) return 0;
  return n+n/16+67;
}
int hs_columnar_lzo1x_measure(const uint8_t *src,size_t n,size_t limit,size_t *written) {
  return decode_lzo1x(src,n,NULL,limit,written);
}
int hs_columnar_lzo1x_decompress(const uint8_t *src,size_t n,uint8_t *dst,size_t cap,size_t *written) {
  if(!dst && cap) { if(written) *written=0; return LZO_OUTPUT; }
  return decode_lzo1x(src,n,dst,cap,written);
}
int hs_columnar_lzo1x_compress(const uint8_t *src,size_t n,uint8_t *dst,size_t cap,
                         size_t *written) {
  size_t op=0,start=0,last_word=SIZE_MAX;
  if(!written) return LZO_OUTPUT;
  *written=0;
  size_t bound=hs_columnar_lzo1x_compress_bound(n);
  if(!bound) return LZO_OVERFLOW;
  if(cap<bound) return LZO_OUTPUT;
  /* Literal-only tiny blocks avoid setting up a match dictionary. */
  if(n>32) {
    uint32_t dictionary[8192];
    unsigned entries=n<=4096 ? 1024 : 8192,mask=entries-1;
    memset(dictionary,0,entries*sizeof(*dictionary));
    dictionary[literal_hash(src)&mask]=1;
    size_t pos=1,attempts=32;
    while(pos<=n-8) {
      unsigned hash=literal_hash(src+pos)&mask;uint32_t previous=dictionary[hash];
      dictionary[hash]=(uint32_t)(pos+1);
      if(previous && pos-(previous-1)<=16384 && memcmp(src+previous-1,src+pos,4)==0) {
        size_t from=previous-1,distance=pos-from;
        size_t length=4+matching_bytes(src+from+4,src+pos+4,n-pos-4);
        put_literals(src,start,pos-start,dst,&op,last_word);
        if(length<=33) dst[op++]=(uint8_t)(32+length-2);
        else { dst[op++]=32;put_extension(dst,&op,length-33); }
        unsigned word=(unsigned)((distance-1)<<2);last_word=op;
        dst[op++]=(uint8_t)word;dst[op++]=(uint8_t)(word>>8);
        pos+=length;start=pos;attempts=32;
        if(pos>=2 && pos-2<=n-8) dictionary[literal_hash(src+pos-2)&mask]=(uint32_t)(pos-1);
        if(pos>=1 && pos-1<=n-8) dictionary[literal_hash(src+pos-1)&mask]=(uint32_t)pos;
      } else {
        /* Gradually reduce probes in long incompressible literal runs. The
         * bounded input extent also bounds attempts and this addition. */
        pos+=attempts++>>5;
      }
    }
  }
  put_literals(src,start,n-start,dst,&op,last_word);
  dst[op++]=17;dst[op++]=0;dst[op++]=0;*written=op;return LZO_OK;
}
