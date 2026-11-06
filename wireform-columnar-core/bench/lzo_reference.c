/* Benchmark-only calls into the independent system liblzo2. No LZO source
 * is included and production libraries never link this reference backend. */
#include <stddef.h>
#include <stdint.h>
#include <lzo/lzo1x.h>
_Static_assert(sizeof(lzo_uint)==sizeof(size_t),"reference byte count width");
size_t hs_bench_lzo_work_size(void) {return lzo_init()==LZO_E_OK?LZO1X_1_MEM_COMPRESS:0;}
int hs_bench_lzo_compress(const uint8_t *src,size_t n,uint8_t *dst,size_t cap,void *work,size_t *written) {
 if(n>SIZE_MAX-67 || n/16>SIZE_MAX-67-n || cap<n+n/16+67)return -100;
 lzo_uint size=cap;int rc=lzo1x_1_compress(src,n,dst,&size,work);*written=size;return rc;
}
int hs_bench_lzo_decompress(const uint8_t *src,size_t n,uint8_t *dst,size_t cap,size_t *written) {
 lzo_uint size=cap;int rc=lzo1x_decompress_safe(src,n,dst,&size,NULL);*written=size;return rc;
}
