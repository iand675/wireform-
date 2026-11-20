#ifndef COLUMNAR_BSS_API_H
#define COLUMNAR_BSS_API_H
#include <stddef.h>
#include <stdint.h>
/* Buffers are disjoint and exactly width*n bytes, n <= SIZE_MAX/width.
   All alignments are permitted. NULL is valid only for zero values.
   Array sources contain off prefix bytes followed by width*n data bytes. */
void hs_columnar_split_bytes4(const uint8_t *,uint8_t *,size_t);
void hs_columnar_join_bytes4(const uint8_t *,uint8_t *,size_t);
void hs_columnar_split_bytes8(const uint8_t *,uint8_t *,size_t);
void hs_columnar_join_bytes8(const uint8_t *,uint8_t *,size_t);
void hs_columnar_split_bytes4_array(const uint8_t *,size_t,uint8_t *,size_t);
void hs_columnar_split_bytes8_array(const uint8_t *,size_t,uint8_t *,size_t);
static inline void ref_split_bytes(const uint8_t *src,uint8_t *dst,size_t width,size_t n){
    for(size_t lane=0;lane<width;lane++) for(size_t row=0;row<n;row++)dst[lane*n+row]=src[width*row+lane];
}
#endif
