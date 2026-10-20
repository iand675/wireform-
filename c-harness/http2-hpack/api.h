/* Kernel API as wireform-http2/src/Network/HTTP2/HPACK/Huffman.hs imports
 * it (wireform-http2/cbits/hpack_huffman.c), plus a bit-by-bit reference
 * codec built from the RFC 7541 Appendix B table that the tests and fuzz
 * targets compare against. */
#ifndef HTTP2_HPACK_API_H
#define HTTP2_HPACK_API_H

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ------------------------------------------------------------------ */
/* Kernel API                                                          */
/* ------------------------------------------------------------------ */

/* wireform_hpack_huffman_encode (c_huffman_encode, huffmanEncode)
 *
 * Huffman-codes src[0..len) and pads the last byte with the high bits of
 * EOS (all ones). Returns the number of bytes written, which is exactly
 * wireform_hpack_huffman_encode_len(src, len).
 * Contract: dst has room for encode_len(src, len) bytes. The longest code
 * is 30 bits, so ceil(30 * len / 8) <= 4 * len + 1 always suffices;
 * huffmanEncode allocates exactly 4 * len + 1 and is never called with
 * len == 0 (it returns BS.empty first). src may be any pointer when
 * len == 0. */
size_t wireform_hpack_huffman_encode(const uint8_t *src, size_t len,
                                     uint8_t *dst);

/* wireform_hpack_huffman_encode_len (c_huffman_encode_len,
 * huffmanEncodeLength)
 *
 * Returns ceil(sum of code lengths / 8): the exact encoded size. Reads
 * src[0..len) only; len == 0 returns 0 without touching src. The bit sum
 * cannot overflow size_t for any len a ByteString can have (30 * len). */
size_t wireform_hpack_huffman_encode_len(const uint8_t *src, size_t len);

/* wireform_hpack_huffman_decode (c_huffman_decode, huffmanDecode)
 *
 * Decodes src[0..src_len) into dst. Returns 0 and sets *out_len to the
 * decoded length on success. Returns -1 if the input is not a valid
 * RFC 7541 Huffman string (EOS decoded anywhere, padding longer than 7
 * bits, padding that is not all ones) or if the output would exceed
 * dst_cap. It never writes dst[dst_cap] or beyond, and always sets
 * *out_len (on failure, to the number of bytes written so far).
 * Contract: dst has dst_cap writable bytes; out_len is valid. The
 * shortest code is 5 bits, so a valid input decodes to at most
 * floor(8 * src_len / 5) bytes; huffmanDecode passes
 * dst_cap = 2 * src_len + 256, so the capacity failure is unreachable from
 * Haskell (and would surface as InvalidHuffmanEncoding if it were). The
 * input comes straight off the wire: every byte string must be handled. */
int wireform_hpack_huffman_decode(const uint8_t *src, size_t src_len,
                                  uint8_t *dst, size_t dst_cap,
                                  size_t *out_len);

/* ------------------------------------------------------------------ */
/* Reference codec                                                     */
/* ------------------------------------------------------------------ */

#define REF_EOS 256

/* RFC 7541 Appendix B, "code as bits aligned to MSB" column, copied from
 * the RFC text. '|' separators are ignored. */
static const char *const ref_huffman_bits[257] = {
    /*   0   0x00 */ "11111111|11000",
    /*   1   0x01 */ "11111111|11111111|1011000",
    /*   2   0x02 */ "11111111|11111111|11111110|0010",
    /*   3   0x03 */ "11111111|11111111|11111110|0011",
    /*   4   0x04 */ "11111111|11111111|11111110|0100",
    /*   5   0x05 */ "11111111|11111111|11111110|0101",
    /*   6   0x06 */ "11111111|11111111|11111110|0110",
    /*   7   0x07 */ "11111111|11111111|11111110|0111",
    /*   8   0x08 */ "11111111|11111111|11111110|1000",
    /*   9   0x09 */ "11111111|11111111|11101010",
    /*  10   0x0a */ "11111111|11111111|11111111|111100",
    /*  11   0x0b */ "11111111|11111111|11111110|1001",
    /*  12   0x0c */ "11111111|11111111|11111110|1010",
    /*  13   0x0d */ "11111111|11111111|11111111|111101",
    /*  14   0x0e */ "11111111|11111111|11111110|1011",
    /*  15   0x0f */ "11111111|11111111|11111110|1100",
    /*  16   0x10 */ "11111111|11111111|11111110|1101",
    /*  17   0x11 */ "11111111|11111111|11111110|1110",
    /*  18   0x12 */ "11111111|11111111|11111110|1111",
    /*  19   0x13 */ "11111111|11111111|11111111|0000",
    /*  20   0x14 */ "11111111|11111111|11111111|0001",
    /*  21   0x15 */ "11111111|11111111|11111111|0010",
    /*  22   0x16 */ "11111111|11111111|11111111|111110",
    /*  23   0x17 */ "11111111|11111111|11111111|0011",
    /*  24   0x18 */ "11111111|11111111|11111111|0100",
    /*  25   0x19 */ "11111111|11111111|11111111|0101",
    /*  26   0x1a */ "11111111|11111111|11111111|0110",
    /*  27   0x1b */ "11111111|11111111|11111111|0111",
    /*  28   0x1c */ "11111111|11111111|11111111|1000",
    /*  29   0x1d */ "11111111|11111111|11111111|1001",
    /*  30   0x1e */ "11111111|11111111|11111111|1010",
    /*  31   0x1f */ "11111111|11111111|11111111|1011",
    /*  32    ' ' */ "010100",
    /*  33    '!' */ "11111110|00",
    /*  34    '"' */ "11111110|01",
    /*  35    '#' */ "11111111|1010",
    /*  36    '$' */ "11111111|11001",
    /*  37    '%' */ "010101",
    /*  38    '&' */ "11111000",
    /*  39   0x27 */ "11111111|010",
    /*  40    '(' */ "11111110|10",
    /*  41    ')' */ "11111110|11",
    /*  42    '*' */ "11111001",
    /*  43    '+' */ "11111111|011",
    /*  44    ',' */ "11111010",
    /*  45    '-' */ "010110",
    /*  46    '.' */ "010111",
    /*  47    '/' */ "011000",
    /*  48    '0' */ "00000",
    /*  49    '1' */ "00001",
    /*  50    '2' */ "00010",
    /*  51    '3' */ "011001",
    /*  52    '4' */ "011010",
    /*  53    '5' */ "011011",
    /*  54    '6' */ "011100",
    /*  55    '7' */ "011101",
    /*  56    '8' */ "011110",
    /*  57    '9' */ "011111",
    /*  58    ':' */ "1011100",
    /*  59    ';' */ "11111011",
    /*  60    '<' */ "11111111|1111100",
    /*  61    '=' */ "100000",
    /*  62    '>' */ "11111111|1011",
    /*  63    '?' */ "11111111|00",
    /*  64    '@' */ "11111111|11010",
    /*  65    'A' */ "100001",
    /*  66    'B' */ "1011101",
    /*  67    'C' */ "1011110",
    /*  68    'D' */ "1011111",
    /*  69    'E' */ "1100000",
    /*  70    'F' */ "1100001",
    /*  71    'G' */ "1100010",
    /*  72    'H' */ "1100011",
    /*  73    'I' */ "1100100",
    /*  74    'J' */ "1100101",
    /*  75    'K' */ "1100110",
    /*  76    'L' */ "1100111",
    /*  77    'M' */ "1101000",
    /*  78    'N' */ "1101001",
    /*  79    'O' */ "1101010",
    /*  80    'P' */ "1101011",
    /*  81    'Q' */ "1101100",
    /*  82    'R' */ "1101101",
    /*  83    'S' */ "1101110",
    /*  84    'T' */ "1101111",
    /*  85    'U' */ "1110000",
    /*  86    'V' */ "1110001",
    /*  87    'W' */ "1110010",
    /*  88    'X' */ "11111100",
    /*  89    'Y' */ "1110011",
    /*  90    'Z' */ "11111101",
    /*  91    '[' */ "11111111|11011",
    /*  92   0x5c */ "11111111|11111110|000",
    /*  93    ']' */ "11111111|11100",
    /*  94    '^' */ "11111111|111100",
    /*  95    '_' */ "100010",
    /*  96    '`' */ "11111111|1111101",
    /*  97    'a' */ "00011",
    /*  98    'b' */ "100011",
    /*  99    'c' */ "00100",
    /* 100    'd' */ "100100",
    /* 101    'e' */ "00101",
    /* 102    'f' */ "100101",
    /* 103    'g' */ "100110",
    /* 104    'h' */ "100111",
    /* 105    'i' */ "00110",
    /* 106    'j' */ "1110100",
    /* 107    'k' */ "1110101",
    /* 108    'l' */ "101000",
    /* 109    'm' */ "101001",
    /* 110    'n' */ "101010",
    /* 111    'o' */ "00111",
    /* 112    'p' */ "101011",
    /* 113    'q' */ "1110110",
    /* 114    'r' */ "101100",
    /* 115    's' */ "01000",
    /* 116    't' */ "01001",
    /* 117    'u' */ "101101",
    /* 118    'v' */ "1110111",
    /* 119    'w' */ "1111000",
    /* 120    'x' */ "1111001",
    /* 121    'y' */ "1111010",
    /* 122    'z' */ "1111011",
    /* 123    '{' */ "11111111|1111110",
    /* 124    '|' */ "11111111|100",
    /* 125    '}' */ "11111111|111101",
    /* 126    '~' */ "11111111|11101",
    /* 127   0x7f */ "11111111|11111111|11111111|1100",
    /* 128   0x80 */ "11111111|11111110|0110",
    /* 129   0x81 */ "11111111|11111111|010010",
    /* 130   0x82 */ "11111111|11111110|0111",
    /* 131   0x83 */ "11111111|11111110|1000",
    /* 132   0x84 */ "11111111|11111111|010011",
    /* 133   0x85 */ "11111111|11111111|010100",
    /* 134   0x86 */ "11111111|11111111|010101",
    /* 135   0x87 */ "11111111|11111111|1011001",
    /* 136   0x88 */ "11111111|11111111|010110",
    /* 137   0x89 */ "11111111|11111111|1011010",
    /* 138   0x8a */ "11111111|11111111|1011011",
    /* 139   0x8b */ "11111111|11111111|1011100",
    /* 140   0x8c */ "11111111|11111111|1011101",
    /* 141   0x8d */ "11111111|11111111|1011110",
    /* 142   0x8e */ "11111111|11111111|11101011",
    /* 143   0x8f */ "11111111|11111111|1011111",
    /* 144   0x90 */ "11111111|11111111|11101100",
    /* 145   0x91 */ "11111111|11111111|11101101",
    /* 146   0x92 */ "11111111|11111111|010111",
    /* 147   0x93 */ "11111111|11111111|1100000",
    /* 148   0x94 */ "11111111|11111111|11101110",
    /* 149   0x95 */ "11111111|11111111|1100001",
    /* 150   0x96 */ "11111111|11111111|1100010",
    /* 151   0x97 */ "11111111|11111111|1100011",
    /* 152   0x98 */ "11111111|11111111|1100100",
    /* 153   0x99 */ "11111111|11111110|11100",
    /* 154   0x9a */ "11111111|11111111|011000",
    /* 155   0x9b */ "11111111|11111111|1100101",
    /* 156   0x9c */ "11111111|11111111|011001",
    /* 157   0x9d */ "11111111|11111111|1100110",
    /* 158   0x9e */ "11111111|11111111|1100111",
    /* 159   0x9f */ "11111111|11111111|11101111",
    /* 160   0xa0 */ "11111111|11111111|011010",
    /* 161   0xa1 */ "11111111|11111110|11101",
    /* 162   0xa2 */ "11111111|11111110|1001",
    /* 163   0xa3 */ "11111111|11111111|011011",
    /* 164   0xa4 */ "11111111|11111111|011100",
    /* 165   0xa5 */ "11111111|11111111|1101000",
    /* 166   0xa6 */ "11111111|11111111|1101001",
    /* 167   0xa7 */ "11111111|11111110|11110",
    /* 168   0xa8 */ "11111111|11111111|1101010",
    /* 169   0xa9 */ "11111111|11111111|011101",
    /* 170   0xaa */ "11111111|11111111|011110",
    /* 171   0xab */ "11111111|11111111|11110000",
    /* 172   0xac */ "11111111|11111110|11111",
    /* 173   0xad */ "11111111|11111111|011111",
    /* 174   0xae */ "11111111|11111111|1101011",
    /* 175   0xaf */ "11111111|11111111|1101100",
    /* 176   0xb0 */ "11111111|11111111|00000",
    /* 177   0xb1 */ "11111111|11111111|00001",
    /* 178   0xb2 */ "11111111|11111111|100000",
    /* 179   0xb3 */ "11111111|11111111|00010",
    /* 180   0xb4 */ "11111111|11111111|1101101",
    /* 181   0xb5 */ "11111111|11111111|100001",
    /* 182   0xb6 */ "11111111|11111111|1101110",
    /* 183   0xb7 */ "11111111|11111111|1101111",
    /* 184   0xb8 */ "11111111|11111110|1010",
    /* 185   0xb9 */ "11111111|11111111|100010",
    /* 186   0xba */ "11111111|11111111|100011",
    /* 187   0xbb */ "11111111|11111111|100100",
    /* 188   0xbc */ "11111111|11111111|1110000",
    /* 189   0xbd */ "11111111|11111111|100101",
    /* 190   0xbe */ "11111111|11111111|100110",
    /* 191   0xbf */ "11111111|11111111|1110001",
    /* 192   0xc0 */ "11111111|11111111|11111000|00",
    /* 193   0xc1 */ "11111111|11111111|11111000|01",
    /* 194   0xc2 */ "11111111|11111110|1011",
    /* 195   0xc3 */ "11111111|11111110|001",
    /* 196   0xc4 */ "11111111|11111111|100111",
    /* 197   0xc5 */ "11111111|11111111|1110010",
    /* 198   0xc6 */ "11111111|11111111|101000",
    /* 199   0xc7 */ "11111111|11111111|11110110|0",
    /* 200   0xc8 */ "11111111|11111111|11111000|10",
    /* 201   0xc9 */ "11111111|11111111|11111000|11",
    /* 202   0xca */ "11111111|11111111|11111001|00",
    /* 203   0xcb */ "11111111|11111111|11111011|110",
    /* 204   0xcc */ "11111111|11111111|11111011|111",
    /* 205   0xcd */ "11111111|11111111|11111001|01",
    /* 206   0xce */ "11111111|11111111|11110001",
    /* 207   0xcf */ "11111111|11111111|11110110|1",
    /* 208   0xd0 */ "11111111|11111110|010",
    /* 209   0xd1 */ "11111111|11111111|00011",
    /* 210   0xd2 */ "11111111|11111111|11111001|10",
    /* 211   0xd3 */ "11111111|11111111|11111100|000",
    /* 212   0xd4 */ "11111111|11111111|11111100|001",
    /* 213   0xd5 */ "11111111|11111111|11111001|11",
    /* 214   0xd6 */ "11111111|11111111|11111100|010",
    /* 215   0xd7 */ "11111111|11111111|11110010",
    /* 216   0xd8 */ "11111111|11111111|00100",
    /* 217   0xd9 */ "11111111|11111111|00101",
    /* 218   0xda */ "11111111|11111111|11111010|00",
    /* 219   0xdb */ "11111111|11111111|11111010|01",
    /* 220   0xdc */ "11111111|11111111|11111111|1101",
    /* 221   0xdd */ "11111111|11111111|11111100|011",
    /* 222   0xde */ "11111111|11111111|11111100|100",
    /* 223   0xdf */ "11111111|11111111|11111100|101",
    /* 224   0xe0 */ "11111111|11111110|1100",
    /* 225   0xe1 */ "11111111|11111111|11110011",
    /* 226   0xe2 */ "11111111|11111110|1101",
    /* 227   0xe3 */ "11111111|11111111|00110",
    /* 228   0xe4 */ "11111111|11111111|101001",
    /* 229   0xe5 */ "11111111|11111111|00111",
    /* 230   0xe6 */ "11111111|11111111|01000",
    /* 231   0xe7 */ "11111111|11111111|1110011",
    /* 232   0xe8 */ "11111111|11111111|101010",
    /* 233   0xe9 */ "11111111|11111111|101011",
    /* 234   0xea */ "11111111|11111111|11110111|0",
    /* 235   0xeb */ "11111111|11111111|11110111|1",
    /* 236   0xec */ "11111111|11111111|11110100",
    /* 237   0xed */ "11111111|11111111|11110101",
    /* 238   0xee */ "11111111|11111111|11111010|10",
    /* 239   0xef */ "11111111|11111111|1110100",
    /* 240   0xf0 */ "11111111|11111111|11111010|11",
    /* 241   0xf1 */ "11111111|11111111|11111100|110",
    /* 242   0xf2 */ "11111111|11111111|11111011|00",
    /* 243   0xf3 */ "11111111|11111111|11111011|01",
    /* 244   0xf4 */ "11111111|11111111|11111100|111",
    /* 245   0xf5 */ "11111111|11111111|11111101|000",
    /* 246   0xf6 */ "11111111|11111111|11111101|001",
    /* 247   0xf7 */ "11111111|11111111|11111101|010",
    /* 248   0xf8 */ "11111111|11111111|11111101|011",
    /* 249   0xf9 */ "11111111|11111111|11111111|1110",
    /* 250   0xfa */ "11111111|11111111|11111101|100",
    /* 251   0xfb */ "11111111|11111111|11111101|101",
    /* 252   0xfc */ "11111111|11111111|11111101|110",
    /* 253   0xfd */ "11111111|11111111|11111101|111",
    /* 254   0xfe */ "11111111|11111111|11111110|000",
    /* 255   0xff */ "11111111|11111111|11111011|10",
    /* 256    EOS */ "11111111|11111111|11111111|111111",
};

typedef struct {
    int16_t child[2]; /* -1: none */
    int16_t sym;      /* -1: internal node */
} ref_node;

static ref_node ref_trie[2 * 257];
static int ref_trie_n;
static uint32_t ref_code[257];
static uint8_t ref_len[257];
static int ref_ready;

static void ref_die(const char *what)
{
    fprintf(stderr, "reference Huffman table: %s\n", what);
    abort();
}

/* Build the decoding trie from the bit strings and check that it is a
 * complete prefix code (every internal node has two children, no code is
 * a prefix of another). Called lazily by every reference function. */
static void ref_init(void)
{
    if (ref_ready) return;
    ref_trie_n = 1;
    ref_trie[0] = (ref_node){ { -1, -1 }, -1 };
    for (int s = 0; s < 257; s++) {
        int node = 0;
        uint32_t code = 0;
        int len = 0;
        for (const char *c = ref_huffman_bits[s]; *c; c++) {
            if (*c == '|') continue;
            if (*c != '0' && *c != '1') ref_die("bad character");
            int b = *c - '0';
            if (ref_trie[node].sym >= 0) ref_die("code has a prefix code");
            if (ref_trie[node].child[b] < 0) {
                if (ref_trie_n >= (int)(sizeof ref_trie / sizeof ref_trie[0]))
                    ref_die("too many nodes");
                ref_trie[ref_trie_n] = (ref_node){ { -1, -1 }, -1 };
                ref_trie[node].child[b] = (int16_t)ref_trie_n++;
            }
            node = ref_trie[node].child[b];
            code = (code << 1) | (uint32_t)b;
            len++;
        }
        if (len < 5 || len > 30) ref_die("bad length");
        if (ref_trie[node].sym >= 0 || ref_trie[node].child[0] >= 0
            || ref_trie[node].child[1] >= 0)
            ref_die("duplicate code or code is a prefix");
        ref_trie[node].sym = (int16_t)s;
        ref_code[s] = code;
        ref_len[s] = (uint8_t)len;
    }
    for (int i = 0; i < ref_trie_n; i++) {
        if (ref_trie[i].sym < 0
            && (ref_trie[i].child[0] < 0 || ref_trie[i].child[1] < 0))
            ref_die("incomplete code");
    }
    ref_ready = 1;
}

/* Exact encoded size. */
static inline size_t ref_encode_len(const uint8_t *src, size_t len)
{
    ref_init();
    size_t bits = 0;
    for (size_t i = 0; i < len; i++) bits += ref_len[src[i]];
    return (bits + 7) / 8;
}

/* Bit-at-a-time encoder; dst needs ref_encode_len(src, len) bytes. */
static inline size_t ref_encode(const uint8_t *src, size_t len, uint8_t *dst)
{
    ref_init();
    size_t nbits = 0;
    for (size_t i = 0; i < len; i++) {
        for (int k = ref_len[src[i]] - 1; k >= 0; k--) {
            size_t byte = nbits / 8;
            if (nbits % 8 == 0) dst[byte] = 0;
            dst[byte] |= (uint8_t)(((ref_code[src[i]] >> k) & 1u) << (7 - nbits % 8));
            nbits++;
        }
    }
    while (nbits % 8 != 0) { /* EOS prefix: ones */
        dst[nbits / 8] |= (uint8_t)(1u << (7 - nbits % 8));
        nbits++;
    }
    return nbits / 8;
}

/* Upper bound on decoded size: the shortest code is 5 bits. */
#define REF_DECODE_MAX(n) ((n) * 8 / 5 + 1)

/* Bit-at-a-time decoder following RFC 7541 section 5.2. dst needs
 * REF_DECODE_MAX(len) bytes. Sets *emitted to the number of symbols
 * decoded before success or the first error (the symbols preceding an
 * EOS, or everything before invalid padding). Returns 0 or -1. */
static inline int ref_decode(const uint8_t *src, size_t len, uint8_t *dst,
                             size_t *emitted)
{
    ref_init();
    size_t out = 0;
    int node = 0;
    int depth = 0;    /* bits of the pending, incomplete code */
    int ones = 1;     /* pending bits are all ones */
    for (size_t i = 0; i < len; i++) {
        for (int k = 7; k >= 0; k--) {
            int b = (src[i] >> k) & 1;
            node = ref_trie[node].child[b];
            depth++;
            ones &= b;
            int s = ref_trie[node].sym;
            if (s == REF_EOS) { *emitted = out; return -1; }
            if (s >= 0) {
                dst[out++] = (uint8_t)s;
                node = 0;
                depth = 0;
                ones = 1;
            }
        }
    }
    *emitted = out;
    if (depth > 7) return -1;  /* padding longer than 7 bits */
    if (!ones) return -1;      /* padding is not a prefix of EOS */
    return 0;
}

#endif
