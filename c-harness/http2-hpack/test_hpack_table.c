/* Exhaustive check of the tables inside hpack_huffman.c.
 *
 * The kernel source is compiled a second time into this file (with its
 * exported symbols renamed so they do not clash with the linked copy) to
 * reach its static tables:
 *
 *   - huff_encode_table must equal the RFC 7541 Appendix B codes;
 *   - huff_decode_table, the nghttp2 nibble automaton, must be equivalent
 *     to the bit-by-bit RFC decoder on every input. The test explores the
 *     product of (automaton state, reference trie node) reachable from
 *     (0, root) one nibble at a time. Every reachable pair, for every
 *     nibble, must agree on: failure (EOS decoded), the symbol emitted,
 *     and the accept flag (pending bits are at most 7 ones, i.e. a valid
 *     padding). Since the decoder only consults these per nibble, this
 *     covers all byte strings of every length. */
#define wireform_hpack_huffman_encode tbl_hpack_huffman_encode
#define wireform_hpack_huffman_encode_len tbl_hpack_huffman_encode_len
#define wireform_hpack_huffman_decode tbl_hpack_huffman_decode
#include "../../wireform-http2/cbits/hpack_huffman.c"
#undef wireform_hpack_huffman_encode
#undef wireform_hpack_huffman_encode_len
#undef wireform_hpack_huffman_decode

#include "harness.h"
#include "api.h"

static void test_encode_table(void)
{
    ref_init();
    for (int s = 0; s < 257; s++) {
        WF_CHECK_EQ(huff_encode_table[s].code, ref_code[s]);
        WF_CHECK_EQ(huff_encode_table[s].bits, ref_len[s]);
    }
}

/* Reference trie depth and "all ones" status of every node. */
static int node_depth[2 * 257];
static int node_ones[2 * 257];

static void label_nodes(int node, int depth, int ones)
{
    node_depth[node] = depth;
    node_ones[node] = ones;
    for (int b = 0; b < 2; b++) {
        int c = ref_trie[node].child[b];
        if (c >= 0) label_nodes(c, depth + 1, ones && b);
    }
}

static void test_decode_automaton(void)
{
    ref_init();
    label_nodes(0, 0, 1);

    enum { NA = 257, NT = 2 * 257 };
    static uint8_t seen[NA][NT];
    static uint16_t queue[NA * NT][2];
    size_t head = 0, tail = 0;
    memset(seen, 0, sizeof seen);
    seen[0][0] = 1;
    queue[tail][0] = 0;
    queue[tail][1] = 0;
    tail++;

    long pairs = 0, a_states_used = 0;
    uint8_t a_used[NA] = { 0 };

    while (head < tail) {
        int a = queue[head][0];
        int t = queue[head][1];
        head++;
        pairs++;
        if (!a_used[a]) { a_used[a] = 1; a_states_used++; }

        for (int nib = 0; nib < 16; nib++) {
            /* Reference: walk 4 bits from trie node t. */
            int node = t, ref_fail = 0, ref_sym = -1;
            for (int k = 3; k >= 0; k--) {
                node = ref_trie[node].child[(nib >> k) & 1];
                int s = ref_trie[node].sym;
                if (s == REF_EOS) { ref_fail = 1; break; }
                if (s >= 0) {
                    if (ref_sym >= 0) { WF_CHECK(!"two symbols in one nibble"); }
                    ref_sym = s;
                    node = 0;
                }
            }

            const struct decode_entry *e = &huff_decode_table[a][nib];
            int kern_fail = e->fstate == HUFF_FAIL_STATE;
            WF_CHECK_EQ(kern_fail, ref_fail);
            if (ref_fail || kern_fail) continue;

            WF_CHECK_EQ(!!(e->flags & HUFF_SYM), ref_sym >= 0);
            if (ref_sym >= 0 && (e->flags & HUFF_SYM)) WF_CHECK_EQ(e->sym, ref_sym);
            int ref_accept = node_depth[node] <= 7 && node_ones[node];
            WF_CHECK_EQ(!!(e->flags & HUFF_ACCEPTED), ref_accept);
            WF_CHECK(e->fstate < HUFF_FAIL_STATE);

            if (e->fstate < NA && !seen[e->fstate][node]) {
                seen[e->fstate][node] = 1;
                queue[tail][0] = e->fstate;
                queue[tail][1] = (uint16_t)node;
                tail++;
            }
        }
    }

    /* The automaton is a relabelling of the trie: each live state pairs
     * with exactly one trie node (256 internal nodes, 256 states). */
    WF_CHECK_EQ(pairs, 256);
    WF_CHECK_EQ(a_states_used, 256);

    /* The failure row stays failed. */
    for (int nib = 0; nib < 16; nib++)
        WF_CHECK_EQ(huff_decode_table[HUFF_FAIL_STATE][nib].fstate, HUFF_FAIL_STATE);
}

/* The renamed copy behaves like the linked one (keeps it honest that both
 * are the same code). */
static void test_copies_agree(void)
{
    static const uint8_t s[] = "www.example.com";
    uint8_t a[32], b[32];
    size_t na = tbl_hpack_huffman_encode(s, sizeof s - 1, a);
    size_t nb = wireform_hpack_huffman_encode(s, sizeof s - 1, b);
    WF_CHECK_EQ(na, nb);
    WF_CHECK_MEM(a, b, na);
    WF_CHECK_EQ(tbl_hpack_huffman_encode_len(s, sizeof s - 1), na);
    uint8_t d[32];
    size_t dl = 0;
    WF_CHECK_EQ(tbl_hpack_huffman_decode(a, na, d, sizeof d, &dl), 0);
    WF_CHECK_EQ(dl, sizeof s - 1);
}

int main(void)
{
    WF_RUN(test_encode_table);
    WF_RUN(test_decode_automaton);
    WF_RUN(test_copies_agree);
    return wf_finish("test_hpack_table");
}
