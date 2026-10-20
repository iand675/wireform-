/* Unit, boundary and differential tests for wireform-kafka crc32c.c. */
#include "harness.h"
#include "api.h"

/* RFC 3720 appendix B.4 vectors, plus the classic "123456789" check
 * value. The RFC lists the CRC as the bytes stored on the wire (little
 * endian); the values below are those bytes read as a uint32. */
static void rfc3720_vectors(void)
{
    uint8_t b[48];

    memset(b, 0x00, 32);
    WF_CHECK_EQ(crc32c(b, 32), 0x8A9136AAu);
    memset(b, 0xFF, 32);
    WF_CHECK_EQ(crc32c(b, 32), 0x62A8AB43u);
    for (int i = 0; i < 32; i++) b[i] = (uint8_t)i;
    WF_CHECK_EQ(crc32c(b, 32), 0x46DD794Eu);
    for (int i = 0; i < 32; i++) b[i] = (uint8_t)(31 - i);
    WF_CHECK_EQ(crc32c(b, 32), 0x113FDB5Cu);

    static const uint8_t iscsi_read[48] = {
        0x01, 0xc0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x14, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04, 0x00,
        0x00, 0x00, 0x00, 0x14, 0x00, 0x00, 0x00, 0x18, 0x28, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    };
    WF_CHECK_EQ(crc32c(iscsi_read, sizeof iscsi_read), 0xD9963A56u);
    WF_CHECK_EQ(ref_crc32c(iscsi_read, sizeof iscsi_read), 0xD9963A56u);

    WF_CHECK_EQ(crc32c((const uint8_t *)"123456789", 9), 0xE3069283u);
    WF_CHECK_EQ(crc32c((const uint8_t *)"a", 1), 0xC1D04330u);
}

/* init/finalize are the documented constants; the empty message (with a
 * NULL pointer, as an empty ByteString may pass) has CRC 0 and append
 * leaves any register untouched. */
static void empty_and_null(void)
{
    WF_CHECK_EQ(crc32c_init(), 0xFFFFFFFFu);
    WF_CHECK_EQ(crc32c_finalize(0xFFFFFFFFu), 0);
    WF_CHECK_EQ(crc32c_finalize(0), 0xFFFFFFFFu);
    WF_CHECK_EQ(crc32c_finalize(0x12345678u), 0x12345678u ^ 0xFFFFFFFFu);
    WF_CHECK_EQ(crc32c(NULL, 0), 0);
    uint32_t regs[] = { 0, 1, 0x80000000u, 0xFFFFFFFFu, 0xDEADBEEFu };
    for (size_t i = 0; i < sizeof regs / sizeof regs[0]; i++) {
        WF_CHECK_EQ(crc32c_append(regs[i], NULL, 0), regs[i]);
        uint8_t one = 0x5a;
        WF_CHECK_EQ(crc32c_append(regs[i], &one, 0), regs[i]);
    }
    wf_guarded g = wf_guard_tail(0);
    WF_CHECK_EQ(crc32c(g.p, 0), 0);
    wf_guard_free(g);
}

/* Every length across the 8/4/2/1-byte stages and several 8-byte blocks,
 * every start alignment, and every distance of the end from the guard
 * page (gap filled with bytes that would change the CRC if read). Head
 * guard too, in case a kernel ever reads before `data`. */
static void exhaustive_lengths_alignments(void)
{
    enum { MAXN = 96, MAXSKEW = 16, MAXGAP = 16 };
    wf_rng r = wf_rng_from_env();
    uint8_t src[MAXN];
    wf_rng_fill(&r, src, sizeof src);

    wf_guarded g = wf_guard_tail(MAXN + MAXGAP);
    for (size_t gap = 0; gap < MAXGAP; gap++) {
        for (size_t len = 0; len <= MAXN; len++) {
            uint8_t *b = g.p + g.n - len - gap;
            memcpy(b, src, len);
            memset(b + len, 0xA5, gap);
            WF_CHECK_EQ(crc32c(b, len), ref_crc32c(src, len));
            WF_CHECK_EQ(crc32c_append(0x0BADF00Du, b, len),
                        ref_crc32c_append(0x0BADF00Du, src, len));
        }
    }
    wf_guard_free(g);

    wf_guarded h = wf_guard_head(MAXN + MAXSKEW);
    for (size_t skew = 0; skew < MAXSKEW; skew++) {
        memset(h.p, 0xA5, skew);
        for (size_t len = 0; len <= MAXN; len++) {
            memcpy(h.p + skew, src, len);
            WF_CHECK_EQ(crc32c(h.p + skew, len), ref_crc32c(src, len));
        }
    }
    wf_guard_free(h);
}

/* append is a homomorphism: splitting the message at any point (and into
 * many random pieces) gives the one-shot CRC. */
static void incremental_splits(void)
{
    enum { N = 77 };
    wf_rng r = wf_rng_from_env();
    uint8_t msg[N];
    wf_rng_fill(&r, msg, sizeof msg);
    uint32_t want = ref_crc32c(msg, N);
    for (size_t cut = 0; cut <= N; cut++) {
        uint8_t *a = wf_dup(msg, cut);
        uint8_t *b = wf_dup(msg + cut, N - cut);
        uint32_t c = crc32c_append(crc32c_init(), a, cut);
        c = crc32c_append(c, b, N - cut);
        WF_CHECK_EQ(crc32c_finalize(c), want);
        wf_free(a, cut);
        wf_free(b, N - cut);
    }
    long iters = wf_iters(2000);
    for (long it = 0; it < iters; it++) {
        size_t n = (size_t)wf_rng_below(&r, 600);
        uint8_t *buf = malloc(n ? n : 1);
        wf_rng_fill(&r, buf, n);
        uint32_t c = crc32c_init();
        size_t off = 0;
        while (off < n) {
            size_t k = 1 + (size_t)wf_rng_below(&r, n - off);
            uint8_t *piece = wf_dup(buf + off, k);
            c = crc32c_append(c, piece, k);
            wf_free(piece, k);
            off += k;
        }
        WF_CHECK_EQ(crc32c_finalize(c), ref_crc32c(buf, n));
        free(buf);
    }
}

/* Random contents and lengths with an arbitrary starting register,
 * biased to all-0x00 / all-0xFF / sign-bit bytes. */
static void random_differential(void)
{
    wf_rng r = wf_rng_from_env();
    static const uint8_t alpha[] = { 0x00, 0xFF, 0x80, 0x7F, 0x01 };
    long iters = wf_iters(5000);
    for (long it = 0; it < iters; it++) {
        size_t n = (size_t)wf_rng_below(&r, 2048);
        uint8_t *tmp = malloc(n ? n : 1);
        if (it & 1) wf_rng_fill(&r, tmp, n);
        else wf_rng_fill_from(&r, tmp, n, alpha, sizeof alpha);
        uint8_t *b = wf_dup(tmp, n);
        uint32_t seed = (uint32_t)wf_rng_next(&r);
        WF_CHECK_EQ(crc32c_append(seed, b, n), ref_crc32c_append(seed, tmp, n));
        WF_CHECK_EQ(crc32c(b, n), ref_crc32c(tmp, n));
        wf_free(b, n);
        free(tmp);
    }
}

/* Kafka's use: the RecordBatch CRC covers attributes..end, and a
 * corrupted byte anywhere must change it. Single-bit flips always change
 * a CRC; check every bit of a 64-byte message. */
static void single_bit_flips_detected(void)
{
    uint8_t m[64];
    for (int i = 0; i < 64; i++) m[i] = (uint8_t)(i * 37 + 11);
    uint32_t base = crc32c(m, sizeof m);
    for (int i = 0; i < 64 * 8; i++) {
        m[i / 8] ^= (uint8_t)(1u << (i % 8));
        WF_CHECK(crc32c(m, sizeof m) != base);
        m[i / 8] ^= (uint8_t)(1u << (i % 8));
    }
}

/* A large buffer (several MiB, page-crossing) against the reference. */
static void large_buffer(void)
{
    size_t n = (size_t)(3u << 20) + 13;
    wf_rng r = wf_rng_from_env();
    uint8_t *tmp = malloc(n);
    wf_rng_fill(&r, tmp, n);
    uint8_t *b = wf_dup(tmp, n);
    WF_CHECK_EQ(crc32c(b, n), ref_crc32c(tmp, n));
    WF_CHECK_EQ(crc32c(b + 1, n - 1), ref_crc32c(tmp + 1, n - 1));
    wf_free(b, n);
    free(tmp);
}

int main(void)
{
    WF_RUN(rfc3720_vectors);
    WF_RUN(empty_and_null);
    WF_RUN(exhaustive_lengths_alignments);
    WF_RUN(incremental_splits);
    WF_RUN(random_differential);
    WF_RUN(single_bit_flips_detected);
    WF_RUN(large_buffer);
    return wf_finish("test_crc32c");
}
