/* Tests for wireform-core/cbits/fast_rng.c: the published xoshiro256++
 * vector, differential runs against the reference from known seeds (via the
 * interposed entropy source), lazy per-thread seeding, the CSPRNG failure
 * fallback, fork reseeding, and thread isolation. */
#define _GNU_SOURCE
#include "harness.h"
#include "entropy_fake.h"
#include "api.h"

#include <pthread.h>
#include <sys/wait.h>

/* Seed the calling thread's kernel state with s (via the fake source). */
static void kernel_seed(const uint64_t s[4])
{
    fe_set_seed(s);
    hs_xoshiro256pp_reseed();
}

static int check_stream(const uint64_t seed[4], long n)
{
    uint64_t ref[4];
    memcpy(ref, seed, sizeof ref);
    int ok = 1;
    for (long i = 0; i < n; i++) {
        uint64_t want = ref_xoshiro256pp_next(ref);
        uint64_t got = hs_xoshiro256pp_next();
        if (got != want) {
            ok = 0;
            WF_CHECK_EQ(got, want);
            break;
        }
    }
    return ok;
}

static void published_vector(void)
{
    static const uint64_t s[4] = { 1, 2, 3, 4 };
    uint64_t ref[4] = { 1, 2, 3, 4 };
    for (int i = 0; i < 10; i++) {
        WF_CHECK(ref_xoshiro256pp_next(ref) == ref_xoshiro256pp_vector_1234[i]);
    }
    int before = fe.calls;
    kernel_seed(s);
    for (int i = 0; i < 10; i++) {
        WF_CHECK(hs_xoshiro256pp_next() == ref_xoshiro256pp_vector_1234[i]);
    }
    WF_CHECK_EQ(fe.calls - before, 1);
}

/* Random seeds, long streams; the source is consulted once per reseed. */
static void random_seeds_differential(void)
{
    wf_rng r = wf_rng_from_env();
    long seeds = wf_iters(300);
    for (long k = 0; k < seeds; k++) {
        uint64_t s[4];
        for (int i = 0; i < 4; i++) s[i] = wf_rng_next(&r);
        /* Sparse and extreme seeds too. */
        if (k % 7 == 1) { s[0] = 0; s[1] = 0; s[2] = 0; }
        if (k % 7 == 2) { s[0] = s[1] = s[2] = s[3] = UINT64_MAX; }
        if (k % 7 == 3) { s[1] = s[2] = s[3] = 0; s[0] = 1ULL << (k % 64); }
        int before = fe.calls;
        kernel_seed(s);
        check_stream(s, 2000);
        WF_CHECK_EQ(fe.calls - before, 1);
    }
}

/* Reseed without a new seed: the next draw asks the source again and the
 * stream restarts from what it returns. */
static void reseed_consults_source(void)
{
    static const uint64_t s[4] = { 0x0123456789abcdefULL, 2, 3, 0xfedcba9876543210ULL };
    kernel_seed(s);
    check_stream(s, 50);
    fe.pos = 0;
    int before = fe.calls;
    hs_xoshiro256pp_reseed();
    hs_xoshiro256pp_reseed();
    WF_CHECK_EQ(fe.calls, before);   /* lazy: nothing until the next draw */
    check_stream(s, 50);
    WF_CHECK_EQ(fe.calls - before, 1);
}

/* An all-zero seed is xoshiro's fixed point: the kernel must replace it,
 * and the replacement must actually generate. */
static void check_fallback_stream(void)
{
    uint64_t prev = hs_xoshiro256pp_next(), zero = prev == 0;
    int distinct = 0;
    for (int i = 0; i < 1000; i++) {
        uint64_t v = hs_xoshiro256pp_next();
        zero += v == 0;
        distinct += v != prev;
        prev = v;
    }
    WF_CHECK(zero <= 1);
    WF_CHECK(distinct >= 999);
}

static void zero_seed_fallback(void)
{
    static const uint64_t z[4] = { 0, 0, 0, 0 };
    int before = fe.calls;
    kernel_seed(z);
    check_fallback_stream();
    WF_CHECK_EQ(fe.calls - before, 1);
    /* A seed with one nonzero word is used as is. */
    static const uint64_t one[4] = { 0, 0, 0, 1 };
    kernel_seed(one);
    check_stream(one, 100);
}

/* Linux: getrandom fault handling. */
static void linux_source_faults(void)
{
#if FE_FAULTS
    static const uint64_t s[4] = { 11, 22, 33, 44 };
    int before;

    /* EINTR is retried, not treated as failure (regression). */
    kernel_seed(s);
    fe.eintr = 3;
    before = fe.calls;
    check_stream(s, 100);
    WF_CHECK_EQ(fe.calls - before, 4);

    /* Short reads are continued (regression). */
    kernel_seed(s);
    fe.chunk = 5;
    before = fe.calls;
    check_stream(s, 100);
    WF_CHECK_EQ(fe.calls - before, 7);
    kernel_seed(s);
    fe.chunk = 1;
    fe.eintr = 2;
    before = fe.calls;
    check_stream(s, 100);
    WF_CHECK_EQ(fe.calls - before, 34);

    /* Hard failures, a zero return and an impossible count fall back. */
    static const int errs[] = { ENOSYS, EPERM, EFAULT, EAGAIN };
    for (size_t k = 0; k < sizeof errs / sizeof errs[0]; k++) {
        kernel_seed(s);
        fe.fail_errno = errs[k];
        before = fe.calls;
        uint64_t ref[4];
        memcpy(ref, s, sizeof ref);
        WF_CHECK(hs_xoshiro256pp_next() != ref_xoshiro256pp_next(ref));
        check_fallback_stream();
        WF_CHECK_EQ(fe.calls - before, 1);
    }
    kernel_seed(s);
    fe.ret_zero = 1;
    before = fe.calls;
    check_fallback_stream();
    WF_CHECK_EQ(fe.calls - before, 1);
    kernel_seed(s);
    fe.overreport = 1;
    before = fe.calls;
    check_fallback_stream();
    WF_CHECK_EQ(fe.calls - before, 1);
#endif
}

/* The real CSPRNG path (fake source off). */
static void real_source(void)
{
    fe.mode = 0;
    hs_xoshiro256pp_reseed();
    enum { N = 1 << 16 };
    static uint64_t v[N];
    long bits[64] = { 0 };
    for (int i = 0; i < N; i++) {
        v[i] = hs_xoshiro256pp_next();
        for (int b = 0; b < 64; b++) bits[b] += (long)((v[i] >> b) & 1);
    }
    for (int b = 0; b < 64; b++) {
        /* 6.5 sigma around N/2 (sigma = 128). */
        WF_CHECK(bits[b] > N / 2 - 832 && bits[b] < N / 2 + 832);
    }
    /* No repeats in the first 4096 draws (collision odds ~2^-41). */
    for (int i = 0; i < 4096; i++) {
        for (int j = i + 1; j < 4096; j++) {
            if (v[i] == v[j]) WF_CHECK(v[i] != v[j]);
        }
    }
    /* Two real reseeds give different streams. */
    hs_xoshiro256pp_reseed();
    uint64_t a = hs_xoshiro256pp_next();
    hs_xoshiro256pp_reseed();
    uint64_t b = hs_xoshiro256pp_next();
    WF_CHECK(a != b);
}

/* ---- threads ------------------------------------------------------- */

enum { NTHREADS = 8, THREAD_DRAWS = 20000 };

typedef struct {
    int idx;
    int fake;
    int ok;
    int calls;
    uint64_t first[4];
} thread_arg;

static void *thread_main(void *p)
{
    thread_arg *a = p;
    if (a->fake) {
        /* Seed this thread's state before its first draw: the lazy seed
         * on first use must pick it up without an explicit reseed. */
        uint64_t s[4] = { (uint64_t)a->idx + 1, 0x9E3779B97F4A7C15ULL * (uint64_t)(a->idx + 1),
                          (uint64_t)a->idx << 40, ~(uint64_t)a->idx };
        fe_set_seed(s);
        uint64_t ref[4];
        memcpy(ref, s, sizeof ref);
        a->ok = 1;
        for (int i = 0; i < THREAD_DRAWS; i++) {
            uint64_t got = hs_xoshiro256pp_next();
            if (got != ref_xoshiro256pp_next(ref)) { a->ok = 0; break; }
            if (i < 4) a->first[i] = got;
        }
    } else {
        a->ok = 1;
        for (int i = 0; i < 4; i++) a->first[i] = hs_xoshiro256pp_next();
        for (int i = 0; i < THREAD_DRAWS; i++) (void)hs_xoshiro256pp_next();
    }
    a->calls = fe.calls;
    return NULL;
}

static void run_threads(int fake)
{
    pthread_t t[NTHREADS];
    thread_arg a[NTHREADS];
    for (int i = 0; i < NTHREADS; i++) {
        memset(&a[i], 0, sizeof a[i]);
        a[i].idx = i;
        a[i].fake = fake;
        WF_CHECK_EQ(pthread_create(&t[i], NULL, thread_main, &a[i]), 0);
    }
    for (int i = 0; i < NTHREADS; i++) WF_CHECK_EQ(pthread_join(t[i], NULL), 0);
    for (int i = 0; i < NTHREADS; i++) {
        WF_CHECK(a[i].ok);
        WF_CHECK_EQ(a[i].calls, 1);   /* each thread seeds exactly once */
        for (int j = 0; j < i; j++) WF_CHECK(memcmp(a[i].first, a[j].first, sizeof a[i].first) != 0);
    }
}

static void threads_independent(void)
{
    /* Draws on this thread are unaffected by the others. */
    static const uint64_t s[4] = { 5, 6, 7, 8 };
    kernel_seed(s);
    uint64_t ref[4] = { 5, 6, 7, 8 };
    for (int i = 0; i < 10; i++) WF_CHECK(hs_xoshiro256pp_next() == ref_xoshiro256pp_next(ref));
    run_threads(1);
    run_threads(0);
    for (int i = 0; i < 10; i++) WF_CHECK(hs_xoshiro256pp_next() == ref_xoshiro256pp_next(ref));
}

/* ---- fork ---------------------------------------------------------- */

/* Run `child` in a forked child; it returns up to 8 words through a pipe. */
static int in_child(void (*child)(uint64_t out[8]), uint64_t out[8])
{
    int fd[2];
    if (pipe(fd) != 0) return 0;
    fflush(NULL);
    pid_t pid = fork();
    if (pid < 0) return 0;
    if (pid == 0) {
        uint64_t res[8] = { 0 };
        close(fd[0]);
        child(res);
        ssize_t w = write(fd[1], res, sizeof res);
        close(fd[1]);
        exit(w == (ssize_t)sizeof res ? 0 : 1);
    }
    close(fd[1]);
    size_t got = 0;
    while (got < 64) {
        ssize_t n = read(fd[0], (char *)out + got, 64 - got);
        if (n <= 0) break;
        got += (size_t)n;
    }
    close(fd[0]);
    int status = 0;
    waitpid(pid, &status, 0);
    return got == 64 && WIFEXITED(status) && WEXITSTATUS(status) == 0;
}

static const uint64_t child_seed[4] = { 101, 202, 303, 404 };

static void child_fake_reseed(uint64_t out[8])
{
    /* Change the fake seed but do not call reseed: only the atfork handler
     * can make the next draw consult the source. */
    int before = fe.calls;
    fe_set_seed(child_seed);
    for (int i = 0; i < 4; i++) out[i] = hs_xoshiro256pp_next();
    out[4] = (uint64_t)(fe.calls - before);
}

static void child_draw(uint64_t out[8])
{
    int before = fe.calls;
    for (int i = 0; i < 4; i++) out[i] = hs_xoshiro256pp_next();
    out[4] = (uint64_t)(fe.calls - before);
}

static void fork_child_reseeds(void)
{
    static const uint64_t s[4] = { 9, 8, 7, 6 };
    kernel_seed(s);
    for (int i = 0; i < 3; i++) (void)hs_xoshiro256pp_next();
    uint64_t out[8];
    WF_CHECK(in_child(child_fake_reseed, out));
    uint64_t ref[4];
    memcpy(ref, child_seed, sizeof ref);
    for (int i = 0; i < 4; i++) WF_CHECK(out[i] == ref_xoshiro256pp_next(ref));
    WF_CHECK_EQ(out[4], 1);

    /* Real source: the child's draws are not the parent's next draws. */
    fe.mode = 0;
    hs_xoshiro256pp_reseed();
    (void)hs_xoshiro256pp_next();
    WF_CHECK(in_child(child_draw, out));
    WF_CHECK_EQ(out[4], 1);
    uint64_t p[4];
    for (int i = 0; i < 4; i++) p[i] = hs_xoshiro256pp_next();
    WF_CHECK(memcmp(out, p, sizeof p) != 0);
}

/* Regression: with the CSPRNG unusable the fallback seed used to be just
 * the TLS address, which a forked child shares, so a child replayed the
 * parent's stream from its first draw. */
static void fork_fallback_distinct(void)
{
    static const uint64_t z[4] = { 0, 0, 0, 0 };
    for (int round = 0; round < 3; round++) {
        kernel_seed(z);   /* forces the fallback in parent and child */
        uint64_t first[4], next[4], out[8];
        for (int i = 0; i < 4; i++) first[i] = hs_xoshiro256pp_next();
        WF_CHECK(in_child(child_draw, out));
        WF_CHECK_EQ(out[4], 1);
        for (int i = 0; i < 4; i++) next[i] = hs_xoshiro256pp_next();
        WF_CHECK(memcmp(out, first, sizeof first) != 0);
        WF_CHECK(memcmp(out, next, sizeof next) != 0);
    }
}

int main(void)
{
    WF_RUN(published_vector);
    WF_RUN(random_seeds_differential);
    WF_RUN(reseed_consults_source);
    WF_RUN(zero_seed_fallback);
    WF_RUN(linux_source_faults);
    WF_RUN(threads_independent);
    WF_RUN(fork_child_reseeds);
    WF_RUN(fork_fallback_distinct);
    WF_RUN(real_source);
    return wf_finish("test_rng");
}
