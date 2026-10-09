/*
 * xoshiro256++ — a fast, non-cryptographic 64-bit PRNG.
 *
 * Reference:
 *   D. Blackman and S. Vigna, "Scrambled Linear Pseudorandom
 *   Number Generators", ACM Trans. on Math. Software 47:4
 *   (2021).  https://vigna.di.unimi.it/ftp/papers/ScrambledLinear.pdf
 *
 * One 256-bit state per pthread, stored in '__thread' storage so
 * concurrent generation across many Haskell capabilities never
 * touches a shared cache line.  Seeded on first use from the
 * platform CSPRNG and reseeded in a forked child (see
 * pthread_atfork below).
 *
 * Used by 'wireform-websocket' to roll per-frame masking keys
 * without going through the global 'splitmix' MVar.  Exposed
 * generically because the same primitive is what any
 * non-cryptographic random need on the hot path should reach
 * for.
 */

#include <stdint.h>
#include <stddef.h>
#include <string.h>

#if defined(_WIN32) || defined(_WIN64)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <bcrypt.h>
static int kernel_random(void *buf, size_t len)
{
    NTSTATUS status = BCryptGenRandom(
        NULL, (PUCHAR)buf, (ULONG)len, BCRYPT_USE_SYSTEM_PREFERRED_RNG);
    return BCRYPT_SUCCESS(status) ? 0 : -1;
}
#elif defined(__linux__)
#include <unistd.h>
#include <pthread.h>
#include <sys/syscall.h>
#include <linux/random.h>
static int kernel_random(void *buf, size_t len)
{
    long r = syscall(SYS_getrandom, buf, len, 0);
    return r == (long)len ? 0 : -1;
}
#elif defined(__APPLE__) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__)
#include <unistd.h>
#include <pthread.h>
#include <stdlib.h>
static int kernel_random(void *buf, size_t len)
{
    arc4random_buf(buf, len);
    return 0;
}
#else
/* Fall back to /dev/urandom for portability. */
#include <unistd.h>
#include <pthread.h>
#include <fcntl.h>
static int kernel_random(void *buf, size_t len)
{
    int fd = open("/dev/urandom", O_RDONLY);
    if (fd < 0) return -1;
    size_t off = 0;
    while (off < len) {
        ssize_t n = read(fd, (char *)buf + off, len - off);
        if (n <= 0) { close(fd); return -1; }
        off += (size_t)n;
    }
    close(fd);
    return 0;
}
#endif

/* TLS access model for the per-thread state below.
 *
 * A file-local 'static __thread' defaults to the local-exec model: a
 * single FS/GS-relative MOV, the fastest TLS access (what the comment
 * below describes).  That is only valid when the object lands in the
 * main executable's static TLS block.  When the object is built
 * position-independent for a shared library — GHC's dynamic
 * ('.dyn_o' -> '.so') way, used whenever wireform-core sits in a
 * Template-Haskell dependency closure — local-exec / local-dynamic
 * emit 'R_X86_64_TPOFF32' / 'R_X86_64_DTPOFF32' relocations that ld
 * cannot link into the '.so' on x86_64-linux ("relocation truncated
 * to fit").
 *
 * '__PIC__' is defined by GCC / Clang exactly when compiling
 * position-independent (the '.dyn_o' way) and undefined for the plain
 * static '.o'.  Keep the fast local-exec model for the static object;
 * only step down to initial-exec ('R_X86_64_GOTTPOFF', a single GOT
 * indirection that links into a '.so') when actually building PIC.
 * initial-exec keeps the symbol file-local (unlike global-dynamic,
 * which would require an exported symbol) and its few words of TLS sit
 * within glibc's static-TLS surplus when the library is dlopen'd. */
#if defined(__PIC__)
#define WF_TLS_MODEL __attribute__((tls_model("initial-exec")))
#else
#define WF_TLS_MODEL
#endif

/* Per-thread state.  256-bit (4 × uint64_t).  '__thread' is the
 * GCC / Clang extension; on every modern Unix it lands in the
 * TLS block one offset away from FS / GS register, so the
 * dispatch is a single MOV (local-exec; see WF_TLS_MODEL above for
 * the position-independent / shared-object case). */
static __thread uint64_t xoshiro_state[4] WF_TLS_MODEL;
static __thread int      xoshiro_seeded WF_TLS_MODEL = 0;

static inline uint64_t rotl64(uint64_t x, int k)
{
    return (x << k) | (x >> (64 - k));
}

/* Recover from a degenerate all-zero seed (xoshiro256++ has a
 * fixed point at 0).  Vigna's reference splitmix64 is the
 * recommended way to expand a single 64-bit input into a 256-bit
 * xoshiro seed.  We only invoke this when 'kernel_random'
 * returns all-zero, which on a working kernel never happens. */
static inline uint64_t splitmix64_step(uint64_t *x)
{
    *x += 0x9E3779B97F4A7C15ULL;
    uint64_t z = *x;
    z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ULL;
    z = (z ^ (z >> 27)) * 0x94D049BB133111EBULL;
    return z ^ (z >> 31);
}

static void xoshiro_seed(void)
{
    if (kernel_random(xoshiro_state, sizeof(xoshiro_state)) != 0
        || (xoshiro_state[0] | xoshiro_state[1]
          | xoshiro_state[2] | xoshiro_state[3]) == 0)
    {
        /* Last-resort fallback: tid + nanotime through splitmix. */
        uint64_t fallback = (uint64_t)(uintptr_t)&xoshiro_state;
        for (int i = 0; i < 4; i++) {
            xoshiro_state[i] = splitmix64_step(&fallback);
        }
    }
    xoshiro_seeded = 1;
}

/* The xoshiro256++ next() generator.  Pure register work after
 * the (cached) TLS load. */
uint64_t hs_xoshiro256pp_next(void)
{
    if (!xoshiro_seeded) xoshiro_seed();

    const uint64_t *s = xoshiro_state;
    const uint64_t result = rotl64(s[0] + s[3], 23) + s[0];
    const uint64_t t = xoshiro_state[1] << 17;

    xoshiro_state[2] ^= xoshiro_state[0];
    xoshiro_state[3] ^= xoshiro_state[1];
    xoshiro_state[1] ^= xoshiro_state[2];
    xoshiro_state[0] ^= xoshiro_state[3];
    xoshiro_state[2] ^= t;
    xoshiro_state[3] = rotl64(xoshiro_state[3], 45);

    return result;
}

/* Force reseed.  Useful after fork() and for tests that want
 * deterministic state — they can call this then immediately
 * overwrite the seed via hs_xoshiro256pp_reseed. */
void hs_xoshiro256pp_reseed(void)
{
    xoshiro_seeded = 0;
}

/* Fork safety.  A forked child inherits the parent's TLS, so without
 * this the child would replay the parent's stream: two processes
 * emitting identical WebSocket masking keys.  The child handler runs
 * in the forking thread of the child (the only thread it has) and
 * forces a fresh kernel seed on its next draw. */
#if !defined(_WIN32) && !defined(_WIN64)
static void xoshiro_atfork_child(void)
{
    xoshiro_seeded = 0;
}

__attribute__((constructor))
static void xoshiro_register_atfork(void)
{
    pthread_atfork(NULL, NULL, xoshiro_atfork_child);
}
#endif
