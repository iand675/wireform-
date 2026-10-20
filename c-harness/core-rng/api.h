/* Kernel API as wireform-core/src/Wireform/FFI.hs imports it, plus the
 * reference xoshiro256++ the tests compare against.
 *
 * Caller contract:
 *   hs_xoshiro256pp_next()  fastRandomWord64 (foreign import ccall unsafe,
 *       IO Word64). No arguments, no buffers. Each OS thread owns a
 *       256-bit xoshiro256++ state in TLS, seeded lazily on its first draw
 *       from the platform CSPRNG (getrandom(2) through syscall(2) on Linux,
 *       arc4random_buf on Darwin/BSD), copied into the state words in
 *       native byte order. If the CSPRNG fails or yields the all-zero
 *       state, a splitmix64 fallback over (TLS address, pid, monotonic
 *       time) is used. After fork() the child's next draw reseeds
 *       (pthread_atfork). Any thread may call it at any time.
 *   hs_xoshiro256pp_reseed()  not imported by Haskell; discards the calling
 *       thread's state so its next draw reseeds from the CSPRNG.
 *
 * The kernel exports no buffer-filling entry point, so there are no length
 * or alignment preconditions to test.
 */
#ifndef CORE_RNG_API_H
#define CORE_RNG_API_H

#include <stdint.h>

uint64_t hs_xoshiro256pp_next(void);
void hs_xoshiro256pp_reseed(void);

/* Reference: Blackman and Vigna's xoshiro256plusplus.c, verbatim apart
 * from the explicit state argument. */
static inline uint64_t ref_rotl(const uint64_t x, int k)
{
    return (x << k) | (x >> (64 - k));
}

static inline uint64_t ref_xoshiro256pp_next(uint64_t s[4])
{
    const uint64_t result = ref_rotl(s[0] + s[3], 23) + s[0];
    const uint64_t t = s[1] << 17;
    s[2] ^= s[0];
    s[3] ^= s[1];
    s[1] ^= s[2];
    s[0] ^= s[3];
    s[2] ^= t;
    s[3] = ref_rotl(s[3], 45);
    return result;
}

/* First ten outputs of the reference implementation from the state
 * {1, 2, 3, 4} (the vector rand_xoshiro checks against
 * https://prng.di.unimi.it/xoshiro256plusplus.c). */
static const uint64_t ref_xoshiro256pp_vector_1234[10] = {
    41943041ULL, 58720359ULL, 3588806011781223ULL, 3591011842654386ULL,
    9228616714210784205ULL, 9973669472204895162ULL, 14011001112246962877ULL,
    12406186145184390807ULL, 15849039046786891736ULL, 10450023813501588000ULL,
};

#endif
