/* Differential fuzz target for hs_xoshiro256pp_next / _reseed.
 *
 * The input is an op stream run against the kernel with the interposed
 * entropy source (entropy_fake.h), tracking the reference generator:
 *   op 0  seed:   next 32 bytes become the source's seed; reseed
 *   op 1  draw:   next byte + 1 draws, each compared with the reference
 *   op 2  reseed: rewind the source to the current seed and reseed
 *   op 3  faults (Linux): next byte picks EINTR count / short-read chunk /
 *         hard failure, then reseed
 * Seeds the kernel must replace (all-zero, or a failing source) switch the
 * reference off until the next known seed; draws are then only checked
 * for not being stuck. */
#define _GNU_SOURCE
#include "harness.h"
#include "entropy_fake.h"
#include "api.h"

static uint64_t ref[4];
static int tracked;

static void reseed_from_fake(void)
{
    hs_xoshiro256pp_reseed();
    memcpy(ref, fe.bytes, sizeof ref);
    tracked = (ref[0] | ref[1] | ref[2] | ref[3]) != 0;
}

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    wf_input in = { data, size };
    uint64_t seed[4] = { 1, 2, 3, 4 };
    fe_set_seed(seed);
    reseed_from_fake();
    int ops = 0;
    while (in.n && ops++ < 256) {
        uint8_t op = wf_take_u8(&in) % 4;
        switch (op) {
        case 0:
            for (int i = 0; i < 4; i++) {
                seed[i] = (uint64_t)wf_take_u32(&in) | ((uint64_t)wf_take_u32(&in) << 32);
            }
            fe_set_seed(seed);
            reseed_from_fake();
            break;
        case 1: {
            int k = wf_take_u8(&in) + 1;
            uint64_t prev = 0;
            int same = 0;
            for (int i = 0; i < k; i++) {
                uint64_t got = hs_xoshiro256pp_next();
                if (tracked) {
                    WF_FUZZ_ASSERT(got == ref_xoshiro256pp_next(ref));
                } else {
                    same += i > 0 && got == prev;
                }
                prev = got;
            }
            WF_FUZZ_ASSERT(same < 2);
            break;
        }
        case 2:
            fe_set_seed(seed);
            reseed_from_fake();
            break;
        default: {
            uint8_t f = wf_take_u8(&in);
            fe_set_seed(seed);
#if FE_FAULTS
            fe.eintr = f & 3;
            fe.chunk = (f >> 2) & 15;
            switch (f >> 6) {
            case 1: fe.fail_errno = ENOSYS; break;
            case 2: fe.ret_zero = (f & 1); fe.overreport = !(f & 1); break;
            default: break;
            }
            reseed_from_fake();
            if (f >> 6 == 1 || f >> 6 == 2) tracked = 0;
#else
            /* Darwin: arc4random_buf cannot fail; start the stream at a
             * byte offset into the seed instead. */
            fe.pos = f % 32;
            hs_xoshiro256pp_reseed();
            for (int i = 0; i < 32; i++) ((uint8_t *)ref)[i] = fe.bytes[(f % 32 + (size_t)i) % 32];
            tracked = (ref[0] | ref[1] | ref[2] | ref[3]) != 0;
#endif
            break;
        }
        }
    }
    return 0;
}
