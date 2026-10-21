/* Model-based fuzz target for the magic ring.
 *
 * The input is a little program over one ring and a plain `size`-byte
 * model array:
 *
 *   0 create  [class u8][extra u32]  destroy any live ring, then create one;
 *                                    a class picks a small size (up to 8
 *                                    pages, `extra` bytes) or a huge request
 *                                    that must fail cleanly
 *   1 write   [off u32][len u16][seed u8]  write `len` bytes at base + off,
 *                                    anywhere in [0, 2 * size)
 *   2 move    [dst u32][src u32][len u16]  memmove within the 2N view
 *   3 check   [off u32][len u16]     compare a window with the model
 *   4 destroy                        destroy (twice: must be a no-op)
 *
 * Invariants: base[i] == model[i % size] for every i in [0, 2 * size),
 * sizes match the reference rounding, failed creates leave *out untouched
 * and set ENOMEM, and no descriptor leaks across the whole input.
 */
#include "harness.h"
#include "api.h"

#if defined(WF_FI)
#  include "fault_inject_impl.h"
#endif

static uint16_t take_u16(wf_input *in)
{
    uint16_t lo = wf_take_u8(in);
    return (uint16_t)(lo | (uint16_t)wf_take_u8(in) << 8);
}

/* Clip [off, off + len) to the 2N view. */
static size_t clip(size_t off, size_t len, size_t view)
{
    return len > view - off ? view - off : len;
}

static void check_window(const struct hs_ring *r, const uint8_t *model,
                         size_t off, size_t len)
{
    const ring_byte *b = r->base;
    for (size_t i = 0; i < len; i++) {
        size_t j = off + i;
        WF_FUZZ_ASSERT(b[j] == model[j % r->size]);
    }
}

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size > 4096) return 0;
    int fds_at_start = ref_open_fds();
    size_t ps = (size_t)hs_page_size();
    wf_input in = { data, size };
    struct hs_ring r = { NULL, 0 };
    uint8_t *model = NULL;
    uint8_t *tmp = NULL;
    int steps = 0;

    while (in.n && steps++ < 64) {
        uint8_t op = wf_take_u8(&in) % 5;
        if (op == 0) {
            uint8_t cls = wf_take_u8(&in);
            uint32_t extra = wf_take_u32(&in);
            if (r.base) {
                hs_ring_destroy(&r);
                free(model);
                free(tmp);
                model = tmp = NULL;
            }
            size_t req;
            int huge = 0;
            switch (cls % 8) {
            case 0: req = SIZE_MAX - (extra % 8192); huge = 1; break;
            case 1: req = REF_RING_MAX + 1 + extra; huge = 1; break;
            case 2: req = (size_t)(intptr_t)(int32_t)(extra | 0x80000000u); huge = 1; break;
            case 3: req = extra % 4; break;
            default: req = extra % (8 * ps + 1); break;
            }
            struct hs_ring out = { (void *)0x1234, 77 };
            errno = 0;
            int rc = hs_ring_create(req, &out);
            if (huge) {
                WF_FUZZ_ASSERT(rc == -1);
                WF_FUZZ_ASSERT(errno == ENOMEM);
                WF_FUZZ_ASSERT(out.base == (void *)0x1234 && out.size == 77);
                continue;
            }
            WF_FUZZ_ASSERT(rc == 0);
            WF_FUZZ_ASSERT(out.size == ref_ring_size(req, ps));
            WF_FUZZ_ASSERT((uintptr_t)out.base % ps == 0);
            r = out;
            model = calloc(r.size, 1);
            tmp = malloc(r.size);
            if (!model || !tmp) abort();
            /* Zeroed: both ends of each copy (the unit test checks every
             * byte; a full scan per create would dominate the fuzzer). */
            check_window(&r, model, 0, 256);
            check_window(&r, model, r.size - 256, 512);
            check_window(&r, model, 2 * r.size - 256, 256);
        } else if (op == 4) {
            if (r.base) {
                void *base = r.base;
                size_t sz = r.size;
                hs_ring_destroy(&r);
                WF_FUZZ_ASSERT(r.base == NULL && r.size == 0);
                WF_FUZZ_ASSERT(ref_is_mapped(base, 2 * sz) <= 0);
                free(model);
                free(tmp);
                model = tmp = NULL;
            }
            hs_ring_destroy(&r);
            WF_FUZZ_ASSERT(r.base == NULL);
        } else if (!r.base) {
            continue; /* needs a live ring; operands are consumed as ops */
        } else if (op == 1) {
            size_t view = 2 * r.size;
            size_t off = wf_take_u32(&in) % view;
            size_t len = take_u16(&in) % (r.size + 1);
            uint8_t seed = wf_take_u8(&in);
            len = clip(off, len, view);
            ring_byte *b = r.base;
            for (size_t i = 0; i < len; i++) {
                uint8_t v = (uint8_t)(seed + i * 31 + (i >> 7));
                b[off + i] = v;
                model[(off + i) % r.size] = v;
            }
            check_window(&r, model, off >= r.size ? off - r.size : off, len);
        } else if (op == 2) {
            size_t view = 2 * r.size;
            size_t dst = wf_take_u32(&in) % view;
            size_t src = wf_take_u32(&in) % view;
            size_t len = take_u16(&in) % (r.size + 1);
            len = clip(dst, clip(src, len, view), view);
            ring_byte *b = r.base;
            /* Snapshot the source window first: src and dst may alias
             * through the mirror, not just through virtual overlap. */
            for (size_t i = 0; i < len; i++) tmp[i] = b[src + i];
            ring_copy_in(b + dst, tmp, len);
            for (size_t i = 0; i < len; i++) model[(dst + i) % r.size] = tmp[i];
            check_window(&r, model, dst >= r.size ? dst - r.size : dst, len);
        } else {
            size_t view = 2 * r.size;
            size_t off = wf_take_u32(&in) % view;
            size_t len = clip(off, take_u16(&in), view);
            check_window(&r, model, off, len);
        }
    }
    if (r.base) {
        hs_ring_destroy(&r);
        free(model);
        free(tmp);
    }
    WF_FUZZ_ASSERT(ref_fds_unchanged(fds_at_start));
    return 0;
}
