/* Differential fuzz target over whole HTTP/1.1 heads: the kernel-driven
 * head model (framing, line scan, request line, header block with and
 * without in-place name lowercasing) against the byte-at-a-time RFC
 * reference in api.h; see head_diff.h. The input is the raw head. */
#include "harness.h"
#include "api.h"
#include "head_diff.h"

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size > (1u << 18)) return 0;
    uint8_t *buf = wf_dup(data, size);
    const char *err = head_diff(buf, (int)size, NULL);
    if (err) fprintf(stderr, "head_diff: %s\n", err);
    WF_FUZZ_ASSERT(err == NULL);
    wf_free(buf, size);
    return 0;
}
