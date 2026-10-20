/* Kernel API as wireform-kafka/src/Kafka/Protocol/CRC32C.hs imports it,
 * plus a bitwise reference.
 *
 * Caller contract (from the Haskell call sites, all through
 * unsafeUseAsCStringLen on a ByteString):
 *   - crc32c_append(crc, data, len) / crc32c(data, len): `data` points at
 *     `len` readable bytes with no alignment guarantee (ByteString slices
 *     start anywhere). An empty ByteString may hand over data == NULL
 *     with len == 0; the kernel must not touch `data` then.
 *   - `crc` is any 32-bit value: crc32c_init(), or a previous
 *     crc32c_append result, or anything the user passes to
 *     crc32cAppend. The kernel takes and returns the pre-finalised
 *     (inverted) register, so append(append(c, a), b) == append(c, a ++ b).
 *   - The functions are pure and keep no state (the Haskell imports are
 *     pure, unsafePerformIO-free for init/finalize).
 */
#ifndef KAFKA_CRC32C_API_H
#define KAFKA_CRC32C_API_H

#include <stddef.h>
#include <stdint.h>

uint32_t crc32c_init(void);
uint32_t crc32c_append(uint32_t crc, const uint8_t *data, size_t length);
uint32_t crc32c_finalize(uint32_t crc);
uint32_t crc32c(const uint8_t *data, size_t length);

/* Bit-at-a-time CRC-32C (Castagnoli, reflected polynomial 0x82F63B78),
 * operating on the same pre-inverted register as crc32c_append. */
static inline uint32_t ref_crc32c_append(uint32_t crc, const uint8_t *p, size_t n)
{
    for (size_t i = 0; i < n; i++) {
        crc ^= p[i];
        for (int k = 0; k < 8; k++) {
            crc = (crc >> 1) ^ (0x82F63B78u & (0u - (crc & 1u)));
        }
    }
    return crc;
}

static inline uint32_t ref_crc32c(const uint8_t *p, size_t n)
{
    return ref_crc32c_append(0xFFFFFFFFu, p, n) ^ 0xFFFFFFFFu;
}

#endif
