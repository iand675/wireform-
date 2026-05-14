# C Bits - Fast CRC32C Implementation

This directory contains C code for performance-critical operations.

## CRC32C (Castagnoli)

The `crc32c.c` and `crc32c.h` files provide a high-performance CRC32C checksum implementation based on the [fast-crc32](https://github.com/corsix/fast-crc32) library by Peter Cawley (MIT License).

### Features

- **Hardware acceleration**: Automatically detects and uses CPU-specific instructions
  - x86/x64: SSE4.2 CRC32C instructions
  - x86/x64: AVX512 + VPCLMULQDQ for vectorized computation
  - ARM/AArch64: Hardware CRC32 instructions (ARMv8.1-A+, Apple Silicon)
- **Software fallback**: Pure C implementation using lookup tables for all architectures
- **Cross-platform**: Works on x86, x64, ARM, RISC-V, and other architectures

### Performance

Hardware-accelerated performance (single core):

**x86/x64:**
- **Intel Ice Lake**: ~31 GB/s (SSE4.2)
- **AMD Milan**: ~32 GB/s (SSE4.2)
- **AMD Genoa**: ~72 GB/s (AVX512 + VPCLMULQDQ)

**ARM/AArch64:**
- **Apple M1/M2/M3**: ~5-10 GB/s (hardware CRC32)
- **ARM Neoverse**: ~5-10 GB/s (hardware CRC32, ARMv8.1-A+)

**Software fallback:** ~1-2 GB/s (all architectures)

### Implementation Details

The implementation includes the fix from commit [`de1abf28`](https://github.com/corsix/fast-crc32/commit/de1abf28af53fe195026e439a103b29e99a4c40f) which properly zero-extends vectors in the AVX512 code path using `_mm512_zextsi128_si512` instead of `_mm512_castsi128_si512`. This ensures correct CRC calculations on AVX512-capable processors.

### API

```c
// Initialize a CRC computation
uint32_t crc32c_init(void);

// Append data to an ongoing CRC computation
uint32_t crc32c_append(uint32_t crc, const uint8_t* data, size_t length);

// Finalize a CRC computation
uint32_t crc32c_finalize(uint32_t crc);

// One-shot CRC computation
uint32_t crc32c(const uint8_t* data, size_t length);
```

### Usage from Haskell

The `Kafka.Protocol.CRC32C` module provides high-level bindings:

```haskell
import qualified Kafka.Protocol.CRC32C as CRC

-- One-shot checksum
let checksum = CRC.crc32c myByteString

-- Incremental checksum
let crc = CRC.crc32cInit
    crc' = CRC.crc32cAppend crc chunk1
    crc'' = CRC.crc32cAppend crc' chunk2
    finalChecksum = CRC.crc32cFinalize crc''
```

### License

The CRC32C implementation is licensed under the MIT License. See [THIRD_PARTY_LICENSES.md](../THIRD_PARTY_LICENSES.md) for full license text and attribution.

### References

- [fast-crc32 GitHub Repository](https://github.com/corsix/fast-crc32)
- [CRC32C on Wikipedia](https://en.wikipedia.org/wiki/Cyclic_redundancy_check)
- [Intel® 64 and IA-32 Architectures Software Developer's Manual](https://software.intel.com/content/www/us/en/develop/articles/intel-sdm.html) (SSE4.2 CRC32 instructions)
