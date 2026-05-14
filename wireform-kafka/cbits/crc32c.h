/*
 * Fast CRC32C implementation
 * Based on https://github.com/corsix/fast-crc32
 *
 * MIT License
 *
 * Copyright (c) 2016 Peter Cawley
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#ifndef CRC32C_H
#define CRC32C_H

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Initialize a CRC32C computation.
 *
 * @return Initial CRC value (0xFFFFFFFF)
 */
uint32_t crc32c_init(void);

/**
 * Append data to an ongoing CRC32C computation.
 *
 * This function automatically selects the best implementation based on
 * available CPU features:
 * - AVX512 + VPCLMULQDQ on supported x86_64 CPUs (Ice Lake+, Genoa+)
 * - SSE4.2 hardware CRC32C instructions on x86/x64
 * - ARM CRC32 hardware instructions on AArch64 (ARMv8.1-A+, Apple Silicon)
 * - Software lookup table fallback for all other cases
 *
 * @param crc Current CRC value (from crc32c_init or previous crc32c_append)
 * @param data Pointer to data buffer
 * @param length Length of data in bytes
 * @return Updated CRC value
 */
uint32_t crc32c_append(uint32_t crc, const uint8_t* data, size_t length);

/**
 * Finalize a CRC32C computation.
 *
 * @param crc Current CRC value (from crc32c_append)
 * @return Final CRC32C checksum
 */
uint32_t crc32c_finalize(uint32_t crc);

/**
 * Compute CRC32C checksum of a data buffer in one call.
 *
 * Equivalent to:
 *   crc32c_finalize(crc32c_append(crc32c_init(), data, length))
 *
 * Uses the CRC-32C (Castagnoli) polynomial: 0x1EDC6F41
 *
 * @param data Pointer to data buffer
 * @param length Length of data in bytes
 * @return CRC32C checksum
 */
uint32_t crc32c(const uint8_t* data, size_t length);

#ifdef __cplusplus
}
#endif

#endif /* CRC32C_H */
