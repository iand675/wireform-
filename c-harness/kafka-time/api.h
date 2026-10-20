/* Kernel API as wireform-kafka/src/Kafka/Time.hs imports it.
 *
 * Contract: no arguments; both return the wall clock (CLOCK_REALTIME, or
 * CLOCK_REALTIME_COARSE on Linux) as a count since the POSIX epoch,
 * truncated (not rounded) to milliseconds / microseconds. The Haskell
 * side uses them for Kafka record timestamps and stats.json, so the
 * values must agree with gettimeofday()/time() to within the coarse
 * clock's tick, and millis must equal micros / 1000 for the same
 * instant. */
#ifndef KAFKA_TIME_API_H
#define KAFKA_TIME_API_H

#include <stdint.h>

int64_t wireform_current_time_millis(void);
int64_t wireform_current_time_micros(void);

#endif
