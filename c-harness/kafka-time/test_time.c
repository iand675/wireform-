/* Unit tests for wireform-kafka wireform_time.c. */
#include "harness.h"
#include "api.h"

#include <sys/time.h>
#include <time.h>

static int64_t tv_micros(void)
{
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return (int64_t)tv.tv_sec * 1000000 + tv.tv_usec;
}

/* Linux's CLOCK_REALTIME_COARSE lags the precise clock by up to one
 * kernel tick (HZ=100 gives 10 ms); allow a generous 50 ms. */
enum { SLACK_US = 50000 };

/* Each reading lies between two gettimeofday() readings taken around it. */
static void brackets_gettimeofday(void)
{
    long iters = wf_iters(2000);
    for (long i = 0; i < iters; i++) {
        int64_t before = tv_micros();
        int64_t us = wireform_current_time_micros();
        int64_t ms = wireform_current_time_millis();
        int64_t after = tv_micros();
        WF_CHECK(us >= before - SLACK_US && us <= after + SLACK_US);
        WF_CHECK(ms >= (before - SLACK_US) / 1000 && ms <= (after + SLACK_US) / 1000);
    }
}

/* Same scale as time(): seconds agree, and the value is a plausible
 * present (after 2020-01-01, before 2200). */
static void scale_and_epoch(void)
{
    time_t t0 = time(NULL);
    int64_t ms = wireform_current_time_millis();
    int64_t us = wireform_current_time_micros();
    time_t t1 = time(NULL);
    WF_CHECK(ms / 1000 >= (int64_t)t0 - 1 && ms / 1000 <= (int64_t)t1 + 1);
    WF_CHECK(us / 1000000 >= (int64_t)t0 - 1 && us / 1000000 <= (int64_t)t1 + 1);
    WF_CHECK(ms > 1577836800000LL);
    WF_CHECK(ms < 7258118400000LL);
    WF_CHECK(us > 1577836800000000LL);
}

/* millis and micros come from the same clock with truncating division:
 * a millis reading taken between two micros readings lies between their
 * millisecond truncations. */
static void millis_micros_consistent(void)
{
    long iters = wf_iters(20000);
    for (long i = 0; i < iters; i++) {
        int64_t u0 = wireform_current_time_micros();
        int64_t m = wireform_current_time_millis();
        int64_t u1 = wireform_current_time_micros();
        if (u1 < u0) continue; /* wall clock stepped backwards (NTP) */
        WF_CHECK(m >= u0 / 1000 && m <= u1 / 1000);
    }
}

/* Successive readings never go backwards by more than the coarse clock's
 * resolution while the system clock is not being stepped. */
static void no_large_backward_steps(void)
{
    int64_t prev = wireform_current_time_micros();
    long iters = wf_iters(100000);
    long back = 0;
    for (long i = 0; i < iters; i++) {
        int64_t now = wireform_current_time_micros();
        if (now < prev - SLACK_US) back++;
        prev = now;
    }
    WF_CHECK_EQ(back, 0);
}

int main(void)
{
    WF_RUN(brackets_gettimeofday);
    WF_RUN(scale_and_epoch);
    WF_RUN(millis_micros_consistent);
    WF_RUN(no_large_backward_steps);
    return wf_finish("test_time");
}
