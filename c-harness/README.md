# C kernel harness

Tests, fuzz targets and memory-safety checks for every first-party C file
under `*/cbits/`. The Haskell test suites exercise these kernels only
through their FFI wrappers, with whatever inputs those suites happen to
build; this directory tests the C directly, at every buffer boundary and
alignment, under AddressSanitizer, UndefinedBehaviorSanitizer,
MemorySanitizer, valgrind, guard-page builds at the cabal optimisation
level, the clang static analyzer, and libFuzzer / AFL++.

Everything is driven by `make` from this directory (GNU make 3.82+).

## Toolchain

```bash
nix develop .#c-harness        # clang, llvm-cov, pkg-config, the C libs,
                               # valgrind + AFL++ on Linux
make -C c-harness ci
```

The shell and the `checks.c-harness` derivation pin clang (`CC`,
`FUZZ_CC`, `WIREFORM_CLANG`), because every config needs compiler-rt
(ASan/UBSan/MSan, libFuzzer) and because `-Werror` with gcc trips on
`-Wmaybe-uninitialized` false positives in simde and on pointer arguments
it cannot see through. Any clang with compiler-rt works outside the shell
(`make CC=/path/to/clang`). The `fuzz` and `afl` configs need libFuzzer,
which Apple's Xcode clang does not ship.

macOS 26+ note: the AddressSanitizer runtime in LLVM 21 and in Xcode
before 26.4 deadlocks at process start on macOS 26 and later
([llvm/llvm-project#200447](https://github.com/llvm/llvm-project/issues/200447)).
The `c-harness` shell uses LLVM 22, whose runtime starts normally; if ASan
binaries hang before `main`, you are on an affected runtime.

## Targets

| Target | What it runs |
|---|---|
| `make test` | every unit / property / differential test under ASan + UBSan |
| `make test-release` | the same tests at each module's cabal `cc-options` (e.g. `-O3 -march=native`) with `_FORTIFY_SOURCE`, stack protector and guard-page buffers |
| `make test-msan` | tests under MemorySanitizer (Linux; modules that link uninstrumented C libraries opt out) |
| `make test-tsan` | tests of modules that opt into ThreadSanitizer |
| `make test-valgrind` | release tests and corpus replay under memcheck (Linux; `-march=native` capped at x86-64-v3 because valgrind cannot decode AVX-512) |
| `make replay` / `replay-release` / `replay-msan` | every committed seed and regression input, plus `REPLAY_RANDOM_RUNS` random and mutated inputs, through the standalone driver |
| `make fuzz-smoke` | deterministic libFuzzer run of every target (`FUZZ_RUNS`, seed 1) |
| `make fuzz` | time-boxed libFuzzer campaign: `FUZZ_TIME=600 FUZZER=fuzz_foo FUZZ_FORK=8` |
| `make fuzz-merge` | minimise a campaign's new coverage into the committed corpus |
| `make afl FUZZER=fuzz_foo` | AFL++ campaign for one target (`AFL_CC`, default `afl-clang-fast`) |
| `make analyze` | clang static analyzer, warnings are errors |
| `make coverage` | llvm-cov line/branch report of the kernel sources from all tests and corpora; HTML in `build/coverage/html` |
| `make ci` | test, test-release, replay, replay-release, fuzz-smoke, analyze, plus the MSan and TSan targets where available |
| `make list` | modules, variants, tests and fuzzers active on this platform |

`make -k` keeps going past a failing binary so one run reports all of
them; `ONLY="mod-a mod-b"` restricts any target to those modules. Property
tests read `WF_SEED` (replay a failure with the seed it
prints) and `WF_ITERS` (iteration multiplier for long runs).

Every test and replay binary carries a memory ceiling: a watchdog thread
aborts the process once its private memory passes `WF_MEM_LIMIT_MB`
(default 4096, 0 disables), so a runaway test fails its own binary instead
of the machine. libFuzzer binaries use `-rss_limit_mb` for the same job.

Build output, scratch corpora and crash artifacts go to `build/`
(git-ignored). libFuzzer always writes new inputs to a scratch directory
under `build/corpus/`; committed corpora are read-only inputs until you run
`fuzz-merge`.

## Layout

```
c-harness/
  Makefile               rule generator; every module plugs in via module.mk
  common/harness.h       checks, PRNG, exact-size and guard-page buffers
  common/standalone_main.c  libFuzzer-compatible replay driver
  <module>/
    module.mk            sources, flags, tests, fuzzers, variants
    api.h                kernel prototypes as the FFI imports them, plus
                         scalar reference implementations
    test_*.c             unit, boundary, property and differential tests
    fuzz_*.c             LLVMFuzzerTestOneInput targets
    corpus/<fuzzer>/     committed seed corpus
    regressions/<fuzzer>/  reproducers for fixed bugs, replayed everywhere
    <fuzzer>.dict        libFuzzer / AFL dictionary (optional)
    <fuzzer>.options     extra libFuzzer flags, e.g. -max_len=65536 (optional)
```

## Adding a module

`<module>/module.mk` appends to `MODULES` and sets variables prefixed with
the module name:

| Variable | Meaning |
|---|---|
| `_SRCS` | kernel `.c` files, as `$(ROOT)/pkg/cbits/x.c` |
| `_CFLAGS` | include dirs and defines for every config (`-isystem` for vendored simde) |
| `_ARCH` | arch flags from the cabal file (`-march=native`); applied in every config because they pick the SIMD path |
| `_OPT` | optimisation flags from the cabal `cc-options`; release and valgrind configs only |
| `_LIBS` | link flags, usually `$(shell $(PKG_CONFIG) --libs foo)` |
| `_TESTS` / `_FUZZERS` | program names; each is `<module>/<name>.c` |
| `_VARIANTS` | build variants (default `native`); `_VFLAGS_<variant>` holds each one's flags |
| `_PLATFORMS` | `uname -s` values to build on (default `Linux Darwin`) |
| `_MSAN` | `no` if the module links uninstrumented C libraries |
| `_TSAN` | `yes` to run its tests under TSan |

Modules built on simde use two variants: `native` (simde lowers to the host
ISA, NEON on arm64, SSE/AVX on x86-64) and `portable`
(`-DSIMDE_NO_NATIVE`, simde's plain-C fallbacks). Code with hand-written
`#ifdef __x86_64__` / `__ARM_NEON` branches is covered by running the
suite on both architectures; CI runs it on x86-64 Linux.

## Writing tests and fuzz targets

- Declare the kernel API in `api.h` exactly as the Haskell
  `foreign import` uses it, and document the caller contract there. Test
  every input the contract allows; inputs it forbids are out of scope only
  if no Haskell caller can produce them from untrusted data.
- Compare against a slow, obviously correct reference in `api.h` wherever
  one is cheap to write (scanners, decoders, checksums); prefer round
  trips for encoder/decoder pairs.
- Put input bytes in exact-size buffers (`wf_dup`, `wf_alloc`) so an
  overread faults, and sweep lengths across the scalar-prologue /
  SIMD-block / epilogue boundaries with the buffer flush against a guard
  page (`wf_guard_tail`).
- Fuzz targets abort on an invariant failure (`WF_FUZZ_ASSERT`), never
  `exit()`, and must not define `LLVMFuzzerInitialize`.
- Every fixed bug gets its reproducer in `regressions/<fuzzer>/` (or a
  named unit test), so `replay` keeps it fixed in every config.

## Failure injection (`fail` variants)

The out-of-memory and library-failure error paths are unreachable
through the public API: no input makes `malloc` return NULL or
`inflateInit2` fail. A `fail` build variant makes them testable by
compiling the kernels with selected libc and third-party entry points
renamed to failpoint-aware fakes.

Mechanics (see `kafka-compress/` for the worked example:
`module.mk`'s `VFLAGS_fail`, `failfns.h`, `failfns.c`, `test_fail.c`):

- `common/wf_fail.h` provides the failpoint core and the allocator
  fakes (`-Dmalloc=wf_fail_malloc -Dcalloc=wf_fail_calloc`).
  Failpoints arm through the environment: `setenv("WF_FAIL_<name>", "N", 1)`
  fails the Nth subsequent call to that fake (a countdown, so the call
  after it passes). The environment is the shared state, so arming from
  the test translation unit reaches the kernels' fakes directly.
- Third-party entry points are renamed with function-macros in the
  variant's flags, e.g. `-D'inflateInit2_(a,b,c,d)=wf_fail_inflateInit2_(a,b,c,d)'`.
  Their fakes live in the module's `failfns.c` (wired in via the
  module's `_EXTRA`), which starts by `#undef`ing the renames and
  declaring the real entry points explicitly: the force-included
  headers saw the renames and declared the fakes instead. The fakes
  pass through when unarmed, so a fail variant runs ordinary tests
  too. (Asm labels do not bind on Mach-O; the `#undef` pattern is
  portable.)
- Fake modes beyond "fail" are fine where a caller's loop needs a
  specific lie: `LZ4F_decompress`-style fakes that consume nothing,
  claim a full buffer without writing it (`malloc_stub` hands out a
  small real block for a huge request), or consume one byte and
  produce nothing drive the no-progress, growth-guard and
  mid-frame-iteration paths.
- Failure tests live in the module's `test_fail.c` under
  `#ifdef WF_FAIL_VARIANT`, so the binary passes trivially in the
  other variants. Note `WF_SANITIZED_HEAP` is always defined (0 or 1),
  so skip sanitizer-hostile cases with `#if !WF_SANITIZED_HEAP`, not
  `#ifndef`.
