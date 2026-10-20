# Test, fuzz and memory-safety harness for the C kernels under */cbits.
#
# Entry point for every C check in the repository. See README.md for the
# full guide; the short version:
#
#   make test           unit + property + differential tests, ASan+UBSan
#   make test-release   same tests at the cabal optimisation level, with
#                       guard-page buffers and _FORTIFY_SOURCE
#   make test-msan      MemorySanitizer (Linux clang only)
#   make test-valgrind  release tests + corpus replay under memcheck (Linux)
#   make replay         replay every committed corpus + random inputs
#                       through the standalone driver (ASan+UBSan)
#   make fuzz-smoke     deterministic libFuzzer run over every target
#   make fuzz           time-boxed libFuzzer campaign (FUZZ_TIME=secs,
#                       FUZZER=name to pick one)
#   make fuzz-merge     fold new coverage from a campaign back into the
#                       committed seed corpus
#   make afl FUZZER=x   AFL++ campaign for one target (Linux)
#   make analyze        clang static analyzer over every kernel source
#   make coverage       line/branch coverage of the kernels from tests +
#                       corpora (needs llvm-profdata / llvm-cov)
#   make ci             what CI runs: test, test-release, replay,
#                       fuzz-smoke, analyze (+ test-msan on Linux)
#   make list           modules, variants and targets on this platform
#
# Modules live in <module>/module.mk; see README.md for the variables a
# module declares.

HARNESS_DIR := $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
ROOT := $(abspath $(HARNESS_DIR)/..)
BUILD ?= $(HARNESS_DIR)/build
UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)

ifeq ($(origin CC),default)
CC := $(or $(WIREFORM_CLANG),clang)
endif
FUZZ_CC ?= $(CC)
AFL_CC ?= afl-clang-fast
PKG_CONFIG ?= pkg-config
LLVM_PROFDATA ?= llvm-profdata
LLVM_COV ?= llvm-cov
VALGRIND ?= valgrind
# V=1 echoes full compiler command lines.
Q := $(if $(V),,@)

# Fuzzing knobs.
FUZZ_TIME ?= 300
FUZZ_RUNS ?= 20000
FUZZ_FORK ?=
FUZZER ?=
# Random inputs the standalone replay driver adds after the corpus.
REPLAY_RANDOM_RUNS ?= 2000

COMMON := $(HARNESS_DIR)/common
COMMON_DEPS := $(COMMON)/harness.h $(COMMON)/standalone_main.c $(COMMON)/watchdog.c $(HARNESS_DIR)/Makefile

BASE_CFLAGS := -std=gnu11 -g -pthread -I$(COMMON) \
  -Wall -Wextra -Werror -Wno-unused-function

# Memory ceiling (common/watchdog.c, WF_MEM_LIMIT_MB) for every binary
# that libFuzzer's own -rss_limit_mb does not cover.
WATCHDOG := $(COMMON)/watchdog.c
WATCHDOG_asan := $(WATCHDOG)
WATCHDOG_release := $(WATCHDOG)
WATCHDOG_msan := $(WATCHDOG)
WATCHDOG_tsan := $(WATCHDOG)
WATCHDOG_cov := $(WATCHDOG)
WATCHDOG_valgrind := $(WATCHDOG)

SANITIZE := address,undefined
SAN_FLAGS := -fsanitize=$(SANITIZE) -fno-sanitize-recover=all -fno-omit-frame-pointer

CFLAGS_asan := -O1 $(SAN_FLAGS)
LDFLAGS_asan := -fsanitize=$(SANITIZE)

# The release config mirrors the cabal build: each module's OPT flags
# (its cabal cc-options) plus hardening. Buffers come from guard pages.
CFLAGS_release := -fstack-protector-strong -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=2
LDFLAGS_release :=

CFLAGS_msan := -O1 -fsanitize=memory -fsanitize-memory-track-origins=2 -fno-omit-frame-pointer
LDFLAGS_msan := -fsanitize=memory

CFLAGS_tsan := -O1 -fsanitize=thread -fno-omit-frame-pointer
LDFLAGS_tsan := -fsanitize=thread

CFLAGS_cov := -O0 -fprofile-instr-generate -fcoverage-mapping
LDFLAGS_cov := -fprofile-instr-generate

CFLAGS_fuzz := -O1 -fsanitize=fuzzer,$(SANITIZE) -fno-sanitize-recover=all -fno-omit-frame-pointer
LDFLAGS_fuzz := -fsanitize=fuzzer,$(SANITIZE)

CFLAGS_afl := -O1 -fsanitize=fuzzer,$(SANITIZE) -fno-omit-frame-pointer
LDFLAGS_afl := -fsanitize=fuzzer,$(SANITIZE)

# valgrind cannot decode AVX-512; pin -march=native modules to x86-64-v3.
CFLAGS_valgrind := $(CFLAGS_release)
LDFLAGS_valgrind :=
ifeq ($(UNAME_M),x86_64)
ARCH_OVERRIDE_valgrind := -march=x86-64-v3
endif

CC_asan := $(CC)
CC_release := $(CC)
CC_msan := $(CC)
CC_tsan := $(CC)
CC_cov := $(CC)
CC_valgrind := $(CC)
CC_fuzz := $(FUZZ_CC)
CC_afl := $(AFL_CC)

export ASAN_OPTIONS ?= detect_stack_use_after_return=1:strict_string_checks=1:check_initialization_order=1:halt_on_error=1:abort_on_error=1
export UBSAN_OPTIONS ?= print_stacktrace=1:halt_on_error=1
export MSAN_OPTIONS ?= halt_on_error=1:abort_on_error=1
export TSAN_OPTIONS ?= halt_on_error=1
# The nix cc-wrapper strips -march=native unless told otherwise; the
# native variants must compile the SIMD paths the cabal build uses.
export NIX_ENFORCE_NO_NATIVE := 0

# ---------------------------------------------------------------------
# Module registry
# ---------------------------------------------------------------------
#
# Each <module>/module.mk appends its name to MODULES and sets, with the
# module name as prefix:
#
#   _SRCS      kernel .c files under test (absolute, via $(ROOT))
#   _CFLAGS    include dirs / defines shared by every config
#   _ARCH      arch flags the cabal build uses (e.g. -march=native);
#              applied in every config because they select SIMD paths
#   _OPT       optimisation flags from the cabal cc-options (release)
#   _LIBS      link flags (pkg-config output, -lfoo)
#   _TESTS     test programs: <module>/<name>.c, each with a main()
#   _FUZZERS   fuzz targets: <module>/<name>.c defining
#              LLVMFuzzerTestOneInput; seeds in corpus/<name>/,
#              crash reproducers in regressions/<name>/, optional
#              <name>.dict and <name>.options (extra libFuzzer flags)
#   _VARIANTS  build variants (default: native); each gets _VFLAGS_<v>
#   _PLATFORMS uname -s values the module builds on (default Linux Darwin)
#   _MSAN      "no" when it links uninstrumented C libraries
#   _TSAN      "yes" to also build and run tests under TSan

MODULES :=
include $(sort $(wildcard $(HARNESS_DIR)/*/module.mk))

active = $(if $(filter $(UNAME_S),$(or $($(1)_PLATFORMS),Linux Darwin)),$(1))
ACTIVE := $(strip $(foreach m,$(MODULES),$(call active,$(m))))
# ONLY=<module ...> restricts every target to those modules.
ifneq ($(ONLY),)
ACTIVE := $(filter $(ONLY),$(ACTIVE))
endif
variants = $(or $($(1)_VARIANTS),native)
msan_ok = $(if $(filter no,$($(1)_MSAN)),,$(1))
tsan_ok = $(if $(filter yes,$($(1)_TSAN)),$(1))

# $(1)=config $(2)=module: arch flags, honouring per-config overrides.
arch = $(if $($(2)_ARCH),$(or $(ARCH_OVERRIDE_$(1)),$($(2)_ARCH)))
# release-like configs take the module's cabal optimisation level.
opt = $(if $(filter release valgrind,$(1)),$(or $($(2)_OPT),-O2))

# ---------------------------------------------------------------------
# Build rules
# ---------------------------------------------------------------------

# $(1)=config $(2)=module $(3)=variant $(4)=program $(5)=extra sources
define PROG_RULE
$(BUILD)/$(1)/$(2)/$(3)/$(4): $(HARNESS_DIR)/$(2)/$(4).c $$($(2)_SRCS) $(5) $(COMMON_DEPS) $$(wildcard $(HARNESS_DIR)/$(2)/*.h) $$(foreach s,$$($(2)_SRCS),$$(wildcard $$(dir $$(s))*.h)) $(HARNESS_DIR)/$(2)/module.mk
	@mkdir -p $$(@D)
	@$(if $(V),:,echo "  CC  $(1)/$(2)/$(3)/$(4)")
	$$(Q)$$(CC_$(1)) $$(BASE_CFLAGS) $$(CFLAGS_$(1)) $$(call opt,$(1),$(2)) $$(call arch,$(1),$(2)) $$($(2)_CFLAGS) $$($(2)_VFLAGS_$(3)) -I$(HARNESS_DIR)/$(2) \
	  -o $$@ $(HARNESS_DIR)/$(2)/$(4).c $$($(2)_SRCS) $(5) $$(WATCHDOG_$(1)) $$($(2)_LIBS) $$(LDFLAGS_$(1))
endef

# Tests and standalone replay binaries for the non-libFuzzer configs.
NONFUZZ_CONFIGS := asan release msan tsan cov valgrind
$(foreach c,$(NONFUZZ_CONFIGS),\
  $(foreach m,$(ACTIVE),\
    $(foreach v,$(call variants,$(m)),\
      $(foreach t,$($(m)_TESTS),$(eval $(call PROG_RULE,$(c),$(m),$(v),$(t),)))\
      $(foreach f,$($(m)_FUZZERS),$(eval $(call PROG_RULE,$(c),$(m),$(v),$(f),$(COMMON)/standalone_main.c))))))

# libFuzzer / AFL++ binaries.
$(foreach c,fuzz afl,\
  $(foreach m,$(ACTIVE),\
    $(foreach v,$(call variants,$(m)),\
      $(foreach f,$($(m)_FUZZERS),$(eval $(call PROG_RULE,$(c),$(m),$(v),$(f),))))))

# Binary lists. $(1)=config, $(2)=module filter function.
tests_of = $(foreach m,$(2),$(foreach v,$(call variants,$(m)),$(foreach t,$($(m)_TESTS),$(BUILD)/$(1)/$(m)/$(v)/$(t))))
fuzzers_of = $(foreach m,$(2),$(foreach v,$(call variants,$(m)),$(foreach f,$($(m)_FUZZERS),$(if $(FUZZER),$(filter $(FUZZER),$(f)),$(f)))))
fuzz_bins = $(foreach m,$(2),$(foreach v,$(call variants,$(m)),$(foreach f,$($(m)_FUZZERS),$(if $(FUZZER),$(if $(filter $(FUZZER),$(f)),$(BUILD)/$(1)/$(m)/$(v)/$(f)),$(BUILD)/$(1)/$(m)/$(v)/$(f)))))

MSAN_MODULES := $(strip $(foreach m,$(ACTIVE),$(call msan_ok,$(m))))
TSAN_MODULES := $(strip $(foreach m,$(ACTIVE),$(call tsan_ok,$(m))))

# Path helpers: a binary path build/<cfg>/<module>/<variant>/<name>.
mod_of = $(word 1,$(subst /, ,$(patsubst $(BUILD)/%,%,$(1))))
mod_of2 = $(word 2,$(subst /, ,$(patsubst $(BUILD)/%,%,$(1))))
name_of = $(notdir $(1))
corpus_dirs = $(HARNESS_DIR)/$(call mod_of2,$(1))/corpus/$(call name_of,$(1)) $(HARNESS_DIR)/$(call mod_of2,$(1))/regressions/$(call name_of,$(1))
dict_of = $(wildcard $(HARNESS_DIR)/$(call mod_of2,$(1))/$(call name_of,$(1)).dict)
opts_of = $(shell cat $(wildcard $(HARNESS_DIR)/$(call mod_of2,$(1))/$(call name_of,$(1)).options) /dev/null)
variant_of = $(word 3,$(subst /, ,$(patsubst $(BUILD)/%,%,$(1))))

# ---------------------------------------------------------------------
# Run targets
# ---------------------------------------------------------------------

.PHONY: all test test-release test-msan test-tsan test-valgrind replay replay-release \
        fuzz-build fuzz-smoke fuzz fuzz-merge afl-build afl analyze coverage ci list clean

all: test

# Each run is a phony "<binary>.run" target so `make -k` reports every
# failing binary rather than stopping at the first.
define RUN_TEST
.PHONY: $(1).run
$(1).run: $(1)
	@echo "== $(patsubst $(BUILD)/%,%,$(1))"
	@$(2) $(1)
endef

define RUN_REPLAY
.PHONY: $(1).replay
$(1).replay: $(1)
	@echo "== replay $(patsubst $(BUILD)/%,%,$(1))"
	@WF_RANDOM_RUNS=$(REPLAY_RANDOM_RUNS) $(2) $(1) $(call corpus_dirs,$(1))
endef

VALGRIND_CMD := $(VALGRIND) -q --error-exitcode=99 --leak-check=full --errors-for-leak-kinds=definite --track-origins=yes

$(foreach b,$(call tests_of,asan,$(ACTIVE)) $(call tests_of,release,$(ACTIVE)) \
            $(call tests_of,msan,$(MSAN_MODULES)) $(call tests_of,tsan,$(TSAN_MODULES)) \
            $(call tests_of,cov,$(ACTIVE)),$(eval $(call RUN_TEST,$(b),)))
$(foreach b,$(call tests_of,valgrind,$(ACTIVE)),$(eval $(call RUN_TEST,$(b),$(VALGRIND_CMD))))
$(foreach b,$(call fuzz_bins,asan,$(ACTIVE)) $(call fuzz_bins,release,$(ACTIVE)) \
            $(call fuzz_bins,msan,$(MSAN_MODULES)) $(call fuzz_bins,cov,$(ACTIVE)),$(eval $(call RUN_REPLAY,$(b),)))
$(foreach b,$(call fuzz_bins,valgrind,$(ACTIVE)),$(eval $(call RUN_REPLAY,$(b),$(VALGRIND_CMD))))

test: $(addsuffix .run,$(call tests_of,asan,$(ACTIVE)))
test-release: $(addsuffix .run,$(call tests_of,release,$(ACTIVE)))
test-msan: $(addsuffix .run,$(call tests_of,msan,$(MSAN_MODULES)))
test-tsan: $(addsuffix .run,$(call tests_of,tsan,$(TSAN_MODULES)))
test-valgrind: $(addsuffix .run,$(call tests_of,valgrind,$(ACTIVE))) \
               $(addsuffix .replay,$(call fuzz_bins,valgrind,$(ACTIVE)))
replay: $(addsuffix .replay,$(call fuzz_bins,asan,$(ACTIVE)))
replay-release: $(addsuffix .replay,$(call fuzz_bins,release,$(ACTIVE)))
replay-msan: $(addsuffix .replay,$(call fuzz_bins,msan,$(MSAN_MODULES)))

fuzz-build: $(call fuzz_bins,fuzz,$(ACTIVE))
afl-build: $(call fuzz_bins,afl,$(ACTIVE))

# libFuzzer writes new inputs into its first corpus directory, so that is
# always a scratch dir under build/; committed seeds are read-only inputs.
scratch_of = $(BUILD)/corpus/$(call mod_of2,$(1))/$(call variant_of,$(1))/$(call name_of,$(1))
crashdir_of = $(BUILD)/crashes/$(call mod_of2,$(1))/$(call variant_of,$(1))/$(call name_of,$(1))
fuzz_cmd = mkdir -p $(call scratch_of,$(1)) $(call crashdir_of,$(1)) && \
  $(1) $(call scratch_of,$(1)) $(wildcard $(call corpus_dirs,$(1))) \
    $(if $(call dict_of,$(1)),-dict=$(call dict_of,$(1))) $(call opts_of,$(1)) \
    -artifact_prefix=$(call crashdir_of,$(1))/

define FUZZ_RUN
.PHONY: $(1).smoke $(1).fuzz $(1).merge
$(1).smoke: $(1)
	@echo "== fuzz-smoke $(patsubst $(BUILD)/%,%,$(1))"
	@mkdir -p $(call crashdir_of,$(1)); log=$(call crashdir_of,$(1)).smoke.log; \
	  { $(call fuzz_cmd,$(1)) -runs=$(FUZZ_RUNS) -seed=1; } > $$$$log 2>&1 \
	    && grep -E '^Done ' $$$$log || { cat $$$$log; exit 1; }
$(1).fuzz: $(1)
	@echo "== fuzz $(patsubst $(BUILD)/%,%,$(1)) for $(FUZZ_TIME)s"
	$(call fuzz_cmd,$(1)) -max_total_time=$(FUZZ_TIME) -print_final_stats=1 $(if $(FUZZ_FORK),-fork=$(FUZZ_FORK) -ignore_crashes=1)
$(1).merge: $(1)
	@mkdir -p $(HARNESS_DIR)/$(call mod_of2,$(1))/corpus/$(call name_of,$(1))
	$(1) -merge=1 $(call opts_of,$(1)) $(HARNESS_DIR)/$(call mod_of2,$(1))/corpus/$(call name_of,$(1)) $(call scratch_of,$(1))
endef
$(foreach b,$(call fuzz_bins,fuzz,$(ACTIVE)),$(eval $(call FUZZ_RUN,$(b))))

fuzz-smoke: $(addsuffix .smoke,$(call fuzz_bins,fuzz,$(ACTIVE)))
fuzz: $(addsuffix .fuzz,$(call fuzz_bins,fuzz,$(ACTIVE)))
fuzz-merge: $(addsuffix .merge,$(call fuzz_bins,fuzz,$(ACTIVE)))

afl: afl-build
	@test -n "$(FUZZER)" || { echo "afl: set FUZZER=<target>"; exit 2; }
	@for b in $(call fuzz_bins,afl,$(ACTIVE)); do \
	  m=$$(echo $$b | sed 's|$(BUILD)/afl/||' | cut -d/ -f1); \
	  in=$(HARNESS_DIR)/$$m/corpus/$(FUZZER); out=$(BUILD)/afl-out/$$m/$(FUZZER); \
	  d=$(HARNESS_DIR)/$$m/$(FUZZER).dict; mkdir -p $$out; \
	  echo "== afl-fuzz $$b ($(FUZZ_TIME)s)"; \
	  afl-fuzz -V $(FUZZ_TIME) -i $$in -o $$out $$( [ -f $$d ] && echo -x $$d ) -- $$b || exit $$?; \
	done

# ---------------------------------------------------------------------
# Static analysis
# ---------------------------------------------------------------------

# DeprecatedOrUnsafeBufferHandling flags every memcpy (it wants Annex K
# memcpy_s); memcpy is the correct tool for unaligned loads here.
ANALYZE_FLAGS := --analyze -Wno-unused-command-line-argument -Xclang -analyzer-werror -Xanalyzer -analyzer-output=text \
  -Xanalyzer -analyzer-checker=core,unix,security,deadcode,nullability \
  -Xanalyzer -analyzer-disable-checker=security.insecureAPI.DeprecatedOrUnsafeBufferHandling

define ANALYZE_RULE
.PHONY: analyze-$(1)
analyze-$(1):
	@echo "== analyze $(1)"
	@for s in $$($(1)_SRCS); do \
	  $$(CC) $$(ANALYZE_FLAGS) -std=gnu11 $$(call arch,analyze,$(1)) $$($(1)_CFLAGS) -o /dev/null $$$$s || exit 1; \
	done
endef
$(foreach m,$(ACTIVE),$(eval $(call ANALYZE_RULE,$(m))))
analyze: $(addprefix analyze-,$(ACTIVE))

# ---------------------------------------------------------------------
# Coverage
# ---------------------------------------------------------------------

COV_DIR := $(BUILD)/coverage
COV_TESTS = $(call tests_of,cov,$(ACTIVE))
COV_FUZZ = $(call fuzz_bins,cov,$(ACTIVE))
KERNEL_SRCS = $(sort $(foreach m,$(ACTIVE),$($(m)_SRCS)))

coverage: $(COV_TESTS) $(COV_FUZZ)
	@rm -rf $(COV_DIR) && mkdir -p $(COV_DIR)/raw
	@for b in $(COV_TESTS); do \
	  LLVM_PROFILE_FILE=$(COV_DIR)/raw/$$(echo $$b | tr / _).%p.profraw $$b >/dev/null || exit 1; done
	@for b in $(COV_FUZZ); do \
	  m=$$(echo $$b | sed 's|$(BUILD)/cov/||' | cut -d/ -f1); n=$$(basename $$b); \
	  LLVM_PROFILE_FILE=$(COV_DIR)/raw/$$(echo $$b | tr / _).%p.profraw \
	    WF_RANDOM_RUNS=$(REPLAY_RANDOM_RUNS) $$b $(HARNESS_DIR)/$$m/corpus/$$n $(HARNESS_DIR)/$$m/regressions/$$n $(BUILD)/corpus/$$m/*/$$n >/dev/null || exit 1; done
	@$(LLVM_PROFDATA) merge -sparse -o $(COV_DIR)/all.profdata $(COV_DIR)/raw/*.profraw
	@objs=""; for b in $(COV_TESTS) $(COV_FUZZ); do \
	    if [ -z "$$objs" ]; then objs="$$b"; else objs="$$objs -object=$$b"; fi; done; \
	  $(LLVM_COV) report -instr-profile=$(COV_DIR)/all.profdata $$objs $(KERNEL_SRCS) | tee $(COV_DIR)/report.txt; \
	  $(LLVM_COV) show -format=html -output-dir=$(COV_DIR)/html -instr-profile=$(COV_DIR)/all.profdata $$objs $(KERNEL_SRCS) >/dev/null; \
	  echo "html: $(COV_DIR)/html/index.html"

# ---------------------------------------------------------------------
# Aggregates
# ---------------------------------------------------------------------

CI_TARGETS := test test-release replay replay-release fuzz-smoke analyze
ifeq ($(UNAME_S),Linux)
CI_TARGETS += test-msan replay-msan
ifneq ($(shell command -v $(VALGRIND) 2>/dev/null),)
CI_TARGETS += test-valgrind
endif
endif
ifneq ($(TSAN_MODULES),)
CI_TARGETS += test-tsan
endif

ci: $(CI_TARGETS)

list:
	@echo "platform: $(UNAME_S) $(UNAME_M)   CC=$(CC)   FUZZ_CC=$(FUZZ_CC)"
	@$(foreach m,$(MODULES),echo "$(m)$(if $(call active,$(m)),, (inactive here))"; \
	  echo "    variants: $(call variants,$(m))"; \
	  echo "    tests:    $($(m)_TESTS)"; \
	  echo "    fuzzers:  $($(m)_FUZZERS)"; \
	  echo "    sources:  $(patsubst $(ROOT)/%,%,$($(m)_SRCS))";)

clean:
	rm -rf $(BUILD)
