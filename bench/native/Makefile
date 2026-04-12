# bench/native/Makefile — build and run lexbor comparison benchmarks
#
# Prerequisites: lexbor (provided via nix)
#   nix-shell -p lexbor --run "make run"
#
# Or if lexbor is already available:
#   make run

CC       ?= cc
CFLAGS   := -O3 -march=native -Wall -Wextra -std=c11
LDFLAGS  :=

# Try pkg-config first; fall back to nix store path.
LEXBOR_CFLAGS := $(shell pkg-config --cflags lexbor 2>/dev/null)
LEXBOR_LIBS   := $(shell pkg-config --libs lexbor 2>/dev/null)

ifeq ($(LEXBOR_LIBS),)
  LEXBOR_PREFIX := $(shell nix build 'nixpkgs\#lexbor' --no-link --print-out-paths 2>/dev/null)
  ifneq ($(LEXBOR_PREFIX),)
    LEXBOR_CFLAGS := -I$(LEXBOR_PREFIX)/include
    LEXBOR_LIBS   := -L$(LEXBOR_PREFIX)/lib -Wl,-rpath,$(LEXBOR_PREFIX)/lib -llexbor
  else
    $(error lexbor not found — install via nix or set PKG_CONFIG_PATH)
  endif
endif

SRCS     := html_bench.c
TARGET   := html_bench

.PHONY: all clean run

all: $(TARGET)

$(TARGET): $(SRCS)
	$(CC) $(CFLAGS) $(LEXBOR_CFLAGS) -o $@ $^ $(LDFLAGS) $(LEXBOR_LIBS)

run: $(TARGET)
	./$(TARGET)

clean:
	rm -f $(TARGET)
