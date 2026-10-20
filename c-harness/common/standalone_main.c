/*
 * Replay driver for libFuzzer-style targets, linked instead of libFuzzer
 * when a target is built without -fsanitize=fuzzer (gcc, valgrind, MSan,
 * coverage, -O3 release builds).
 *
 *   replay_bin [FILE | DIR]...
 *
 * Runs LLVMFuzzerTestOneInput once per file (directories are read one
 * level deep, dotfiles skipped). Each input is copied into an exact-size
 * buffer (wf_dup) so overreads fault even without a sanitizer.
 *
 * WF_RANDOM_RUNS=N additionally feeds N pseudo-random inputs (lengths in
 * [0, WF_RANDOM_MAXLEN], default 4096), seeded by WF_SEED. Half of them
 * are mutations of the replayed files, so the random phase starts from
 * structured inputs rather than pure noise.
 */
#include "harness.h"

#include <dirent.h>
#include <errno.h>
#include <sys/stat.h>

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size);

typedef struct {
    uint8_t *p;
    size_t n;
} blob;

static blob *seeds;
static size_t nseeds, capseeds;

static void run_one(const uint8_t *p, size_t n)
{
    uint8_t *copy = wf_dup(p, n);
    LLVMFuzzerTestOneInput(copy, n);
    wf_free(copy, n);
}

static int read_file(const char *path, blob *out)
{
    FILE *f = fopen(path, "rb");
    if (!f) {
        fprintf(stderr, "replay: cannot open %s: %s\n", path, strerror(errno));
        return -1;
    }
    size_t cap = 4096, n = 0;
    uint8_t *buf = malloc(cap);
    if (!buf) abort();
    for (;;) {
        if (n == cap) {
            cap *= 2;
            buf = realloc(buf, cap);
            if (!buf) abort();
        }
        size_t r = fread(buf + n, 1, cap - n, f);
        n += r;
        if (r == 0) break;
    }
    fclose(f);
    out->p = buf;
    out->n = n;
    return 0;
}

static void add_file(const char *path)
{
    blob b;
    if (read_file(path, &b) != 0) exit(2);
    run_one(b.p, b.n);
    if (nseeds == capseeds) {
        capseeds = capseeds ? capseeds * 2 : 64;
        seeds = realloc(seeds, capseeds * sizeof *seeds);
        if (!seeds) abort();
    }
    seeds[nseeds++] = b;
}

static void add_path(const char *path)
{
    struct stat st;
    if (stat(path, &st) != 0) {
        /* A missing corpus directory is not an error: modules may have no
         * seeds or regressions yet. */
        if (errno == ENOENT) return;
        fprintf(stderr, "replay: stat %s: %s\n", path, strerror(errno));
        exit(2);
    }
    if (!S_ISDIR(st.st_mode)) {
        add_file(path);
        return;
    }
    DIR *d = opendir(path);
    if (!d) {
        fprintf(stderr, "replay: opendir %s: %s\n", path, strerror(errno));
        exit(2);
    }
    struct dirent *e;
    while ((e = readdir(d)) != NULL) {
        if (e->d_name[0] == '.') continue;
        size_t len = strlen(path) + strlen(e->d_name) + 2;
        char *child = malloc(len);
        if (!child) abort();
        snprintf(child, len, "%s/%s", path, e->d_name);
        struct stat cst;
        if (stat(child, &cst) == 0 && S_ISREG(cst.st_mode)) add_file(child);
        free(child);
    }
    closedir(d);
}

int main(int argc, char **argv)
{
    for (int i = 1; i < argc; i++) {
        if (argv[i][0] == '-') continue; /* tolerate libFuzzer-style flags */
        add_path(argv[i]);
    }
    size_t replayed = nseeds;

    const char *runs_s = getenv("WF_RANDOM_RUNS");
    long runs = runs_s ? strtol(runs_s, NULL, 10) : 0;
    const char *max_s = getenv("WF_RANDOM_MAXLEN");
    size_t maxlen = max_s ? (size_t)strtoull(max_s, NULL, 10) : 4096;
    wf_rng r = wf_rng_from_env();
    uint8_t *scratch = malloc(maxlen ? maxlen : 1);
    if (!scratch) abort();

    for (long i = 0; i < runs; i++) {
        size_t n = (size_t)wf_rng_below(&r, maxlen + 1);
        if (nseeds && (wf_rng_next(&r) & 1)) {
            /* Mutate a seed: copy a prefix, then flip a few bytes. */
            blob *s = &seeds[wf_rng_below(&r, nseeds)];
            n = s->n < maxlen ? s->n : maxlen;
            if (n) memcpy(scratch, s->p, n);
            int flips = 1 + (int)wf_rng_below(&r, 8);
            for (int k = 0; k < flips && n; k++) {
                scratch[wf_rng_below(&r, n)] = (uint8_t)wf_rng_next(&r);
            }
            if (wf_rng_next(&r) & 1) n = (size_t)wf_rng_below(&r, n + 1);
        } else {
            wf_rng_fill(&r, scratch, n);
        }
        run_one(scratch, n);
    }
    free(scratch);

    printf("replay: %zu files, %ld random inputs\n", replayed, runs);
    return 0;
}
