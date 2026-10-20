/*
 * wf_pmd.c
 *
 * RFC 7692 permessage-deflate FFI shim.  See wf_pmd.h for the API.
 *
 * Implementation notes:
 *
 * * Raw deflate (no zlib header) selected via negative
 *   window_bits to inflateInit2 / deflateInit2.  RFC 7692 sec 7.2
 *   says compressed messages are raw DEFLATE blocks per RFC 1951.
 *
 * * The PMD framing wraps each message with a Z_SYNC_FLUSH, which
 *   emits a non-compressed-block trailer of 00 00 FF FF.  The
 *   Haskell side strips that on send and appends it on receive
 *   (RFC 7692 sec 7.2.1 / 7.2.2).
 *
 * * No global state, no thread-local state, no locks.  The Haskell
 *   side serialises per-connection access via the existing
 *   connection-level send / recv locks (or single-threaded mode,
 *   where the user has already promised no concurrent use).
 *
 * * The opaque 'wf_pmd_stream' wraps the z_stream plus the shim's own
 *   bookkeeping: input beyond what fits zlib's 32-bit counters, and
 *   whether the current message already restarted after a BFINAL
 *   block.
 */

#include <stdlib.h>
#include <string.h>
#include <zlib.h>

#include "wf_pmd.h"

struct wf_pmd_stream {
    z_stream zs;
    int      kind;      /* 0 = inflate, 1 = deflate */
    int      restarted; /* inflate: this message already restarted after a BFINAL block */
    size_t   in_left;   /* caller input beyond zs.avail_in, not yet handed to zlib */
};

/* zlib counts input and output in 32-bit uInt; the shim takes size_t
 * and hands zlib at most this much at a time. Overridable so the test
 * harness can exercise the splitting with small buffers. */
#ifndef WF_PMD_CHUNK
#define WF_PMD_CHUNK ((size_t)(uInt)-1)
#endif

/* Install a new message's input. src == NULL && src_len == 0 is the
 * resume call after WF_PMD_NEEDS_MORE_OUT: the z_stream (and in_left)
 * still hold the unconsumed input, so leave them alone. */
static void pmd_set_input(wf_pmd_stream *s, const uint8_t *src, size_t src_len) {
    if (src == NULL && src_len == 0) {
        return;
    }
    size_t n = src_len < WF_PMD_CHUNK ? src_len : WF_PMD_CHUNK;
    s->zs.next_in  = (Bytef *) src;
    s->zs.avail_in = (uInt) n;
    s->in_left     = src_len - n;
    s->restarted   = 0;
}

/* Top up whichever side zlib drained from what the caller has left. */
static void pmd_refill(wf_pmd_stream *s, size_t *out_left) {
    if (s->zs.avail_in == 0 && s->in_left != 0) {
        size_t n = s->in_left < WF_PMD_CHUNK ? s->in_left : WF_PMD_CHUNK;
        s->zs.avail_in = (uInt) n;
        s->in_left -= n;
    }
    if (s->zs.avail_out == 0 && *out_left != 0) {
        size_t n = *out_left < WF_PMD_CHUNK ? *out_left : WF_PMD_CHUNK;
        s->zs.avail_out = (uInt) n;
        *out_left -= n;
    }
}

/* zlib stopped on a drained chunk but the caller has more of it. */
static int pmd_more(const wf_pmd_stream *s, size_t out_left) {
    return (s->zs.avail_in == 0 && s->in_left != 0)
        || (s->zs.avail_out == 0 && out_left != 0);
}

/* RFC 7692 sec 7.2.2: the receiver appends this to every message. */
static const uint8_t pmd_tail[4] = { 0x00, 0x00, 0xff, 0xff };

/* Start a new raw-deflate stream after a BFINAL = 1 block while keeping
 * the LZ77 window: RFC 7692 sec 7.2.1 lets more blocks follow a final
 * one (on the next byte boundary), and sec 7.2.2 decompresses them and
 * later messages against the same window. zlib stops at a final block,
 * so save the window, reset, and load it back as the dictionary. */
static int pmd_inflate_restart(wf_pmd_stream *s) {
    Bytef *win = (Bytef *) malloc(32768);
    if (!win) {
        return Z_MEM_ERROR;
    }
    uInt n = 0;
    int rc = inflateGetDictionary(&s->zs, win, &n);
    if (rc == Z_OK) rc = inflateReset(&s->zs);
    if (rc == Z_OK) rc = inflateSetDictionary(&s->zs, win, n);
    free(win);
    return rc;
}

wf_pmd_stream *wf_pmd_inflate_new(int window_bits) {
    if (window_bits < 8 || window_bits > 15) {
        return NULL;
    }
    /* zlib does not actually support window_bits == 8 for inflate
     * even though RFC 7692 allows it on the wire; bump to 9.  This
     * matches what nginx and Chrome do internally. */
    if (window_bits == 8) {
        window_bits = 9;
    }

    wf_pmd_stream *s = (wf_pmd_stream *) calloc(1, sizeof(*s));
    if (!s) {
        return NULL;
    }
    s->kind = 0;

    int rc = inflateInit2(&s->zs, -window_bits);
    if (rc != Z_OK) {
        free(s);
        return NULL;
    }
    return s;
}

wf_pmd_stream *wf_pmd_deflate_new(int window_bits, int level, int mem_level) {
    if (window_bits < 8 || window_bits > 15) {
        return NULL;
    }
    if (window_bits == 8) {
        window_bits = 9;
    }
    if (mem_level < 1 || mem_level > 9) {
        mem_level = 8;
    }
    if (level < -1 || level > 9) {
        level = Z_DEFAULT_COMPRESSION;
    }

    wf_pmd_stream *s = (wf_pmd_stream *) calloc(1, sizeof(*s));
    if (!s) {
        return NULL;
    }
    s->kind = 1;

    int rc = deflateInit2(&s->zs,
                          level,
                          Z_DEFLATED,
                          -window_bits,
                          mem_level,
                          Z_DEFAULT_STRATEGY);
    if (rc != Z_OK) {
        free(s);
        return NULL;
    }
    return s;
}

void wf_pmd_free(wf_pmd_stream *s) {
    if (!s) return;
    if (s->kind == 0) {
        (void) inflateEnd(&s->zs);
    } else {
        (void) deflateEnd(&s->zs);
    }
    free(s);
}

int wf_pmd_inflate(wf_pmd_stream *s,
                   const uint8_t *src, size_t src_len,
                   uint8_t *dst, size_t dst_cap,
                   size_t *out_produced) {
    if (out_produced) *out_produced = 0;
    if (!s || s->kind != 0) {
        return WF_PMD_BAD_INPUT;
    }

    pmd_set_input(s, src, src_len);
    size_t out_left = dst_cap;
    s->zs.next_out  = (Bytef *) dst;
    s->zs.avail_out = 0;

    int rc;
    for (;;) {
        pmd_refill(s, &out_left);
        rc = inflate(&s->zs, Z_SYNC_FLUSH);
        if (rc == Z_STREAM_END) {
            /* A BFINAL = 1 block ended. At most one restart per message:
             * each costs two window copies, and a message of nothing but
             * tiny final blocks would otherwise buy ~32 KiB of copying
             * per input byte. */
            if (s->restarted) {
                rc = Z_DATA_ERROR;
                break;
            }
            s->restarted = 1;
            /* Sender omitted the 0x00 header octet of sec 7.2.3.4: only
             * the appended tail follows the final block. The unconsumed
             * input (avail_in, then in_left) is contiguous. */
            if ((size_t) s->zs.avail_in + s->in_left == sizeof pmd_tail
                && memcmp(s->zs.next_in, pmd_tail, sizeof pmd_tail) == 0) {
                s->zs.next_in += sizeof pmd_tail;
                s->zs.avail_in = 0;
                s->in_left = 0;
            }
            rc = pmd_inflate_restart(s);
            if (rc != Z_OK) {
                break;
            }
            if (s->zs.avail_in == 0 && s->in_left == 0) {
                rc = Z_STREAM_END;
                break;
            }
            continue;
        }
        if ((rc != Z_OK && rc != Z_BUF_ERROR) || !pmd_more(s, out_left)) {
            break;
        }
        /* Input gone and at a block boundary: the message is complete.
         * Calling inflate() again would move it past TYPE (it skips the
         * block-boundary state on entry) and lose data_type's bit 128. */
        if (s->zs.avail_in == 0 && s->in_left == 0 && (s->zs.data_type & 128)) {
            break;
        }
    }

    if (out_produced) {
        *out_produced = dst_cap - out_left - s->zs.avail_out;
    }

    if (rc == Z_STREAM_END) {
        return WF_PMD_OK;
    }
    if (rc == Z_OK || rc == Z_BUF_ERROR) {
        /* Every RFC 7692 message ends, once the tail is appended, with
         * an empty stored block: all input consumed and inflate waiting
         * for the next block header (data_type bit 128). Nothing can be
         * pending at a block boundary, even with a full buffer. */
        if (s->zs.avail_in == 0 && s->in_left == 0 && (s->zs.data_type & 128)) {
            return WF_PMD_OK;
        }
        /* Output full: more may be pending. The resume call either
         * produces it or, if the message ended exactly at the buffer's
         * end, completes with nothing. */
        if (s->zs.avail_out == 0) {
            return WF_PMD_NEEDS_MORE_OUT;
        }
        /* Input exhausted mid-block: truncated or malformed message. */
        return WF_PMD_BAD_INPUT;
    }
    return rc == Z_MEM_ERROR ? WF_PMD_OOM : WF_PMD_BAD_INPUT;
}

int wf_pmd_deflate(wf_pmd_stream *s,
                   const uint8_t *src, size_t src_len,
                   uint8_t *dst, size_t dst_cap,
                   size_t *out_produced) {
    if (out_produced) *out_produced = 0;
    if (!s || s->kind != 1) {
        return WF_PMD_BAD_INPUT;
    }

    pmd_set_input(s, src, src_len);
    size_t out_left = dst_cap;
    s->zs.next_out  = (Bytef *) dst;
    s->zs.avail_out = 0;

    int rc;
    for (;;) {
        pmd_refill(s, &out_left);
        /* Sync-flush once the whole message has been handed to zlib. */
        rc = deflate(&s->zs, s->in_left ? Z_NO_FLUSH : Z_SYNC_FLUSH);
        if (rc != Z_OK || !pmd_more(s, out_left)) {
            break;
        }
    }

    size_t produced = dst_cap - out_left - s->zs.avail_out;
    if (out_produced) {
        *out_produced = produced;
    }

    if (rc == Z_OK) {
        /* With room left the flush completed and all input was taken. */
        return s->zs.avail_out == 0 ? WF_PMD_NEEDS_MORE_OUT : WF_PMD_OK;
    }
    if (rc == Z_BUF_ERROR) {
        if (s->zs.avail_out == 0) {
            return WF_PMD_NEEDS_MORE_OUT;
        }
        /* deflate() had nothing to do: no input, nothing pending, and
         * the previous message's sync flush already completed (so the
         * bit stream is byte aligned). An empty message still needs a
         * payload (sec 7.2.3.6), so emit the empty stored block zlib
         * would have: header octet + 00 00 FF FF. */
        static const uint8_t empty_block[5] = { 0x00, 0x00, 0x00, 0xff, 0xff };
        if (dst_cap - produced < sizeof empty_block) {
            return WF_PMD_NEEDS_MORE_OUT;
        }
        memcpy(dst + produced, empty_block, sizeof empty_block);
        if (out_produced) {
            *out_produced = produced + sizeof empty_block;
        }
        return WF_PMD_OK;
    }
    return rc == Z_MEM_ERROR ? WF_PMD_OOM : WF_PMD_BAD_INPUT;
}

int wf_pmd_reset_inflate(wf_pmd_stream *s) {
    if (!s || s->kind != 0) return WF_PMD_BAD_INPUT;
    s->in_left = 0;
    s->restarted = 0;
    return inflateReset(&s->zs) == Z_OK ? WF_PMD_OK : WF_PMD_INIT_FAIL;
}

int wf_pmd_reset_deflate(wf_pmd_stream *s) {
    if (!s || s->kind != 1) return WF_PMD_BAD_INPUT;
    s->in_left = 0;
    return deflateReset(&s->zs) == Z_OK ? WF_PMD_OK : WF_PMD_INIT_FAIL;
}
