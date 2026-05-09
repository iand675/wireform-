/*
 * Haskell FFI Wrapper for libregexp
 *
 * This wrapper provides a simplified interface for Haskell FFI bindings
 * and implements the required callbacks for libregexp.
 */

#include <stdlib.h>
#include <string.h>
#include "libregexp.h"
#include "utf8_utf16.h"

/* Callback implementations required by libregexp */

int lre_check_stack_overflow(void *opaque, size_t alloca_size) {
    /* For now, we don't check stack overflow.
     * Could be improved to check against a limit. */
    return 0;
}

int lre_check_timeout(void *opaque) {
    /* No timeout checking for now */
    return 0;
}

void *lre_realloc(void *opaque, void *ptr, size_t size) {
    if (size == 0) {
        free(ptr);
        return NULL;
    }
    return realloc(ptr, size);
}

/* Wrapper functions for easier FFI */

typedef struct {
    uint8_t *bytecode;
    int bytecode_len;
    int capture_count;
    int flags;
    char error_msg[256];
} ecma262_regex_t;

/* Compile a regular expression pattern */
ecma262_regex_t *ecma262_regex_compile(const char *pattern, size_t pattern_len, int flags) {
    ecma262_regex_t *regex = (ecma262_regex_t *)malloc(sizeof(ecma262_regex_t));
    if (!regex) return NULL;

    memset(regex, 0, sizeof(ecma262_regex_t));
    regex->flags = flags;

    regex->bytecode = lre_compile(&regex->bytecode_len, regex->error_msg,
                                   sizeof(regex->error_msg), pattern, pattern_len,
                                   flags, NULL);

    if (!regex->bytecode) {
        return regex; /* Return with error message */
    }

    regex->capture_count = lre_get_capture_count(regex->bytecode);
    return regex;
}

/* Free a compiled regex */
void ecma262_regex_free(ecma262_regex_t *regex) {
    if (regex) {
        if (regex->bytecode) {
            free(regex->bytecode);
        }
        free(regex);
    }
}

/* Get error message from compilation */
const char *ecma262_regex_get_error(ecma262_regex_t *regex) {
    return regex ? regex->error_msg : "NULL regex";
}

/* Get capture count */
int ecma262_regex_get_capture_count(ecma262_regex_t *regex) {
    return regex ? regex->capture_count : 0;
}

/* Get flags */
int ecma262_regex_get_flags(ecma262_regex_t *regex) {
    return regex ? regex->flags : 0;
}

/* Get group names (NULL-terminated string, or NULL if no named groups) */
const char *ecma262_regex_get_groupnames(ecma262_regex_t *regex) {
    if (!regex || !regex->bytecode) return NULL;
    return lre_get_groupnames(regex->bytecode);
}

/* Execute a match against UTF-8 subject
 * Returns:
 *   1 if match found
 *   0 if no match
 *   negative value on error
 *
 * Automatically uses UTF-16 mode when Unicode flag is set for proper
 * Unicode character handling.
 */
int ecma262_regex_exec(ecma262_regex_t *regex, const uint8_t *subject,
                       int start_index, int subject_len, uint8_t **captures) {
    if (!regex || !regex->bytecode) return -1;

    /* Check if Unicode mode is required */
    int re_flags = lre_get_flags(regex->bytecode);
    int use_unicode = (re_flags & (LRE_FLAG_UNICODE | LRE_FLAG_UNICODE_SETS)) != 0;

    if (!use_unicode) {
        /* Byte mode (cbuf_type=0): Fast path for non-Unicode patterns */
        return lre_exec(captures, regex->bytecode, subject, start_index,
                       subject_len, 0, NULL);
    }

    /* UTF-16 mode (cbuf_type=2): Required for Unicode patterns */

    /* Convert UTF-8 to UTF-16 */
    int utf16_len = 0;
    uint16_t *subject_utf16 = utf8_to_utf16(subject, subject_len, &utf16_len);
    if (!subject_utf16) {
        return -1;
    }

    /* Convert start index from UTF-8 byte offset to UTF-16 code unit offset */
    int start_index_utf16 = utf8_offset_to_utf16(subject, start_index);
    if (start_index_utf16 < 0) {
        free(subject_utf16);
        return -1;
    }

    /* Allocate capture array for UTF-16 pointers */
    int num_captures = regex->capture_count * 2;
    uint16_t **captures_utf16 = (uint16_t **)malloc(num_captures * sizeof(uint16_t *));
    if (!captures_utf16) {
        free(subject_utf16);
        return -1;
    }

    /* Execute with UTF-16 mode (cbuf_type=1, libregexp will upgrade to 2 if Unicode flag set) */
    int result = lre_exec((uint8_t **)captures_utf16, regex->bytecode,
                         (uint8_t *)subject_utf16, start_index_utf16,
                         utf16_len, 1, NULL);

    if (result == 1) {
        /* Match found - convert capture positions from UTF-16 offsets to UTF-8 byte offsets */
        for (int i = 0; i < num_captures; i += 2) {
            /* Calculate UTF-16 offsets relative to the start of subject_utf16 */
            int utf16_start = (int)(captures_utf16[i] - subject_utf16);
            int utf16_end = (int)(captures_utf16[i + 1] - subject_utf16);

            /* Convert to UTF-8 byte offsets */
            int utf8_start = utf16_offset_to_utf8(subject, subject_len, utf16_start);
            int utf8_end = utf16_offset_to_utf8(subject, subject_len, utf16_end);

            /* Store as pointers into the original UTF-8 subject */
            captures[i] = (uint8_t *)(subject + utf8_start);
            captures[i + 1] = (uint8_t *)(subject + utf8_end);
        }
    }

    free(captures_utf16);
    free(subject_utf16);

    return result;
}

/* Execute a match against UTF-16LE subject (for Unicode patterns)
 * Returns:
 *   1 if match found
 *   0 if no match
 *   negative value on error
 *
 * Note: subject should be UTF-16LE encoded (little-endian), and captures
 * will be UTF-16 code unit offsets (as pointers).
 */
int ecma262_regex_exec_utf16(ecma262_regex_t *regex, const uint16_t *subject,
                              int start_index, int subject_len, uint8_t **captures) {
    if (!regex || !regex->bytecode) return -1;

    /* Execute with UTF-16 mode (cbuf_type=1, libregexp will upgrade to 2 if Unicode flag set) */
    return lre_exec(captures, regex->bytecode, (const uint8_t *)subject,
                   start_index, subject_len, 1, NULL);
}

/* Check if compilation was successful */
int ecma262_regex_is_valid(ecma262_regex_t *regex) {
    return regex && regex->bytecode != NULL;
}
