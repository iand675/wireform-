/*
 * failfns.c - see failfns.h. One translation unit per program (via
 * websocket-pmd_EXTRA) defines the fakes. The command-line renames
 * are undefined first, so the pass-through calls reach the real zlib
 * functions under their own names, while every other translation unit
 * (the kernel, the tests) keeps the renamed calls.
 */
#undef inflateInit2_
#undef deflateInit2_
#undef inflateGetDictionary
#undef inflateReset
#undef deflateReset
#undef deflate

#include "failfns.h"
#include "wf_fail.h"

/* The force-included headers above saw the command-line renames, so
 * they declared the fakes' names instead of the real library entry
 * points. Declare the real ones under their own names for the
 * pass-through calls. */
extern int inflateInit2_(z_streamp strm, int windowBits,
                         const char *version, int stream_size);
extern int deflateInit2_(z_streamp strm, int level, int method,
                         int windowBits, int memLevel, int strategy,
                         const char *version, int stream_size);
extern int inflateGetDictionary(z_streamp strm, Bytef *dictionary,
                                uInt *dictLength);
extern int inflateReset(z_streamp strm);
extern int deflateReset(z_streamp strm);
extern int deflate(z_streamp strm, int flush);

int wf_fail_inflateInit2_(z_streamp strm, int windowBits,
                          const char *version, int stream_size)
{
    if (wf_failpoint("inflateInit2")) return Z_MEM_ERROR;
    return inflateInit2_(strm, windowBits, version, stream_size);
}

int wf_fail_deflateInit2_(z_streamp strm, int level, int method,
                          int windowBits, int memLevel, int strategy,
                          const char *version, int stream_size)
{
    if (wf_failpoint("deflateInit2")) return Z_MEM_ERROR;
    return deflateInit2_(strm, level, method, windowBits, memLevel,
                         strategy, version, stream_size);
}

int wf_fail_inflateGetDictionary(z_streamp strm, Bytef *dictionary,
                                 uInt *dictLength)
{
    if (wf_failpoint("inflateGetDictionary")) return Z_STREAM_ERROR;
    return inflateGetDictionary(strm, dictionary, dictLength);
}

int wf_fail_inflateReset(z_streamp strm)
{
    if (wf_failpoint("inflateReset")) return Z_STREAM_ERROR;
    return inflateReset(strm);
}

int wf_fail_deflateReset(z_streamp strm)
{
    if (wf_failpoint("deflateReset")) return Z_STREAM_ERROR;
    return deflateReset(strm);
}

int wf_fail_deflate(z_streamp strm, int flush)
{
    if (wf_failpoint("deflate_zmem")) return Z_MEM_ERROR;
    if (wf_failpoint("deflate_zstream")) return Z_STREAM_ERROR;
    return deflate(strm, flush);
}
