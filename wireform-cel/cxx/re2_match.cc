#include <cstddef>
#include <re2/re2.h>

// CEL uses UTF-8 RE2 substring matching. The slices can contain NUL bytes.
extern "C" int wireform_cel_re2_match(const char *text, std::size_t text_size,
                                      const char *pattern,
                                      std::size_t pattern_size) noexcept {
  try {
    RE2::Options options;
    options.set_log_errors(false);
    const RE2 regex(re2::StringPiece(pattern, pattern_size), options);
    if (!regex.ok()) {
      return -1;
    }
    return RE2::PartialMatch(re2::StringPiece(text, text_size), regex);
  } catch (...) {
    return -2; // C++ exceptions cannot cross the FFI boundary.
  }
}
