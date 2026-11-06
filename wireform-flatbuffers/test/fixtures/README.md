`codec.bin` is generated output from official `flatc` 25.12.19, not Wireform.
Regenerate from the repository root, using the same compiler version:

```
flatc --binary --strict-json -o wireform-flatbuffers/test/fixtures \
  wireform-flatbuffers/test/fixtures/codec.fbs wireform-flatbuffers/test/fixtures/codec.json
```

The Haskell suite checks the decoded values and re-encodes them. For the reverse
interop check, run `python3 scripts/verify-flatbuffers-codec.py --encoded <buffer>`.
It generates C++ accessors, runs the official verifier on both buffers, and
compares the JSON produced by flatc, including default-valued fields.
