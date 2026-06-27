---
title: Parser adapters
description: "Tiny bridges that let existing attoparsec, binary, and cereal parsers run on wireform-core's ChunkParser streaming surface."
sidebar:
  order: 5
---

`wireform-attoparsec`, `wireform-binary`, and `wireform-cereal` are
three tiny adapter packages. Each one bridges an existing incremental
parser library to wireform-core's `ChunkParser` streaming surface, so
you can reuse parsers you already have rather than rewriting them.

Each package exposes exactly one module and one function. They all
target the same target type: `ChunkParser`, defined in
`Wireform.Parser` (the `Wireform.Parser.Adapter` module the adapters
import lives in `wireform-core`). `ChunkParser` is the chunked-feeding
parser surface, "the lingua franca for incremental parser libraries
(attoparsec, binary, cereal, etc.)", that `runChunked` drives against a
transport.

| Package | Module | Function |
|---------|--------|----------|
| `wireform-attoparsec` | `Wireform.Parser.Adapter.Attoparsec` | `fromAttoparsec :: A.Parser a -> ChunkParser a` |
| `wireform-binary` | `Wireform.Parser.Adapter.Binary` | `fromBinary :: B.Get a -> ChunkParser a` |
| `wireform-cereal` | `Wireform.Parser.Adapter.Cereal` | `fromCereal :: S.Get a -> ChunkParser a` |

## wireform-attoparsec

`Wireform.Parser.Adapter.Attoparsec.fromAttoparsec` converts an
attoparsec `Data.Attoparsec.ByteString.Parser` into a `ChunkParser`. It
drives the parser's incremental `Partial` / `Done` / `Fail` results,
feeding chunks as they arrive and reporting how many bytes of each chunk
were consumed. The module's own doc-comment shows the intended usage:

```haskell
import Wireform.Parser.Adapter.Attoparsec (fromAttoparsec)
import qualified Data.Attoparsec.ByteString as A

myParser :: A.Parser MyType
myParser = ...

main = withRecvTransport cfg sock $ \t ->
  runChunked t ChunkCopy (fromAttoparsec myParser) >>= print
```

## wireform-binary

`Wireform.Parser.Adapter.Binary.fromBinary` converts a `binary`
`Data.Binary.Get.Get` decoder into a `ChunkParser`, built on
`runGetIncremental` / `pushChunk` / `pushEndOfInput`. Feed it through
`runChunked` exactly as with the attoparsec adapter.

## wireform-cereal

`Wireform.Parser.Adapter.Cereal.fromCereal` converts a `cereal`
`Data.Serialize.Get.Get` decoder into a `ChunkParser`, built on
`runGetPartial`. It behaves the same way as the other two adapters, so
an existing cereal `Get` can run on the wireform streaming surface
without modification.
