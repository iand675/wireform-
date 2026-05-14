{- | Primary interface for wireform-proto.

A single @import Proto@ gives you everything needed for everyday
protobuf use: encoding, decoding, schema introspection, the type
registry, and proto2 extension support.

== Quick start

@
import Proto

-- Serialise to a strict ByteString (exact-size, single allocation).
let bs = 'encodeMessage' myMsg

-- Deserialise; returns Either so errors are explicit.
case 'decodeMessage' bs of
  Left  err -> handleError err   -- 'DecodeError'
  Right msg -> use msg
@

== Encoding

'encodeMessage' is the common entry point.  It calls 'buildSized' on
the message (which generated types implement via 'MessageEncode') and
allocates a single 'Data.ByteString.ByteString' of exactly the right
length in one pass.

Variants:

* 'encodeLazy'                   — lazy @ByteString@ (chunked output)
* 'hPutMessage'                  — write to a @Handle@ without materialising
* 'hPutMessageLen'               — same, but also returns the byte count
* 'encodeMessageStream'          — length-delimited stream (list of messages → lazy @ByteString@)
* 'encodeMessageStreamSized'     — like above but avoids a materialisation step
* 'hPutMessageStream'            — stream directly to a @Handle@

For low-level access, 'buildMessage' returns a
@Wireform.Builder.Builder@ and 'messageSize' returns the wire byte
count without encoding.

== Decoding

'decodeMessage' parses a strict @ByteString@. For lazy input or
length-delimited streams see "Proto.Decode.Stream".

'decodeSubmessage' is identical to 'decodeMessage'; the name is a hint
that the bytes come from a submessage payload rather than a top-level
framing.

Error values: 'DecodeError' covers truncated input, invalid varints,
UTF-8 validation failures, negative length prefixes, and domain errors
from generated closed-enum decoders.

== Unknown field preservation

Generated types carry an @unknownFields@ slot. Any field numbers
absent from the @.proto@ schema are accumulated there as 'UnknownField'
values and re-emitted verbatim on encode, so messages survive a
round-trip through code compiled against an older schema version.
'encodeUnknownFields' produces the raw 'Data.ByteString.ByteString'
fragment for manual encoding pipelines.

== Schema metadata

Re-exports from "Proto.Schema": 'ProtoMessage', 'FieldDescriptor',
'protoFieldDescriptors', etc.  These are used by the dynamic decoder in
"Proto.Dynamic" and by generic tooling that needs to introspect message
shape at runtime.

== Generating message instances

There are two ways to obtain 'MessageEncode' \/ 'MessageDecode' instances
and schema metadata for your types.  Choose whichever fits your workflow:

=== 1. From a @.proto@ file — "Proto.TH"

@
{\-\# LANGUAGE TemplateHaskell \#-\}
import Proto.TH

\$(loadProto "proto\/person.proto")
@

'Proto.TH.loadProto' parses the @.proto@ file at compile time and
splices in Haskell record types, wire codecs, JSON instances, and
'Proto.Schema.ProtoMessage' metadata for every message and enum.
Use 'Proto.TH.loadProtoWith' with a 'Proto.TH.LoadOpts' value to
customise field naming, include paths, or lazy submessage decoding.

=== 2. From hand-written records — "Proto.TH.Derive"

If you already have a Haskell record and want to assign protobuf
field numbers and wire semantics to it:

@
{\-\# LANGUAGE TemplateHaskell \#-\}
import Proto.TH.Derive
import Wireform.Derive.Modifier

data Person = Person
  { personName :: !Text
  , personAge  :: !Word32
  } deriving stock (Show, Eq, Generic)

{\-\# ANN personName (tag 1) \#-\}
{\-\# ANN personAge  (tag 2) \#-\}

\$(deriveProto ''Person)
@

'Proto.TH.Derive.deriveProto' reads the @ANN@ pragmas and generates
the same instances as the @.proto@-file route.  Use
'Proto.TH.Derive.deriveProtoEncode' or
'Proto.TH.Derive.deriveProtoDecode' separately if you only need one
direction.

=== 3. Pure text code generator — "Proto.CodeGen"

For offline or build-system–driven generation, "Proto.CodeGen"
provides a pure @.proto@ → Haskell source pipeline.  The
@wireform-gen@ executable wraps this for use with @protoc@ or
custom build scripts.

== Lazy submessage decoding

'LazyMessage' lets you defer parsing a submessage until you actually
need it, which is useful in middleware that forwards messages without inspecting
every field. Opt in via @genLazySubmessages = True@ in
'Proto.CodeGen.GenerateOpts'; fields then have type @'LazyMessage' Foo@
instead of @Foo@. Call 'forceLazyMessage' to run the deferred parse.
See the 'LazyMessage' haddock for a full example and memory-retention
caveats.

-}
module Proto (
  -- * Encoding
  MessageEncode (..),
  encodeMessage,
  encodeLazy,
  hPutMessage,
  hPutMessageLen,
  buildMessage,
  messageSize,

  -- ** Length-delimited streams
  encodeMessageLazy,
  encodeMessageStream,
  encodeMessageStreamSized,
  hPutMessageStream,
  buildMessageFramed,

  -- * Decoding
  MessageDecode (..),
  decodeMessage,
  decodeSubmessage,

  -- * Errors
  DecodeError (..),

  -- * Lazy submessage decoding
  --
  -- | See 'LazyMessage' for when to use this and how to opt in.
  LazyMessage (..),
  forceLazyMessage,

  -- * Unknown field preservation
  --
  -- | Unrecognised fields round-trip automatically; 'encodeUnknownFields'
  -- is only needed in hand-written encode pipelines.
  UnknownField (..),
  encodeUnknownFields,

  -- * Schema metadata
  module Proto.Schema,

  -- * Type registry
  module Proto.Registry,

  -- * Proto2 extensions
  module Proto.Extension,
) where

import Proto.Internal.Decode
  ( DecodeError (..)
  , LazyMessage (..)
  , MessageDecode (..)
  , UnknownField (..)
  , decodeMessage
  , decodeSubmessage
  , encodeUnknownFields
  , forceLazyMessage
  )
import Proto.Internal.Encode
  ( MessageEncode (..)
  , buildMessage
  , buildMessageFramed
  , encodeMessage
  , encodeLazy
  , encodeMessageLazy
  , encodeMessageStream
  , encodeMessageStreamSized
  , hPutMessage
  , hPutMessageLen
  , hPutMessageStream
  , messageSize
  )
import Proto.Extension
import Proto.Registry
import Proto.Schema
