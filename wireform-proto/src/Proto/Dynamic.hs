{- | Dynamic messages for runtime protobuf manipulation.

A 'DynamicMessage' can represent any protobuf message without
compile-time generated code, using field descriptors to interpret
the wire format at runtime.

== Quick start

@
import Proto.Dynamic

-- Schemaless decode (wire-type inference)
case 'decodeDynamic' rawBytes of
  Left err -> handleError err
  Right msg -> use ('dynamicField' 1 msg)

-- Schema-driven decode (faster, type-aware)
let table = 'compileParseTable' (Proxy @MyMsg)
case 'decodeDynamicWithSchema' table rawBytes of
  Left err -> handleError err
  Right msg -> use msg
@

== Note on module structure

The full schema-table internals ('FieldParser', 'ParseTable' record
fields, 'FieldThunk', thunk builders) live in
"Proto.Internal.Dynamic". That module is exposed for testing and
tooling that needs to construct custom parse tables; normal
application code should only call 'compileParseTable' and the
decode functions in this module.
-}
module Proto.Dynamic (
  -- * Dynamic value type
  DynamicValue (..),

  -- * Dynamic message
  DynamicMessage (..),
  emptyDynamic,
  dynamicField,
  setDynamicField,
  removeDynamicField,

  -- * Encoding / decoding
  encodeDynamic,
  decodeDynamic,

  -- * Streaming decode (length-delimited frames)
  decodeDynamicStream,
  decodeDynamicStreamLazy,

  -- * Schema-driven decoding (fast path)
  --
  -- | Build a parse table once from a 'Proto.Schema.ProtoMessage'
  -- schema and reuse it across many 'decodeDynamicWithSchema' calls.
  ParseTable,
  compileParseTable,
  decodeDynamicWithSchema,
  decodeDynamicStreamWithSchema,

  -- * Conversion
  dynamicToJson,
) where

import Proto.Internal.Dynamic (
  DynamicMessage (..),
  DynamicValue (..),
  ParseTable,
  compileParseTable,
  decodeDynamic,
  decodeDynamicStream,
  decodeDynamicStreamLazy,
  decodeDynamicStreamWithSchema,
  decodeDynamicWithSchema,
  dynamicToJson,
  emptyDynamic,
  encodeDynamic,
  dynamicField,
  removeDynamicField,
  setDynamicField,
 )
