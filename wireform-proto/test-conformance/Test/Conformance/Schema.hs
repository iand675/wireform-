{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches -Wno-unused-top-binds -Wno-orphans -Wno-missing-signatures #-}

{- | All splice sites for the protobuf conformance harness.

This module centralises the @loadProto@ calls so the rest of
the harness can import the generated types without fighting
TH stage restrictions. Two schemas are spliced in:

  * @conformance.proto@ — the wire protocol the upstream
    @conformance_test_runner@ uses to drive any test program.
    Defines @ConformanceRequest@ \/ @ConformanceResponse@ \/
    @WireFormat@ \/ @TestStatus@ \/ @FailureSet@.

  * @test_messages_proto3.proto@ — a pruned copy of the
    upstream @TestAllTypesProto3@ schema. Carries every shape
    wireform-proto generates code for (scalars, repeated
    packed and unpacked, maps, oneofs, nested message and
    enum, foreign message and enum, recursive submessage).
    Well-known-types arms (Timestamp, Duration, Any, Struct,
    Wrappers, FieldMask, Empty, Value) are omitted because
    @loadProto@ doesn't currently follow proto @import@s; the
    wire-format round-trip still survives those fields via
    unknown-field preservation.

The two splices are kept on separate top-level pragmas so a
compile error in one doesn't blow up the entire module — the
error message points straight at the offending @.proto@ file.
-}
module Test.Conformance.Schema where

-- The 'extend' splice for proto2 emits qualified references
-- like 'Ext.ExtInt32' to the extension constructor types.

-- These imports look unused; in fact they bring 'Generic' /
-- 'NFData' / 'Hashable' into scope for the 'deriving anyclass'
-- the splice emits, plus the Vector / Map / ByteString types
-- the generated record fields use.
import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import Data.Int (Int32, Int64)
import Data.Map.Strict qualified as Map
import Data.Reflection (Given (..))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Proto.Extension qualified as Ext
-- The WKT splice paths in the test_messages_proto3.proto file
-- reference @Proto.Google.Protobuf.X@ types; those modules must
-- be in scope at the call site for GHC's renamer to resolve the
-- spliced 'ConT' references. We bring them in unqualified so the
-- spliced ConT names ('Proto.Google.Protobuf.Timestamp.Timestamp'
-- etc., as constructed by 'Proto.TH.lookupWkt') resolve.
import Proto.Google.Protobuf.Any (Any)
import Proto.Google.Protobuf.Duration (Duration)
import Proto.Google.Protobuf.Empty (Empty)
import Proto.Google.Protobuf.FieldMask (FieldMask)
import Proto.Google.Protobuf.Struct (
  ListValue,
  NullValue (..),
  Struct,
  Value,
 )
import Proto.Google.Protobuf.Timestamp (Timestamp)
import Proto.Google.Protobuf.Wrappers (
  BoolValue,
  BytesValue,
  DoubleValue,
  FloatValue,
  Int32Value,
  Int64Value,
  StringValue,
  UInt32Value,
  UInt64Value,
 )
import Proto.Internal.JSON.Extension (ExtensionRegistry, emptyExtensionRegistry)
import Proto.TH (loadProto)


-- Reference the imported types to keep GHC from stripping them
-- when -Wunused-imports is on.
_keepWkts
  :: ( Any
     , Duration
     , Empty
     , FieldMask
     , Struct
     , Value
     , ListValue
     , NullValue
     , Timestamp
     , BoolValue
     , Int32Value
     , Int64Value
     , UInt32Value
     , UInt64Value
     , FloatValue
     , DoubleValue
     , StringValue
     , BytesValue
     )
  -> ()
_keepWkts _ = ()


-- The conformance protos have no proto2 extensions, but the TH-generated
-- JSON instances carry a Given ExtensionRegistry constraint. Satisfy it
-- with an empty registry.
instance Given ExtensionRegistry where
  given = emptyExtensionRegistry


-- The wire protocol the upstream conformance_test_runner uses.
$(loadProto "test-conformance/protos/conformance.proto")


-- The TestAllTypesProto3 message schema (subset; see haddock above).
$(loadProto "test-conformance/protos/test_messages_proto3.proto")


-- The TestAllTypesProto2 message schema (also a pruned subset —
-- group syntax, message_set_wire_format, and required-fields-
-- only TestAllRequiredTypesProto2 are dropped because loadProto
-- doesn't support them yet; tests targeting those features
-- stay 'skipped' downstream).
$(loadProto "test-conformance/protos/test_messages_proto2.proto")


-- Force the generated types into the export list so a downstream
-- module can `import Test.Conformance.Schema` without naming any
-- type explicitly.
_keepImports :: (Map.Map Int Int, V.Vector Int, Seq.Seq Int)
_keepImports = (Map.empty, V.empty, Seq.empty)
