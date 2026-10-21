module Main (main) where

-- Proto-specific tests moved to wireform-proto-test suite.
-- Cross-format integration tests remain here.

import Hedgehog.Internal.Config (resolveSkip)
import Test.ASN1 (asn1Tests)
import Test.ASN1CodeGen (asn1CodeGenTests)
import Test.ASN1Parser (asn1ParserTests)
import Test.Arrow (arrowTests)
import Test.Arrow.FlatBufferProperties (flatBufferPropertyTests)
import Test.Arrow.Record (arrowRecordTests)
import Test.Arrow.RecordProperties (arrowRecordProperties)
import Test.Avro (avroTests)
import Test.AvroCodeGen (avroCodeGenTests)
import Test.AvroContainer (avroContainerTests)
import Test.AvroIDL (avroIDLTests)
import Test.AvroSchemaParse (avroSchemaParseTests)
import Test.BSON (bsonTests)
import Test.Bencode (bencodeTests)
import Test.Bond (bondTests)
import Test.BondCodeGen (bondCodeGenTests)
import Test.BondParser (bondParserTests)
import Test.CBOR (cborTests)
import Test.CBORDiagnostic (cborDiagnosticTests)
import Test.CDDLCodeGen (cddlCodeGenTests)
import Test.CDDLParser (cddlParserTests)
import Test.CSV (csvTests)
import Test.CapnProto (capnProtoTests)
import Test.CapnProtoCodeGen (capnProtoCodeGenTests)
import Test.CapnProtoParser (capnProtoParserTests)
import Test.Class (classTests)
import Test.Columnar (columnarFacadeTests)
import Test.Columnar.Properties (columnarPropertyTests)
import Test.EDN (ednTests)
import Test.FlatBuffers (flatBuffersTests)
import Test.FlatBuffersCodeGen (flatBuffersCodeGenTests)
import Test.FlatBuffersParser (flatBuffersParserTests)
import Test.HTML (htmlTests)
import Test.ISLCodeGen (islCodeGenTests)
import Test.ISLParser (islParserTests)
import Test.Iceberg (icebergTests)
import Test.Ion (ionTests)
import Test.MsgPack (msgPackTests)
import Test.MsgPackRPC (msgPackRPCTests)
import Test.NDJSON (ndjsonTests)
import Test.ORC (orcTests)
import Test.Parquet (parquetTests)
import Test.Registry (registryTests)
import Test.Streaming (streamingTests)
import Test.Syd
import Test.TOML (tomlTests)
import Test.Thrift (thriftTests)
import Test.ThriftCodeGen (thriftCodeGenTests)
import Test.ThriftParser (thriftParserTests)
import Test.XML (xmlTests)


main :: IO ()
main = do
  -- Force Hedgehog's "HEDGEHOG_SKIP" CAF before sydtest starts its workers.
  -- Every property forces it first; when many capabilities hit it at once,
  -- GHC 9.8.4's RTS can lose the wakeup of a thread blocked on it, leaving
  -- one property hung until the per-test timeout.
  _ <- resolveSkip Nothing
  sydTest $
    describe "wireform" $
      sequence_
        [ avroTests
        , thriftTests
        , msgPackTests
        , cborTests
        , bsonTests
        , ionTests
        , capnProtoTests
        , flatBuffersTests
        , icebergTests
        , bondTests
        , asn1Tests
        , parquetTests
        , arrowTests
        , flatBufferPropertyTests
        , arrowRecordTests
        , arrowRecordProperties
        , columnarFacadeTests
        , columnarPropertyTests
        , ednTests
        , classTests
        , msgPackRPCTests
        , cborDiagnosticTests
        , streamingTests
        , avroContainerTests
        , thriftParserTests
        , avroSchemaParseTests
        , bondParserTests
        , avroCodeGenTests
        , thriftCodeGenTests
        , bondCodeGenTests
        , capnProtoCodeGenTests
        , flatBuffersCodeGenTests
        , asn1CodeGenTests
        , cddlCodeGenTests
        , islCodeGenTests
        , capnProtoParserTests
        , flatBuffersParserTests
        , asn1ParserTests
        , cddlParserTests
        , islParserTests
        , avroIDLTests
        , registryTests
        , xmlTests
        , bencodeTests
        , tomlTests
        , htmlTests
        , csvTests
        , ndjsonTests
        , orcTests
        ]
