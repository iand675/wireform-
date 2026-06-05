module Main (main) where

import Test.Syd

-- Proto-specific tests moved to wireform-proto-test suite.
-- Cross-format integration tests remain here.
import Test.Thrift (thriftTests)
import Test.Avro (avroTests)
import Test.MsgPack (msgPackTests)
import Test.CBOR (cborTests)
import Test.BSON (bsonTests)
import Test.Ion (ionTests)
import Test.CapnProto (capnProtoTests)
import Test.FlatBuffers (flatBuffersTests)
import Test.Iceberg (icebergTests)
import Test.Bond (bondTests)
import Test.ASN1 (asn1Tests)
import Test.Parquet (parquetTests)
import Test.Arrow (arrowTests)
import Test.Arrow.FlatBufferProperties (flatBufferPropertyTests)
import Test.Arrow.Record (arrowRecordTests)
import Test.Arrow.RecordProperties (arrowRecordProperties)
import Test.Columnar (columnarFacadeTests)
import Test.Columnar.Properties (columnarPropertyTests)
import Test.EDN (ednTests)
import Test.EDI (ediTests)
import Test.MsgPackRPC (msgPackRPCTests)
import Test.CBORDiagnostic (cborDiagnosticTests)
import Test.Streaming (streamingTests)
import Test.Class (classTests)
import Test.AvroContainer (avroContainerTests)
import Test.ThriftParser (thriftParserTests)
import Test.AvroSchemaParse (avroSchemaParseTests)
import Test.BondParser (bondParserTests)
import Test.AvroCodeGen (avroCodeGenTests)
import Test.ThriftCodeGen (thriftCodeGenTests)
import Test.BondCodeGen (bondCodeGenTests)
import Test.CapnProtoCodeGen (capnProtoCodeGenTests)
import Test.FlatBuffersCodeGen (flatBuffersCodeGenTests)
import Test.ASN1CodeGen (asn1CodeGenTests)
import Test.CDDLCodeGen (cddlCodeGenTests)
import Test.ISLCodeGen (islCodeGenTests)
import Test.CapnProtoParser (capnProtoParserTests)
import Test.FlatBuffersParser (flatBuffersParserTests)
import Test.ASN1Parser (asn1ParserTests)
import Test.CDDLParser (cddlParserTests)
import Test.ISLParser (islParserTests)
import Test.AvroIDL (avroIDLTests)
import Test.Registry (registryTests)
import Test.XML (xmlTests)
import Test.Bencode (bencodeTests)
import Test.TOML (tomlTests)
import Test.HTML (htmlTests)
import Test.CSV (csvTests)
import Test.NDJSON (ndjsonTests)
import Test.ORC (orcTests)

main :: IO ()
main = sydTest $ describe "wireform" $ sequence_
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
  , ediTests
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
