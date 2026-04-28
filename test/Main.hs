module Main (main) where

import Test.Tasty

import Test.Parser (parserTests)
import Test.Wire (wireTests)
import Test.Roundtrip (roundtripTests)
import Test.CodeGen (codeGenTests)
import Test.WellKnown (wellKnownTests)
import Test.WellKnownUtil (wellKnownUtilTests)
import Test.PrintInspect (printInspectTests)
import Test.Compat (compatTests)
import Test.Schema (schemaTests)
import Test.Options (optionsTests)
import Test.Lens (lensTests)
import Test.StreamCodec (streamCodecTests)
import Test.JSON (jsonTests)
import Test.Hooks (hooksTests)
import Test.TDP (tdpTests)
import Test.Thrift (thriftTests)
import Test.Avro (avroTests)
import Test.GRPC (grpcTests)
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
import Test.EDN (ednTests)
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
import Test.Resolver (resolverTests)
import Test.CapnProtoParser (capnProtoParserTests)
import Test.FlatBuffersParser (flatBuffersParserTests)
import Test.ASN1Parser (asn1ParserTests)
import Test.CDDLParser (cddlParserTests)
import Test.ISLParser (islParserTests)
import Test.AvroIDL (avroIDLTests)
import Test.Registry (registryTests)
import Test.XML (xmlTests)
import Test.XMLSecurity (xmlSecurityTests)
import Test.Bencode (bencodeTests)
import Test.TOML (tomlTests)
import Test.HTML (htmlTests)
import Test.CSV (csvTests)
import Test.NDJSON (ndjsonTests)
import Test.ORC (orcTests)

main :: IO ()
main = defaultMain $ testGroup "wireform"
  [ parserTests
  , wireTests
  , roundtripTests
  , codeGenTests
  , wellKnownTests
  , wellKnownUtilTests
  , printInspectTests
  , compatTests
  , schemaTests
  , optionsTests
  , lensTests
  , streamCodecTests
  , jsonTests
  , hooksTests
  , tdpTests
  , avroTests
  , thriftTests
  , grpcTests
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
  , resolverTests
  , capnProtoParserTests
  , flatBuffersParserTests
  , asn1ParserTests
  , cddlParserTests
  , islParserTests
  , avroIDLTests
  , registryTests
  , xmlTests
  , xmlSecurityTests
  , bencodeTests
  , tomlTests
  , htmlTests
  , csvTests
  , ndjsonTests
  , orcTests
  ]
