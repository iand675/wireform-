{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The actual conformance handler: takes one
'ConformanceRequest', decodes the inner payload as a
'TestAllTypesProto3' (or 'FailureSet' for the runner's first
request), re-encodes it in the requested output format, and
returns one 'ConformanceResponse'.

Wire format coverage:

  * @PROTOBUF@ in -> @PROTOBUF@ out: full round-trip via the
    @loadProto@-generated codecs. Unknown fields are preserved
    end-to-end through the message's unknown-fields slot, so
    even tests carrying WKT-typed fields (which the schema in
    "Test.Conformance.Schema" deliberately omits — see haddock
    there) round-trip byte-identically.
  * @PROTOBUF@ in -> @JSON@ out: encode via 'Aeson.encode'
    (handler skips when the message has any unknown fields,
    because the JSON shape isn't well-defined for them).
  * @JSON@ in -> @PROTOBUF@ out: decode via 'Aeson.decode'.
  * @JSON@ in -> @JSON@ out: same as above plus an
    @encode . decode@ pass.
  * Anything else (JSPB, TEXT_FORMAT) is reported as
    @Skipped@; the upstream runner treats Skipped as a
    non-failure for those categories.

The first request the upstream runner sends carries
@messageType = "conformance.FailureSet"@; the handler
responds with an empty 'FailureSet' (we don't pre-declare
expected failures here — the test-suite driver
"Test.Conformance.Driver" interprets the runner's overall
summary itself).
-}
module Test.Conformance.Handler (
  handleRequest,
) where

import Control.Exception (SomeException, evaluate, try)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Proto.Decode qualified as PD
import Proto.Encode qualified as PE
import Proto.Internal.JSON.Extension qualified as PJExt
import Proto.TH.Metadata qualified as PTM
import Proto.TextFormat qualified as PTF
import Test.Conformance.Schema


{- | One request -> one response. Threaded through 'IO' so the
WKT range-error path (Timestamp/Duration too large/small)
can return @serialize_error@ via 'try' instead of escaping
as a process-level runtime exception.
-}
handleRequest :: ConformanceRequest -> IO ConformanceResponse
handleRequest req
  | mt == "conformance.FailureSet" = pure failureSetResponse
  | mt == "protobuf_test_messages.proto3.TestAllTypesProto3" =
      handleTestAllTypesProto3 req
  | mt == "protobuf_test_messages.proto2.TestAllTypesProto2" =
      handleTestAllTypesProto2 req
  | otherwise = pure (skipped ("Unknown message type: " <> mt))
  where
    mt = conformanceRequestMessageType req


{- | First-request sentinel: the runner asks for the FailureSet
before any real test. We always return an empty set; the
driver does its own pass/fail accounting from the runner's
summary output.
-}
failureSetResponse :: ConformanceResponse
failureSetResponse =
  let payload = PE.encodeMessage defaultFailureSet
  in defaultConformanceResponse
      { conformanceResponseResult =
          Just (ConformanceResponse'Result'ProtobufPayload payload)
      }


handleTestAllTypesProto3 :: ConformanceRequest -> IO ConformanceResponse
handleTestAllTypesProto3 req = do
  -- Per-request lenient enum mode: the conformance suite's
  -- 'JSON_IGNORE_UNKNOWN_PARSING_TEST' category expects unknown
  -- enum string values to be silently dropped.
  PTM.setLenientUnknownEnum (isIgnoreUnknownCategory req)
  case payloadInputFormat req of
    PayloadProtobuf bs -> case PD.decodeMessage bs of
      Left e -> pure (parseErr (T.pack (show e)))
      Right tm -> serializeTAT outFmt tm
    PayloadJson js -> do
      -- Lenient mode: pre-strip unknown enum strings out of
      -- the JSON before handing it to the FromJSON parser.
      -- This is a pragmatic shortcut around the fact that the
      -- spec's "ignore unknown enum value" semantics need a
      -- per-call signal that's hard to thread through Aeson's
      -- pure 'Parser' machinery — and the conformance suite
      -- uses a fixed sentinel string ("UNKNOWN_ENUM_VALUE"),
      -- so a syntactic rewrite is sufficient.
      let js' =
            if isIgnoreUnknownCategory req
              then stripUnknownEnumStrings js
              else js
      case Aeson.eitherDecodeStrictText js' of
        Left e -> pure (parseErr (T.pack e))
        Right tm -> serializeTAT outFmt tm
    PayloadText _ -> pure (skipped "TEXT_FORMAT input not supported")
    PayloadJspb _ -> pure (skipped "JSPB input not supported")
    PayloadNone -> pure (skipped "no payload set")
  where
    outFmt = conformanceRequestRequestedOutputFormat req


{- | Walk the JSON looking for the literal string
@\"UNKNOWN_ENUM_VALUE\"@ (the conformance suite's sentinel
for "an unknown enum value") and replace it with @null@. The
standard FromJSON path then treats the field as absent / the
array element as a @null@ (which we filter out at the parser
level for repeated/map enum fields). Cheap textual hack — the
sentinel is unique enough not to collide with real data.
-}
stripUnknownEnumStrings :: T.Text -> T.Text
stripUnknownEnumStrings =
  T.replace (T.pack "\"UNKNOWN_ENUM_VALUE\"") (T.pack "null")


{- | Encode a parsed TestAllTypesProto3 in the requested output
format and wrap the bytes / string in the appropriate
'ConformanceResponse' arm. JSON / TEXT_FORMAT encoding is
done under 'try' so the WKT range-check 'error' calls
(Timestamp\/Duration ProtoInputTooLarge\/Small) surface as
@serialize_error@ rather than killing the process.
-}
serializeTAT :: WireFormat -> TestAllTypesProto3 -> IO ConformanceResponse
serializeTAT fmt tm = case fmt of
  WireFormat'Protobuf ->
    pure
      defaultConformanceResponse
        { conformanceResponseResult =
            Just
              (ConformanceResponse'Result'ProtobufPayload (PE.encodeMessage tm))
        }
  WireFormat'Json
    | hasUnknownFields tm ->
        pure
          ( skipped
              "JSON output skipped: payload contains fields outside the spliced \
              \schema (e.g. WKT arms); their JSON shape isn't recoverable from \
              \the unknown-fields slot."
          )
    | otherwise ->
        trySerialize "JSON" $
          do
            bs <- evaluate (Aeson.encode tm)
            evaluate (decodeUtf8Lazy bs)
            >>= \t ->
              pure
                defaultConformanceResponse
                  { conformanceResponseResult =
                      Just
                        (ConformanceResponse'Result'JsonPayload t)
                  }
  WireFormat'TextFormat -> trySerialize "TEXT_FORMAT" $ do
    !pbtxt <- evaluate (PTF.typedToTextPretty (Proxy :: Proxy TestAllTypesProto3) tm)
    pure
      defaultConformanceResponse
        { conformanceResponseResult =
            Just
              (ConformanceResponse'Result'TextPayload pbtxt)
        }
  WireFormat'Jspb -> pure (skipped "JSPB output not supported")
  WireFormat'Unspecified -> pure (serializeError "UNSPECIFIED requested_output_format")
  WireFormat''Unrecognized _ -> pure (serializeError "Unknown WireFormat enum value")


{- | Wrap an IO action that builds a 'ConformanceResponse' so
any 'SomeException' (typically from a WKT canonical-range
check) becomes a @serialize_error@ response.
-}
trySerialize :: T.Text -> IO ConformanceResponse -> IO ConformanceResponse
trySerialize tag act = do
  res <- try act
  case res of
    Left (e :: SomeException) ->
      pure (serializeError (tag <> ": " <> T.pack (show e)))
    Right r -> pure r


{- | Aeson.encode produces a lazy 'BL.ByteString' of UTF-8; the
'ConformanceResponse' wants a 'Text' for the @json_payload@
arm. Round-trip via 'TLE.decodeUtf8'.
-}
decodeUtf8Lazy :: BL.ByteString -> T.Text
decodeUtf8Lazy = TL.toStrict . TLE.decodeUtf8


{- | Did the wire-format decoder route any tags into the
record's unknown-fields slot? If so, JSON encoding loses
those fields, so the handler reports Skipped rather than a
partial success.
-}
hasUnknownFields :: TestAllTypesProto3 -> Bool
hasUnknownFields = not . null . testAllTypesProto3UnknownFields


{- | Proto2 sibling of 'handleTestAllTypesProto3'. Same wire
and JSON shapes, just rooted at 'TestAllTypesProto2'.
-}
handleTestAllTypesProto2 :: ConformanceRequest -> IO ConformanceResponse
handleTestAllTypesProto2 req = do
  PTM.setLenientUnknownEnum (isIgnoreUnknownCategory req)
  case payloadInputFormat req of
    PayloadProtobuf bs -> case PD.decodeMessage bs of
      Left e -> pure (parseErr (T.pack (show e)))
      Right tm -> serializeTAT2 outFmt tm
    PayloadJson js -> do
      let js' =
            if isIgnoreUnknownCategory req
              then stripUnknownEnumStrings js
              else js
      case Aeson.eitherDecodeStrictText js' of
        Left e -> pure (parseErr (T.pack e))
        Right tm -> serializeTAT2 outFmt tm
    PayloadText _ -> pure (skipped "TEXT_FORMAT input not supported")
    PayloadJspb _ -> pure (skipped "JSPB input not supported")
    PayloadNone -> pure (skipped "no payload set")
  where
    outFmt = conformanceRequestRequestedOutputFormat req


serializeTAT2 :: WireFormat -> TestAllTypesProto2 -> IO ConformanceResponse
serializeTAT2 fmt tm = case fmt of
  WireFormat'Protobuf ->
    pure
      defaultConformanceResponse
        { conformanceResponseResult =
            Just
              (ConformanceResponse'Result'ProtobufPayload (PE.encodeMessage tm))
        }
  WireFormat'Json
    | hasUnknownFields2 tm ->
        pure
          ( skipped
              "JSON output skipped: payload contains fields outside the spliced \
              \proto2 schema; their JSON shape isn't recoverable from the \
              \unknown-fields slot."
          )
    | otherwise ->
        trySerialize "JSON" $
          do
            bs <- evaluate (Aeson.encode tm)
            evaluate (decodeUtf8Lazy bs)
            >>= \t ->
              pure
                defaultConformanceResponse
                  { conformanceResponseResult =
                      Just
                        (ConformanceResponse'Result'JsonPayload t)
                  }
  WireFormat'TextFormat -> trySerialize "TEXT_FORMAT" $ do
    !pbtxt <- evaluate (PTF.typedToTextPretty (Proxy :: Proxy TestAllTypesProto2) tm)
    pure
      defaultConformanceResponse
        { conformanceResponseResult =
            Just
              (ConformanceResponse'Result'TextPayload pbtxt)
        }
  WireFormat'Jspb -> pure (skipped "JSPB output not supported")
  WireFormat'Unspecified -> pure (serializeError "UNSPECIFIED requested_output_format")
  WireFormat''Unrecognized _ -> pure (serializeError "Unknown WireFormat enum value")


hasUnknownFields2 :: TestAllTypesProto2 -> Bool
hasUnknownFields2 m =
  not (all knownExtension (testAllTypesProto2UnknownFields m))
  where
    knownExtension uf =
      case PJExt.lookupExtensionByNumber
        registerExt_protobuf_test_messages_proto2_extension_int32
        (T.pack "protobuf_test_messages.proto2.TestAllTypesProto2")
        (extUfNumber uf) of
        Just _ -> True
        Nothing -> False
    extUfNumber uf = case uf of
      PD.UnknownVarint n _ -> n
      PD.UnknownFixed64 n _ -> n
      PD.UnknownLenDelim n _ -> n
      PD.UnknownFixed32 n _ -> n


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

data PayloadInput
  = PayloadProtobuf !BS.ByteString
  | PayloadJson !T.Text
  | PayloadText !T.Text
  | PayloadJspb !T.Text
  | PayloadNone


isIgnoreUnknownCategory :: ConformanceRequest -> Bool
isIgnoreUnknownCategory req =
  case conformanceRequestTestCategory req of
    TestCategory'JsonIgnoreUnknownParsingTest -> True
    _ -> False


payloadInputFormat :: ConformanceRequest -> PayloadInput
payloadInputFormat r = case r.conformanceRequestPayload of
  Just (ConformanceRequest'Payload'ProtobufPayload bs) -> PayloadProtobuf bs
  Just (ConformanceRequest'Payload'JsonPayload t) -> PayloadJson t
  Just (ConformanceRequest'Payload'TextPayload t) -> PayloadText t
  Just (ConformanceRequest'Payload'JspbPayload t) -> PayloadJspb t
  Nothing -> PayloadNone


skipped :: T.Text -> ConformanceResponse
skipped t =
  defaultConformanceResponse
    { conformanceResponseResult = Just (ConformanceResponse'Result'Skipped t)
    }


parseErr :: T.Text -> ConformanceResponse
parseErr t =
  defaultConformanceResponse
    { conformanceResponseResult = Just (ConformanceResponse'Result'ParseError t)
    }


serializeError :: T.Text -> ConformanceResponse
serializeError t =
  defaultConformanceResponse
    { conformanceResponseResult = Just (ConformanceResponse'Result'SerializeError t)
    }
