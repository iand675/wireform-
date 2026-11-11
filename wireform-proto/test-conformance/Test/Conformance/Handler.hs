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
import Data.Aeson.Decoding qualified as JSON
import Data.Aeson.Decoding.Text (textToTokens)
import Data.Aeson.Decoding.Tokens (TkArray (..), TkRecord (..), Tokens (..))
import Data.Aeson.Types qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Proto.Decode qualified as PD
import Proto.Encode qualified as PE
import Proto.Extension qualified as Ext
import Proto.Internal.JSON qualified as PJ
import Proto.Internal.JSON.Extension qualified as PJExt
import Proto.Schema (ProtoMessage)
import Proto.TextFormat qualified as PTF
import Test.Conformance.EditionUnstable qualified as Unstable
import Test.Conformance.MigrationProto2 qualified as Migration2
import Test.Conformance.MigrationProto3 qualified as Migration3
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
      handleTestWithExtensions hasUnknownFields2 req
  | mt == "protobuf_test_messages.editions.TestAllTypesEdition2023" =
      handleTestWithExtensions hasUnknownEditionFields req
  | mt == "protobuf_test_messages.editions.proto2.TestAllTypesProto2" =
      handleTestWithExtensions (hasUnregisteredFields Migration2.migrationProto2Registry mt . Migration2.testAllTypesProto2UnknownFields) req
  | mt == "protobuf_test_messages.editions.proto3.TestAllTypesProto3" =
      handleTestWithExtensions (not . null . Migration3.testAllTypesProto3UnknownFields) req
  | mt == "protobuf_test_messages.edition_unstable.TestAllTypesEditionUnstable" =
      handleTestWithExtensions (hasUnregisteredFields Unstable.unstableExtensionRegistry mt . Unstable.testAllTypesEditionUnstableUnknownFields) req
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
handleTestAllTypesProto3 req =
  case payloadInputFormat req of
    PayloadProtobuf bs -> case PD.decodeMessage bs of
      Left e -> pure (parseErr (T.pack (show e)))
      Right tm -> serializeTAT req tm
    PayloadJson js -> case decodeConformanceJSON req js of
      Left e -> pure (parseErr (T.pack e))
      Right tm -> serializeTAT req tm
    PayloadText input -> case conformanceTextSchema >>= (\schema -> PTF.textToTyped schema input) of
      Left err -> pure (parseErr (T.pack err))
      Right tm -> serializeTAT req tm
    PayloadJspb _ -> pure (skipped "JSPB input not supported")
    PayloadNone -> pure (skipped "no payload set")


-- Inspect raw keys before Aeson constructs a KeyMap. This runner uses the
-- conformance suite's duplicate-rejection policy.
decodeConformanceJSON :: (ProtoMessage a, Aeson.FromJSON a) => ConformanceRequest -> T.Text -> Either String a
decodeConformanceJSON req input = do
  PJ.checkJSONNumberExponents (TE.encodeUtf8 input)
  let tokens = textToTokens input
  remaining <- checkValue tokens
  if T.all (\c -> c == ' ' || c == '\t' || c == '\r' || c == '\n') remaining
    then do
      (value, _) <- JSON.toEitherValue tokens
      let parser = if isIgnoreUnknownCategory req then PJ.parseJSONIgnoringUnknownEnumNames else Aeson.parseJSON
      Aeson.parseEither parser value
    else Left "Trailing data after JSON value"
  where
    checkValue :: Tokens k String -> Either String k
    checkValue (TkLit _ next) = Right next
    checkValue (TkText _ next) = Right next
    checkValue (TkNumber _ next) = Right next
    checkValue (TkArrayOpen items) = checkArray items
    checkValue (TkRecordOpen fields) = checkRecord Set.empty fields
    checkValue (TkErr err) = Left err

    checkArray :: TkArray k String -> Either String k
    checkArray (TkItem value) = checkValue value >>= checkArray
    checkArray (TkArrayEnd next) = Right next
    checkArray (TkArrayErr err) = Left err

    checkRecord :: Set.Set Aeson.Key -> TkRecord k String -> Either String k
    checkRecord seen (TkPair key value)
      | Set.member key seen = Left ("Duplicate JSON field: " <> show key)
      | otherwise = checkValue value >>= checkRecord (Set.insert key seen)
    checkRecord _ (TkRecordEnd next) = Right next
    checkRecord _ (TkRecordErr err) = Left err


{- | Encode a parsed TestAllTypesProto3 in the requested output
format and wrap the bytes / string in the appropriate
'ConformanceResponse' arm. JSON / TEXT_FORMAT encoding is
done under 'try' so the WKT range-check 'error' calls
(Timestamp\/Duration ProtoInputTooLarge\/Small) surface as
@serialize_error@ rather than killing the process.
-}
serializeTAT :: ConformanceRequest -> TestAllTypesProto3 -> IO ConformanceResponse
serializeTAT req tm = case conformanceRequestRequestedOutputFormat req of
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
  WireFormat'TextFormat -> serializeText req tm
  WireFormat'Jspb -> pure (skipped "JSPB output not supported")
  WireFormat'Unspecified -> pure (serializeError "UNSPECIFIED requested_output_format")
  WireFormat''Unrecognized _ -> pure (serializeError "Unknown WireFormat enum value")


serializeText :: (ProtoMessage a, PE.MessageEncode a) => ConformanceRequest -> a -> IO ConformanceResponse
serializeText req message = trySerialize "TEXT_FORMAT" $
  case conformanceTextSchema >>= (\schema -> PTF.typedToTextWithSchema schema (conformanceRequestPrintUnknownFields req) message) of
    Left err -> pure (serializeError (T.pack err))
    Right !text ->
      pure defaultConformanceResponse
        { conformanceResponseResult = Just (ConformanceResponse'Result'TextPayload text)
        }


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


handleTestWithExtensions
  :: (ProtoMessage a, PD.MessageDecode a, PE.MessageEncode a, Aeson.FromJSON a, Aeson.ToJSON a)
  => (a -> Bool) -> ConformanceRequest -> IO ConformanceResponse
handleTestWithExtensions hasUnknown req =
  case payloadInputFormat req of
    PayloadProtobuf bs -> case PD.decodeMessage bs of
      Left e -> pure (parseErr (T.pack (show e)))
      Right tm -> serializeWithExtensions hasUnknown req tm
    PayloadJson js -> case decodeConformanceJSON req js of
      Left e -> pure (parseErr (T.pack e))
      Right tm -> serializeWithExtensions hasUnknown req tm
    PayloadText input -> case conformanceTextSchema >>= (\schema -> PTF.textToTyped schema input) of
      Right tm -> serializeWithExtensions hasUnknown req tm
      Left err -> pure (parseErr (T.pack err))
    PayloadJspb _ -> pure (skipped "JSPB input not supported")
    PayloadNone -> pure (skipped "no payload set")


serializeWithExtensions
  :: (ProtoMessage a, PE.MessageEncode a, Aeson.ToJSON a)
  => (a -> Bool) -> ConformanceRequest -> a -> IO ConformanceResponse
serializeWithExtensions hasUnknown req tm = case conformanceRequestRequestedOutputFormat req of
  WireFormat'Protobuf ->
    pure
      defaultConformanceResponse
        { conformanceResponseResult =
            Just
              (ConformanceResponse'Result'ProtobufPayload (PE.encodeMessage tm))
        }
  WireFormat'Json
    | hasUnknown tm ->
        pure
          ( skipped
              "JSON output skipped: payload contains fields outside the spliced \
              \schema; their JSON shape isn't recoverable from the \
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
  WireFormat'TextFormat -> serializeText req tm
  WireFormat'Jspb -> pure (skipped "JSPB output not supported")
  WireFormat'Unspecified -> pure (serializeError "UNSPECIFIED requested_output_format")
  WireFormat''Unrecognized _ -> pure (serializeError "Unknown WireFormat enum value")


hasUnknownFields2 :: TestAllTypesProto2 -> Bool
hasUnknownFields2 =
  hasUnregisteredFields registerExt_protobuf_test_messages_proto2_extension_int32
    "protobuf_test_messages.proto2.TestAllTypesProto2" . testAllTypesProto2UnknownFields


hasUnknownEditionFields :: TestAllTypesEdition2023 -> Bool
hasUnknownEditionFields =
  hasUnregisteredFields editionExtensionRegistry "protobuf_test_messages.editions.TestAllTypesEdition2023"
    . testAllTypesEdition2023UnknownFields


hasUnregisteredFields :: PJExt.ExtensionRegistry -> T.Text -> [PD.UnknownField] -> Bool
hasUnregisteredFields registry parent = not . all knownExtension
  where
    knownExtension uf =
      case PJExt.lookupExtensionByNumber registry parent (Ext.unknownFieldNumber uf) of
        Just _ -> True
        Nothing -> False


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
