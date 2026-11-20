{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Error-/accumulating/ message decoding.

The primary 'Proto.Decode.decodeMessage' is fail-fast: it stops at the first
'DecodeError'. This module adds a diagnostic, schema-driven pass that, like
the validation library, collects /all/ recoverable problems in a message at
once — each with a field path — instead of surfacing only the first.

'decodeCollecting' returns the list of 'DecodeIssue's plus the strictly
decoded value when (and only when) the message is clean. It is built on the
message's 'Proto.Schema.ProtoMessage' field descriptors and shares checked
wire readers with the fail-fast decoder.

What it reports:

  * invalid UTF-8 in @string@ fields (each occurrence, with path/index);
  * structurally malformed (e.g. truncated) sub-messages and map entries;
  * malformed top-level wire data (bad varint, truncation, bad wire type).

Deep semantic checks inside sub-messages require the nested type's schema;
this pass validates each sub-message's framing one level down. Wire-type
mismatches on scalar fields are intentionally not flagged because packed
repeated fields make them ambiguous.
-}
module Proto.Decode.Collect (
  DecodeIssue (..),
  decodeCollecting,
  collectIssues,
) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as TB
import GHC.Generics (Generic)
import Proto.Decode (DecodeError (..), MessageDecode, decodeMessage)
import Proto.Internal.Wire (Tag (..), WireType (..), wireTypeToWord)
import Proto.Internal.Wire.Decode (DecodeResult (..), getLengthDelimited, getTag, runDecoder', skipTaggedWireType, validateUtf8)
import Proto.Schema (
  FieldDescriptor (..),
  FieldLabel' (..),
  FieldTypeDescriptor (..),
  OneofDescriptor (..),
  OneofFieldDescriptor (..),
  ProtoMessage (..),
  ScalarFieldType (..),
  SomeFieldDescriptor (..),
 )


{- | A single problem found while decoding, with a dotted/indexed field path
(empty for message-level / framing problems).
-}
data DecodeIssue = DecodeIssue
  { issuePath :: ![Text]
  , issueError :: !DecodeError
  }
  deriving stock (Eq, Show, Generic)


instance NFData DecodeIssue


{- | Decode a message, collecting every recoverable problem. The typed value is
@'Just' a@ exactly when the strict decoder also succeeds (i.e. the message is
clean); otherwise it is 'Nothing' and 'fst' enumerates the problems.
-}
decodeCollecting :: forall a. (ProtoMessage a, MessageDecode a) => ByteString -> ([DecodeIssue], Maybe a)
decodeCollecting bytes =
  let issues = collectIssues (Proxy :: Proxy a) bytes
  in case decodeMessage bytes :: Either DecodeError a of
       Right a -> (issues, Just a)
       Left e -> (if null issues then [DecodeIssue [] e] else issues, Nothing)


-- | Collect issues for a message type's wire bytes using its field schema.
collectIssues :: ProtoMessage a => Proxy a -> ByteString -> [DecodeIssue]
collectIssues proxy bytes =
  let (entries, mErr) = scan bytes
      fieldIssues = checkEntries (diagnosticFields proxy) entries
      tailIssue = maybe [] (\e -> [DecodeIssue [] e]) mErr
  in fieldIssues ++ tailIssue


data DiagnosticField = DiagnosticField !Text !FieldLabel' !FieldTypeDescriptor


diagnosticFields :: ProtoMessage a => Proxy a -> Map.Map Int DiagnosticField
diagnosticFields proxy =
  Map.union oneofFields (Map.mapMaybe ordinaryField (protoFieldDescriptors proxy))
  where
    groups = protoOneofDescriptors proxy
    carriers = Set.fromList (map odName groups)
    oneofFields = Map.fromList (concatMap (map member . odFields) groups)
    member arm = (ofdNumber arm, DiagnosticField (ofdName arm) LabelOptional (ofdType arm))
    ordinaryField (SomeField fd)
      | Set.member (fdName fd) carriers = Nothing
      | otherwise = Just (DiagnosticField (fdName fd) (fdLabel fd) (fdTypeDesc fd))


-- Only accepted length-delimited records contribute to an occurrence index.
checkEntries :: Map.Map Int DiagnosticField -> [(Int, WV)] -> [DecodeIssue]
checkEntries fields = go Map.empty
  where
    go _ [] = []
    go !counts ((fn, WLen body) : rest)
      | Just (DiagnosticField name label fieldType) <- Map.lookup fn fields
      , Just check <- payloadCheck fieldType =
          let idx = Map.findWithDefault (0 :: Int) fn counts
              repeated = label == LabelRepeated
              counts' = if repeated then Map.insert fn (idx + 1) counts else counts
              path = if repeated
                then name <> TL.toStrict (TB.toLazyText ("[" <> TB.decimal idx <> "]"))
                else name
          in case check body of
               Nothing -> go counts' rest
               Just err -> DecodeIssue [path] err : go counts' rest
    go counts (_ : rest) = go counts rest


payloadCheck :: FieldTypeDescriptor -> Maybe (ByteString -> Maybe DecodeError)
payloadCheck (ScalarType StringField) = Just $ \body ->
  if validateUtf8 body then Nothing else Just InvalidUtf8
payloadCheck (MessageType _) = Just (fmap SubMessageError . snd . scan)
payloadCheck (MapType _ _) = Just (fmap SubMessageError . snd . scan)
payloadCheck _ = Nothing


----------------------------------------------------------------------
-- Wire scanner
----------------------------------------------------------------------

data WV = WOther | WLen !ByteString


{- | Scan a message into @(fieldNumber, value)@ pairs, preserving repeats.
Returns any trailing structural error encountered.
-}
scan :: ByteString -> ([(Int, WV)], Maybe DecodeError)
scan bytes = go id 0
  where
    len = BS.length bytes
    go acc !off
      | off == len = (acc [], Nothing)
      | otherwise = case runDecoder' getTag bytes off of
          DecodeFail e -> (acc [], Just e)
          DecodeOK (Tag fn wt) start ->
            let value = case wt of
                  WireLengthDelimited -> WLen <$> getLengthDelimited
                  _ -> WOther <$ skipTaggedWireType fn (fromIntegral (wireTypeToWord wt))
            in case runDecoder' value bytes start of
                 DecodeOK entry end -> go (acc . ((fn, entry) :)) end
                 DecodeFail e -> (acc [], Just e)
