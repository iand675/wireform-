{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

{- |
RFC 9421 §4.2 @Signature@ — HTTP Message Signatures.

The @Signature@ field carries one or more message signatures and may
appear on both requests and responses. It is an RFC 8941 Structured
Field Dictionary whose keys are signature labels and whose member
values are Byte Sequences (@:base64:@) holding the raw signature bytes.
Each label matches a corresponding entry in the companion
@Signature-Input@ field. A label can appear only once across the field lines.

@
Signature = sf-dictionary    ; label = ":" base64(signature-bytes) ":"
@

We surface the dictionary structure directly: an ordered, non-empty list
of @(label, signature-bytes)@ pairs. The base64 wrapping is handled by the
shared RFC 8941 Byte Sequence machinery, so callers work with decoded bytes.
Use 'lookupMessageSignatures' or 'lookupMessageSignaturesWithTrailers' to
pair those bytes with their typed @Signature-Input@ entries.

Spec: <https://www.rfc-editor.org/rfc/rfc9421.html#name-the-signature-http-field>

See also: "Network.HTTP.Headers.SignatureInput", "Network.HTTP.Headers.AcceptSignature", "Network.HTTP.Headers.ContentDigest", "Network.HTTP.Headers.Authorization".
-}
module Network.HTTP.Headers.Signature (
  Signature (..),
  MessageSignature (..),
  MessageSignatureError (..),
  SignatureFieldSection (..),
  lookupMessageSignatures,
  lookupMessageSignaturesWithTrailers,
  signatureParser,
  renderSignature,
) where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Foldable (find)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import Data.Text.Short (ShortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (HeaderFieldName, hSignature, hSignatureInput)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R
import Network.HTTP.Headers.Signature.Internal (renderSignatureDictionary, signatureDictionary)
import Network.HTTP.Headers.SignatureInput (SignatureInput (..), SignatureInputEntry, signatureLabel)


{- | The @Signature@ dictionary: an ordered, non-empty mapping from a
signature label to the raw (base64-decoded) signature bytes.
-}
newtype Signature = Signature
  { signatureValues :: NonEmpty (ShortText, ByteString)
  }
  deriving stock (Eq, Show)


-- | A message signature's metadata and decoded bytes, paired by label.
data MessageSignature = MessageSignature
  { messageSignatureInput :: !SignatureInputEntry
  , messageSignatureBytes :: !ByteString
  }
  deriving stock (Eq, Show)


-- | Where a physical signature field was received.
data SignatureFieldSection = SignatureHeaderSection | SignatureTrailerSection
  deriving stock (Eq, Show)


-- | Failure to parse or pair a message's signature fields.
data MessageSignatureError
  = MalformedMessageSignatureField !SignatureFieldSection !HeaderFieldName !String
  | DuplicateMessageSignatureLabel !HeaderFieldName !ShortText
  | MissingMessageSignatureLabel !HeaderFieldName !ShortText
  deriving stock (Eq, Show)


instance KnownHeader Signature where
  type ParseFailure Signature = String
  type Cardinality Signature = 'ZeroOrMore
  type Direction Signature = 'RequestAndResponse


  parseFromHeaders _ = parseStructuredFieldLines "Signature" signatureParser


  renderToHeaders _ = maybe [] (pure . M.toStrictByteString) . signatureBuilder


  headerName _ = hSignature


-- | Look up and pair this message's signature fields by label, in
-- @Signature-Input@ order. Returns 'Nothing' for an unsigned message.
lookupMessageSignatures :: HeaderMap -> Either MessageSignatureError (Maybe (NonEmpty MessageSignature))
lookupMessageSignatures = lookupSignatureFields Nothing


-- | Pair signature fields once the complete message and its trailers
-- are available. Each section is parsed independently. Labels must be
-- unique within each field across both sections. Results follow the
-- @Signature-Input@ order, with header entries before trailer entries.
lookupMessageSignaturesWithTrailers
  :: HeaderMap -> HeaderMap -> Either MessageSignatureError (Maybe (NonEmpty MessageSignature))
lookupMessageSignaturesWithTrailers headers trailers = lookupSignatureFields (Just trailers) headers


lookupSignatureFields
  :: Maybe HeaderMap -> HeaderMap -> Either MessageSignatureError (Maybe (NonEmpty MessageSignature))
lookupSignatureFields trailers headers = do
  inputs <- lookupEntries signatureInputEntries signatureLabel
  values <- lookupEntries signatureValues fst
  case (inputs, values) of
    (Nothing, Nothing) -> Right Nothing
    (Just (input :| _), Nothing) -> missingValue input
    (Nothing, Just ((label, _) :| _)) -> Left (MissingMessageSignatureLabel hSignatureInput label)
    (Just (input :| []), Just ((label, bytes) :| []))
      | signatureLabel input == label -> Right (Just (MessageSignature input bytes :| []))
      | otherwise -> missingValue input
    (Just names, Just signatures) -> do
      let byLabel = Map.fromList (NE.toList signatures)
          pair input = case Map.lookup (signatureLabel input) byLabel of
            Just bytes -> Right (MessageSignature input bytes)
            Nothing -> missingValue input
      paired <- traverse pair names
      if NE.length names == NE.length signatures
        then Right (Just paired)
        else case Map.lookupMin (foldl' (\unused input -> Map.delete (signatureLabel input) unused) byLabel names) of
          Just (extra, _) -> Left (MissingMessageSignatureLabel hSignatureInput extra)
          Nothing -> Right (Just paired)
  where
    missingValue = Left . MissingMessageSignatureLabel hSignature . signatureLabel

    lookupEntries
      :: forall field entry
       . (KnownHeader field, ParseFailure field ~ String)
      => (field -> NonEmpty entry)
      -> (entry -> ShortText)
      -> Either MessageSignatureError (Maybe (NonEmpty entry))
    lookupEntries entries label = do
      initial <- readSection SignatureHeaderSection headers
      final <- maybe (Right Nothing) (readSection SignatureTrailerSection) trailers
      case (initial, final) of
        (Nothing, _) -> Right final
        (_, Nothing) -> Right initial
        (Just earlier, Just later) ->
          let earlierLabels = foldl' (\seen entry -> Set.insert (label entry) seen) Set.empty earlier
          in case find (\entry -> Set.member (label entry) earlierLabels) later of
            Just repeated -> Left (DuplicateMessageSignatureLabel name (label repeated))
            Nothing -> Right (Just (earlier <> later))
      where
        name = headerName (Proxy @field)
        readSection section fields =
          first (MalformedMessageSignatureField section name) (fmap entries <$> lookupHeader fields)


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

signatureParser :: ParserT st String Signature
signatureParser = Signature <$> signatureDictionary fst member
  where
    member label = do
      $(char '=')
      bytes <- rfc8941Binary
      skipStructuredFieldParameters
      pure (label, bytes)


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

-- | Render the field value. Invalid or repeated labels produce no bytes.
renderSignature :: Signature -> M.Builder
renderSignature = maybe mempty id . signatureBuilder


signatureBuilder :: Signature -> Maybe M.Builder
signatureBuilder (Signature values) = renderSignatureDictionary fst (const True) renderMember values
  where
    renderMember (label, bytes) =
      R.shortText label <> M.char7 '=' <> R.rfc8941Binary bytes
