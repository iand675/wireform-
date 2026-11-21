{- |
RFC 9421 §5.1 @Accept-Signature@ — HTTP Message Signatures negotiation.

The @Accept-Signature@ field lets a sender request that the peer produce a
signature, naming a desired label and the components and signature
parameters that should cover the message. Its value mirrors the
@Signature-Input@ grammar exactly: an RFC 8941 Structured Field Dictionary
whose keys are signature labels and whose member values are Inner Lists of
component identifiers (Structured Field Strings with their own parameters)
carrying the requested signature parameters (@keyid@, @alg@, @created@,
@nonce@, @tag@, ...) as parameters on the Inner List.

@
Accept-Signature = sf-dictionary
                 ; label = inner-list( component-id );param=value ...
component-id     = sf-string *parameter
@

It is a request-and-response field: a client may advertise the signatures it
wants on a response, and a server may request signatures on subsequent
requests.

The @created@ and @expires@ parameters are Boolean flags requesting
that the signer generate timestamps. Component identifiers use the
same name grammar and parameter types as @Signature-Input@.

Spec: <https://www.rfc-editor.org/rfc/rfc9421.html#name-the-accept-signature-field>

See also: "Network.HTTP.Headers.Signature", "Network.HTTP.Headers.SignatureInput", "Network.HTTP.Headers.ContentDigest".
-}
module Network.HTTP.Headers.AcceptSignature (
  AcceptSignature (..),
  AcceptSignatureEntry (..),
  AcceptSignatureComponent (..),
  acceptSignatureParser,
  renderAcceptSignature,
) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text.Short (ShortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAcceptSignature)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R
import Network.HTTP.Headers.Signature.Internal (renderSignatureDictionary, renderSignatureInnerList, signatureInputDictionary, validSignatureInputEntry)


{- | A requested covered component identifier: an RFC 8941 String together
with any parameters that modify it (@;sf@, @;key=\"..\"@, @;req@, ...).
-}
data AcceptSignatureComponent = AcceptSignatureComponent
  { componentName :: !RFC8941String
  , componentParams :: ![(ShortText, Maybe ItemValue)]
  }
  deriving stock (Eq, Show)


-- | One dictionary member: a requested labelled signature and its parameters.
data AcceptSignatureEntry = AcceptSignatureEntry
  { signatureLabel :: !ShortText
  , signatureComponents :: ![AcceptSignatureComponent]
  , signatureParameters :: ![(ShortText, Maybe ItemValue)]
  }
  deriving stock (Eq, Show)


{- | The @Accept-Signature@ dictionary: an ordered, non-empty list of
requested labelled entries.
-}
newtype AcceptSignature = AcceptSignature
  { acceptSignatureEntries :: NonEmpty AcceptSignatureEntry
  }
  deriving stock (Eq, Show)


instance KnownHeader AcceptSignature where
  type ParseFailure AcceptSignature = String
  type Cardinality AcceptSignature = 'ZeroOrMore
  type Direction AcceptSignature = 'RequestAndResponse


  parseFromHeaders _ = parseStructuredFieldLines "Accept-Signature" acceptSignatureParser


  renderToHeaders _ = maybe [] (pure . M.toStrictByteString) . acceptSignatureBuilder


  headerName _ = hAcceptSignature


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

acceptSignatureParser :: ParserT st String AcceptSignature
acceptSignatureParser =
  AcceptSignature
    <$> signatureInputDictionary validTimestamp signatureLabel componentName componentParams AcceptSignatureComponent AcceptSignatureEntry


validTimestamp :: ItemValue -> Bool
validTimestamp (Boolean True) = True
validTimestamp _ = False
{-# INLINE validTimestamp #-}


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

-- | Render the field value. Invalid entries or repeated labels produce no bytes.
renderAcceptSignature :: AcceptSignature -> M.Builder
renderAcceptSignature = maybe mempty id . acceptSignatureBuilder


acceptSignatureBuilder :: AcceptSignature -> Maybe M.Builder
acceptSignatureBuilder (AcceptSignature entries) = renderSignatureDictionary signatureLabel validEntry renderEntry entries
  where
    validEntry (AcceptSignatureEntry _ components metadata) =
      validSignatureInputEntry validTimestamp componentName componentParams components metadata
    renderEntry (AcceptSignatureEntry label components metadata) =
      R.shortText label
        <> M.char7 '='
        <> renderSignatureInnerList componentName componentParams components metadata
