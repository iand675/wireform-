{- |
RFC 9421 §4.1 @Signature-Input@ — HTTP Message Signatures input.

The @Signature-Input@ field describes how each signature in the
companion @Signature@ field was produced, and may appear on both
requests and responses. It is an RFC 8941 Structured Field
Dictionary whose keys are signature labels and whose member values are
Inner Lists of /component identifiers/ (Structured Field Strings, each with
their own parameters such as @;sf@, @;key=\"..\"@, @;req@, @;bs@) carrying
the signature parameters (@created@, @keyid@, @alg@, @nonce@, @expires@,
@tag@, ...) as parameters on the Inner List itself.

@
Signature-Input = sf-dictionary
                ; label = inner-list( component-id );param=value ...
component-id    = sf-string *parameter
@

The component identifiers and the signature parameters are kept structurally
but faithfully: identifiers as their underlying Structured Field Strings and
parameters as the generic RFC 8941 @(key, Maybe value)@ pairs, so the full
set of parameters can be represented. Component and parameter order is
preserved in the canonical wire value. Signature labels and covered
component identifiers must be unique. The @created@ and @expires@
metadata parameters are integer timestamps. Component identifiers use
the name grammar and parameter types defined by RFC 9421.

Spec: <https://www.rfc-editor.org/rfc/rfc9421.html#name-the-signature-input-http-fi>

See also: "Network.HTTP.Headers.Signature", "Network.HTTP.Headers.AcceptSignature", "Network.HTTP.Headers.ContentDigest".
-}
module Network.HTTP.Headers.SignatureInput (
  SignatureInput (..),
  SignatureInputEntry (..),
  SignatureComponent (..),
  signatureInputParser,
  renderSignatureInput,
) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text.Short (ShortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSignatureInput)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R
import Network.HTTP.Headers.Signature.Internal (renderSignatureInnerList, signatureInputDictionary)


{- | A single covered component identifier: an RFC 8941 String (e.g.
@\"\@method\"@, @\"content-digest\"@) together with any parameters that
modify it (@;sf@, @;key=\"..\"@, @;req@, @;bs@, @;tr@, ...).
-}
data SignatureComponent = SignatureComponent
  { componentName :: !RFC8941String
  , componentParams :: ![(ShortText, Maybe ItemValue)]
  }
  deriving stock (Eq, Show)


-- | One dictionary member: a labelled signature and its derivation.
data SignatureInputEntry = SignatureInputEntry
  { signatureLabel :: !ShortText
  , signatureComponents :: ![SignatureComponent]
  , signatureParameters :: ![(ShortText, Maybe ItemValue)]
  }
  deriving stock (Eq, Show)


{- | The @Signature-Input@ dictionary: an ordered, non-empty list of
labelled entries.
-}
newtype SignatureInput = SignatureInput
  { signatureInputEntries :: NonEmpty SignatureInputEntry
  }
  deriving stock (Eq, Show)


instance KnownHeader SignatureInput where
  type ParseFailure SignatureInput = String
  type Cardinality SignatureInput = 'ZeroOrMore
  type Direction SignatureInput = 'RequestAndResponse


  parseFromHeaders _ = parseStructuredFieldLines "Signature-Input" signatureInputParser


  renderToHeaders _ = pure . M.toStrictByteString . renderSignatureInput


  headerName _ = hSignatureInput


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

signatureInputParser :: ParserT st String SignatureInput
signatureInputParser =
  SignatureInput
    <$> signatureInputDictionary validTimestamp signatureLabel componentName componentParams SignatureComponent SignatureInputEntry
  where
    validTimestamp (Integer _) = True
    validTimestamp _ = False


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderSignatureInput :: SignatureInput -> M.Builder
renderSignatureInput (SignatureInput entries) =
  M.intersperse ", " (renderEntry <$> entries)
  where
    renderEntry (SignatureInputEntry label components metadata) =
      R.shortText label
        <> M.char7 '='
        <> renderSignatureInnerList componentName componentParams components metadata
