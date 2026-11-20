{-# LANGUAGE TemplateHaskell #-}

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

Spec: <https://www.rfc-editor.org/rfc/rfc9421.html#name-the-signature-http-field>

See also: "Network.HTTP.Headers.SignatureInput", "Network.HTTP.Headers.AcceptSignature", "Network.HTTP.Headers.ContentDigest", "Network.HTTP.Headers.Authorization".
-}
module Network.HTTP.Headers.Signature (
  Signature (..),
  signatureParser,
  renderSignature,
) where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.Text.Short (ShortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSignature)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R
import Network.HTTP.Headers.Signature.Internal (signatureDictionary)


{- | The @Signature@ dictionary: an ordered, non-empty mapping from a
signature label to the raw (base64-decoded) signature bytes.
-}
newtype Signature = Signature
  { signatureValues :: NonEmpty (ShortText, ByteString)
  }
  deriving stock (Eq, Show)


instance KnownHeader Signature where
  type ParseFailure Signature = String
  type Cardinality Signature = 'ZeroOrMore
  type Direction Signature = 'RequestAndResponse


  parseFromHeaders _ = parseStructuredFieldLines "Signature" signatureParser


  renderToHeaders _ = pure . M.toStrictByteString . renderSignature


  headerName _ = hSignature


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

renderSignature :: Signature -> M.Builder
renderSignature (Signature vs) =
  M.intersperse ", " (renderMember <$> vs)
  where
    renderMember (label, bytes) =
      R.shortText label <> M.char7 '=' <> R.rfc8941Binary bytes
