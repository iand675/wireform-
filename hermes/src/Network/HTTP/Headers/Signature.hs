{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 9421 §4.2 @Signature@ — HTTP Message Signatures.

The @Signature@ field carries one or more message signatures and may
appear on both requests and responses. It is an RFC 8941 Structured
Field Dictionary whose keys are signature labels and whose member
values are Byte Sequences (@:base64:@) holding the raw signature bytes.
Each label matches a corresponding entry in the companion
@Signature-Input@ field.

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
import qualified Data.ByteString as B
import qualified Data.CharSet as CharSet
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text.Short (ShortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSignature)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


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


  parseFromHeaders _ headers =
    case runParser signatureParser (B.intercalate ", " (NE.toList headers)) of
      OK v rest
        | B.null (B.dropWhile isOwsByte rest) -> Right v
        | otherwise -> Left ("Unconsumed input after parsing Signature header: " <> show rest)
      Fail -> Left "Failed to parse Signature header"
      Err e -> Left e
    where
      isOwsByte w = w == 0x20 || w == 0x09


  renderToHeaders _ = pure . M.toStrictByteString . renderSignature


  headerName _ = hSignature


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

signatureParser :: ParserT st String Signature
signatureParser = do
  ows
  Signature <$> rfc8941List1 member
  where
    member = do
      label <- dictionaryKey
      $(char '=')
      bytes <- rfc8941Binary <* $(char ':')
      pure (label, bytes)


{- | An RFC 8941 dictionary @key@:
@( lcalpha \/ "*" ) *( lcalpha \/ DIGIT \/ "_" \/ "-" \/ "." \/ "*" )@.
-}
dictionaryKey :: ParserT st e ShortText
dictionaryKey =
  shortASCIIFromParser_ $
    skipSatisfyAscii (`CharSet.member` keyFirst)
      *> skipMany (skipSatisfyAscii (`CharSet.member` keyRest))
  where
    keyFirst = CharSet.range 'a' 'z' <> CharSet.singleton '*'
    keyRest = CharSet.range 'a' 'z' <> CharSet.range '0' '9' <> "_-.*"


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderSignature :: Signature -> M.Builder
renderSignature (Signature vs) =
  M.intersperse ", " (renderMember <$> vs)
  where
    renderMember (label, bytes) =
      R.shortText label <> M.char7 '=' <> R.rfc8941Binary bytes <> M.char7 ':'
