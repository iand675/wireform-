{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 3230 @Digest@ — a comma-separated list of @instance-digest@
values, each pairing a digest-algorithm token with its
algorithm-specific encoded output (base64 for the @SHA-*@\/@MD5@
families, decimal for @UNIXsum@\/@UNIXcksum@, etc.). It may be sent on
requests and responses.

This field is /obsolete/: RFC 9530 deprecates it in favour of
'Network.HTTP.Headers.ContentDigest.ContentDigest' and
'Network.HTTP.Headers.ReprDigest.ReprDigest'. Because the encoded
output is algorithm-specific, the value is preserved verbatim as a
'ShortText' rather than decoded.

== Grammar

@
Digest          = #( instance-digest )
instance-digest = digest-algorithm \"=\" <encoded digest output>
@

Spec: <https://www.rfc-editor.org/rfc/rfc3230#section-4.3.2>

See also: "Network.HTTP.Headers.ContentDigest", "Network.HTTP.Headers.ReprDigest", "Network.HTTP.Headers.WantDigest", "Network.HTTP.Headers.ContentMD5".
-}
module Network.HTTP.Headers.Digest (
  Digest (..),
  DigestValue (..),
  digestParser,
  renderDigest,
) where

import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.CharSet as CharSet
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup (sconcat)
import Data.Text.Short (ShortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hDigest)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


{- | A single @instance-digest@: a digest-algorithm token and its
opaque, algorithm-specific encoded output (preserved verbatim).
-}
data DigestValue = DigestValue
  { digestAlgorithm :: !ShortText
  , digestEncoded :: !ShortText
  }
  deriving stock (Eq, Show)


-- | A non-empty list of instance digests.
newtype Digest = Digest {digestValues :: NonEmpty DigestValue}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup)


instance KnownHeader Digest where
  type ParseFailure Digest = String
  type Cardinality Digest = 'ZeroOrMore
  type Direction Digest = 'RequestAndResponse


  parseFromHeaders _ headers = sconcat <$> traverse runDigest headers
  renderToHeaders _ = pure . M.toStrictByteString . renderDigest
  headerName _ = hDigest


runDigest :: ByteString -> Either String Digest
runDigest bs = case runParser digestParser bs of
  OK v rest
    | B.null (dropOws rest) -> Right v
    | otherwise -> Left ("Unconsumed input after parsing Digest: " <> show rest)
  Fail -> Left "Failed to parse Digest header"
  Err e -> Left e
  where
    dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

digestParser :: ParserT st String Digest
digestParser = do
  ows
  Digest <$> (entry `sepBy1` (ows *> $(char ',') *> ows))
  where
    entry = do
      alg <- rfc9110Token
      $(char '=')
      val <- shortASCIIFromParser_ (skipSome (skipSatisfyAscii (`CharSet.member` encodedCharSet)))
      pure DigestValue {digestAlgorithm = alg, digestEncoded = val}
    -- base64 / base64url alphabet plus decimal — covers every
    -- algorithm registered for RFC 3230; terminates at OWS or a comma.
    encodedCharSet =
      CharSet.range 'A' 'Z' <> CharSet.range 'a' 'z' <> CharSet.range '0' '9' <> "+/=-_"


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderDigest :: Digest -> M.Builder
renderDigest = M.intersperse ", " . fmap renderEntry . digestValues
  where
    renderEntry (DigestValue alg val) = R.shortText alg <> M.char7 '=' <> R.shortText val
