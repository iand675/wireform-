{-# LANGUAGE TemplateHaskell #-}

{- |
@Forwarded@ — request header by which proxies disclose the client-facing
information that would otherwise be lost or altered when a request passes
through intermediaries: the originating client (@for@), the interface a
proxy received the request on (@by@), the host the client targeted
(@host@), and the protocol used (@proto@). It is the IANA-registered,
standardized replacement for the de-facto @X-Forwarded-*@ family.

== Grammar (RFC 7239 §4)

@
Forwarded         = 1#forwarded-element
forwarded-element = [ forwarded-pair ] *( ";" [ forwarded-pair ] )
forwarded-pair    = token "=" value
value             = token / quoted-string
@

A value is a comma-separated list of forwarded-elements; each element
is a semicolon-separated list of @token=value@ pairs.  The four
registered parameters are @for@, @by@, @host@, and @proto@; the
@for@/@by@ node identifiers may be an IP address, the literal
@unknown@, or an obfuscated @_token@.  We do not interpret the node
syntax here: every pair is preserved verbatim, in document order
(including duplicate names), so the structure round-trips exactly and
callers can route the node values through their own logic.

Parameter names are case-insensitive per RFC 7239 §4; the on-the-wire
spelling is preserved so renderers can reproduce the original bytes.

Spec: <https://www.rfc-editor.org/rfc/rfc7239#section-4>

See also: "Network.HTTP.Headers.XForwardedFor",
"Network.HTTP.Headers.XForwardedHost",
"Network.HTTP.Headers.XForwardedProto",
"Network.HTTP.Headers.ProxyStatus", "Network.HTTP.Headers.Via".
-}
module Network.HTTP.Headers.Forwarded (
  Forwarded (..),
  ForwardedElement (..),
  ForwardedValue (..),
  forwardedParser,
  renderForwarded,
) where

import Control.Monad.Combinators (sepBy1)
import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NE
import Data.Text.Short (ShortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hForwarded)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- ---------------------------------------------------------------------------
-- Data
-- ---------------------------------------------------------------------------

{- | A @forwarded-pair@ value: either a bare @token@ or a
@quoted-string@ (stored unescaped).  The distinction is retained so
the original on-the-wire form can be reproduced.
-}
data ForwardedValue
  = ForwardedToken !ShortText
  | ForwardedQuoted !ShortText
  deriving stock (Eq, Show)


{- | A single @forwarded-element@: an ordered list of @token=value@
pairs (typically @for@\/@by@\/@host@\/@proto@), preserving order and
duplicates.
-}
newtype ForwardedElement = ForwardedElement
  { forwardedPairs :: [(ShortText, ForwardedValue)]
  }
  deriving stock (Eq, Show)


{- | The @Forwarded@ header value: an ordered list of
forwarded-elements, one per proxy hop closest-first.
-}
newtype Forwarded = Forwarded
  { forwardedElements :: [ForwardedElement]
  }
  deriving stock (Eq, Show)


instance KnownHeader Forwarded where
  type ParseFailure Forwarded = String
  type Cardinality Forwarded = 'ZeroOrMore
  type Direction Forwarded = 'Request


  parseFromHeaders _ headers = do
    elss <- traverse parseOne (NE.toList headers)
    pure (Forwarded (concat elss))
    where
      parseOne hdr = case runParser forwardedParser hdr of
        OK (Forwarded els) leftover
          | B.null (dropOws leftover) -> Right els
          | otherwise ->
              Left ("Unconsumed input after parsing Forwarded: " <> show leftover)
        Fail -> Left "Failed to parse Forwarded header"
        Err err -> Left err
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


  renderToHeaders _ fwd = [M.toStrictByteString (renderForwarded fwd)]


  headerName _ = hForwarded


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

-- | Parse a @Forwarded@ value: a comma-separated list of elements.
forwardedParser :: ParserT st String Forwarded
forwardedParser = do
  ows
  els <- forwardedElementParser `sepBy1` (ows *> $(char ',') *> ows)
  ows
  pure (Forwarded els)


-- | Parse a single @forwarded-element@: @forwarded-pair@s joined by @;@.
forwardedElementParser :: ParserT st String ForwardedElement
forwardedElementParser =
  ForwardedElement <$> (forwardedPairParser `sepBy1` (ows *> $(char ';') *> ows))


-- | Parse one @token "=" ( token / quoted-string )@ pair.
forwardedPairParser :: ParserT st String (ShortText, ForwardedValue)
forwardedPairParser = do
  name <- rfc9110Token
  bws
  $(char '=')
  bws
  val <-
    (ForwardedQuoted <$> quotedString)
      <|> (ForwardedToken <$> rfc9110Token)
  pure (name, val)


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderForwarded :: Forwarded -> M.Builder
renderForwarded (Forwarded els) =
  M.intersperse ", " (map renderElement els)


renderElement :: ForwardedElement -> M.Builder
renderElement (ForwardedElement pairs) =
  M.intersperse (M.char7 ';') (map renderPair pairs)


renderPair :: (ShortText, ForwardedValue) -> M.Builder
renderPair (name, val) =
  R.shortText name <> M.char7 '=' <> renderValue val


renderValue :: ForwardedValue -> M.Builder
renderValue = \case
  ForwardedToken t -> R.shortText t
  ForwardedQuoted t -> R.rfc8941String (RFC8941String t)
