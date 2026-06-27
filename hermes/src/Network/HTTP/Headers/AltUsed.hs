{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 7838 §5 @Alt-Used@ — a request header sent by a client to
identify the alternative service it is actually using to reach the
origin.  It lets the alternative distinguish requests routed to it
from those made directly to the origin (and detect loops).

== Grammar (RFC 7838 §5)

@
Alt-Used = uri-host [ \":\" port ]
@

The value is a single @host@ with an optional @port@.  Bracketed
IPv6 literals (e.g. @[2001:db8::1]:443@) are preserved verbatim.

Spec: <https://www.rfc-editor.org/rfc/rfc7838#section-5>

See also: "Network.HTTP.Headers.AltSvc", "Network.HTTP.Headers.Host", "Network.HTTP.Headers.Via".
-}
module Network.HTTP.Headers.AltUsed (
  AltUsed (..),
  altUsedParser,
  renderAltUsed,
) where

import qualified Data.ByteString as B
import Data.CharSet (CharSet)
import qualified Data.CharSet as CharSet
import qualified Data.List.NonEmpty as NE
import Data.Text.Short (ShortText)
import Data.Word (Word16)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAltUsed)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- ---------------------------------------------------------------------------
-- Data
-- ---------------------------------------------------------------------------

-- | @uri-host [ \":\" port ]@.
data AltUsed = AltUsed
  { altUsedHost :: !ShortText
  -- ^ The host (bracketed for IPv6 literals).
  , altUsedPort :: !(Maybe Word16)
  -- ^ The port, when present.
  }
  deriving stock (Eq, Show)


instance KnownHeader AltUsed where
  type ParseFailure AltUsed = String
  type Cardinality AltUsed = 'ZeroOrOne
  type Direction AltUsed = 'Request


  parseFromHeaders _ headers = case runParser altUsedParser (NE.head headers) of
    OK v leftover
      | B.null (B.dropWhile isOwsByte leftover) -> Right v
      | otherwise -> Left ("Unconsumed input after parsing Alt-Used header: " <> show leftover)
    Fail -> Left "Failed to parse Alt-Used header"
    Err e -> Left e
    where
      isOwsByte w = w == 0x20 || w == 0x09


  renderToHeaders _ = M.toStrictByteString . renderAltUsed


  headerName _ = hAltUsed


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

altUsedParser :: ParserT st String AltUsed
altUsedParser = do
  ows
  host <- hostParser
  port <- optional ($(char ':') *> (fromIntegral <$> anyAsciiDecimalWord))
  pure AltUsed {altUsedHost = host, altUsedPort = port}


{- | @uri-host@: a bracketed IP-literal (captured with its brackets)
or a @reg-name@ \/ IPv4address (no colon).
-}
hostParser :: ParserT st e ShortText
hostParser = bracketed <|> regName
  where
    bracketed = shortASCIIFromParser_ $ do
      $(char '[')
      skipSome (skipSatisfyAscii (/= ']'))
      $(char ']')
    regName =
      shortASCIIFromParser_ $
        skipSome (skipSatisfyAscii (`CharSet.member` hostCharSet))


{- | Characters permitted in a non-bracketed @uri-host@: @unreserved@
plus @sub-delims@ (minus @,@) plus @%@.  Excludes @:@ so parsing
stops at the port separator.
-}
hostCharSet :: CharSet
hostCharSet =
  CharSet.range 'A' 'Z'
    <> CharSet.range 'a' 'z'
    <> CharSet.range '0' '9'
    <> "-._~%!$&'()*+;="


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderAltUsed :: AltUsed -> M.Builder
renderAltUsed (AltUsed host mPort) =
  R.shortText host <> maybe mempty (\p -> M.char7 ':' <> M.word16Dec p) mPort
