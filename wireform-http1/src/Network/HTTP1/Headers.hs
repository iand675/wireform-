{- | HTTP header fields (RFC 9110 § 5).

We represent a header block as a strict @[Header]@ list of
@(field-name, field-value)@ pairs. Field names compare case-insensitively
per RFC 9110 § 5.1, so all lookup helpers ('hLookup', 'hHas',
'hLookupAll') go through 'asciiIeq'. We preserve original casing on the
wire because it's a property HTTP clients sometimes test against.

Tight design notes:

* The list is the right structure here. HPACK-style index lookup is an
  HTTP\/2 thing; on HTTP\/1.x the typical block is 5-20 headers and a
  linear scan with SIMD-cased equality is faster than any hashtable.
* For specific framing headers (Content-Length, Transfer-Encoding, Host,
  Connection, Upgrade, Expect, Trailer) we expose @findContentLength@
  etc. that avoid the case-fold on each call site.
-}
module Network.HTTP1.Headers (
  -- * Types
  Header,
  HeaderName,
  HeaderValue,
  Headers,

  -- * Construction
  (=:),

  -- * Lookups
  hLookup,
  hLookupAll,
  hHas,

  -- * Framing-relevant lookups
  findContentLength,
  findTransferEncoding,
  findHost,
  findConnection,
  findExpect,
  expectsContinue,
  findUpgrade,
  findTrailer,

  -- * Connection-token parsing
  ConnectionOption (..),
  parseConnection,
  connectionOptions,

  -- * Case-insensitive helpers
  headerNameIeq,
) where

import Control.DeepSeq (NFData (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Network.HTTP.Headers.Expect qualified as Expect
import Network.HTTP1.Internal.Ascii (asciiIeq)


type HeaderName = ByteString


type HeaderValue = ByteString


type Header = (HeaderName, HeaderValue)


type Headers = [Header]


-- | Convenient construction: @\"content-type\" =: \"text\/plain\"@.
{-# INLINE (=:) #-}
(=:) :: HeaderName -> HeaderValue -> Header
n =: v = (n, v)


infixr 0 =:


-- | First-match case-insensitive lookup.
{-# INLINE hLookup #-}
hLookup :: HeaderName -> Headers -> Maybe HeaderValue
hLookup name = go
  where
    go [] = Nothing
    go ((n, v) : rest)
      | asciiIeq n name = Just v
      | otherwise = go rest


{- | All matching values for a header, in document order. Used to
implement RFC 9110 § 5.2 "combine duplicates with commas" semantics.
-}
hLookupAll :: HeaderName -> Headers -> [HeaderValue]
hLookupAll name = go
  where
    go [] = []
    go ((n, v) : rest)
      | asciiIeq n name = v : go rest
      | otherwise = go rest


{-# INLINE hHas #-}
hHas :: HeaderName -> Headers -> Bool
hHas name = go
  where
    go [] = False
    go ((n, _) : rest) = asciiIeq n name || go rest


{-# INLINE headerNameIeq #-}
headerNameIeq :: HeaderName -> HeaderName -> Bool
headerNameIeq = asciiIeq


------------------------------------------------------------------------
-- Framing-relevant lookups
------------------------------------------------------------------------

findContentLength :: Headers -> Maybe HeaderValue
findContentLength = hLookup "content-length"
{-# INLINE findContentLength #-}


findTransferEncoding :: Headers -> Maybe HeaderValue
findTransferEncoding = hLookup "transfer-encoding"
{-# INLINE findTransferEncoding #-}


findHost :: Headers -> Maybe HeaderValue
findHost = hLookup "host"
{-# INLINE findHost #-}


findConnection :: Headers -> Maybe HeaderValue
findConnection = hLookup "connection"
{-# INLINE findConnection #-}


findExpect :: Headers -> Maybe HeaderValue
findExpect = hLookup "expect"
{-# INLINE findExpect #-}


-- | Interpret every Expect field line as the combined expectation list.
expectsContinue :: Headers -> Bool
expectsContinue = Expect.hasContinueExpectation . hLookupAll "expect"
{-# INLINE expectsContinue #-}


findUpgrade :: Headers -> Maybe HeaderValue
findUpgrade = hLookup "upgrade"
{-# INLINE findUpgrade #-}


findTrailer :: Headers -> Maybe HeaderValue
findTrailer = hLookup "trailer"
{-# INLINE findTrailer #-}


------------------------------------------------------------------------
-- Connection: tokens
------------------------------------------------------------------------

{- | A parsed @Connection@ header token. We treat @close@ and
@keep-alive@ specially because they directly affect the persistent-
connection state machine (RFC 9112 § 9.3); everything else is a
generic option name that the application can match on.
-}
data ConnectionOption
  = ConnClose
  | ConnKeepAlive
  | ConnOption !ByteString
  deriving stock (Eq, Show)


instance NFData ConnectionOption where
  rnf ConnClose = ()
  rnf ConnKeepAlive = ()
  rnf (ConnOption bs) = rnf bs


{- | Parse a comma-separated @Connection@ header field-value into
'ConnectionOption' tokens.

@Connection: keep-alive, Upgrade, close@ becomes
@['ConnKeepAlive', 'ConnOption' \"upgrade\", 'ConnClose']@.

Token comparisons are case-insensitive; we lowercase via SIMD on the
way through.
-}
parseConnection :: HeaderValue -> [ConnectionOption]
parseConnection raw =
  let
    splitComma = BSC.split ','
    trim = BSC.dropWhile isSp . BSC.dropWhileEnd isSp
    isSp c = c == ' ' || c == '\t'
  in
    map classify (filter (not . BS.null) (map trim (splitComma raw)))
  where
    classify t
      | asciiIeq t "close" = ConnClose
      | asciiIeq t "keep-alive" = ConnKeepAlive
      | otherwise = ConnOption t


-- | Read connection options across all field lines in wire order.
connectionOptions :: Headers -> [ConnectionOption]
connectionOptions = concatMap parseConnection . hLookupAll "connection"
