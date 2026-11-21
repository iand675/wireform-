{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Field-name and field-value grammar from RFC 9110 \u00a75.1 \/ \u00a75.5.

The unified 'Network.HTTP.Types.Header.Headers' representation is a flat
'(CI ByteString, ByteString)' list and the smart inserters there are the
primary fast path; they don't validate, on purpose, because the hot path is
serialisation and a check on every @insertHeader@ would double the cost of
every header touch.

This module supplies the validating layer that should be used at the API
boundary \u2014 wherever bytes from the outside world become 'HeaderName' \/
'HeaderValue' values. The shipped middleware (auth, cookies, redirect, etc.)
sticks to constants that are already valid by construction; user code that
accepts header names \/ values from external input should run through one
of the @mk*@ smart constructors here.

This module is /below/ "Network.HTTP.Types.Header" in the import order so
that the validation logic can be shared between header construction and
the per-request validators in middleware.

= Grammar (RFC 9110)

* Field name: @token = 1*tchar@ where @tchar@ is any of
    @!#$%&'*+-.^_\`|~@ plus DIGIT and ALPHA.
* Field value: a sequence of @field-content@ which is @VCHAR \/ obs-text \/
    SP \/ HTAB@. CR (0x0D), LF (0x0A) and NUL (0x00) are explicitly
    forbidden \u2014 they're how header smuggling attacks land bytes the
    framer wasn't meant to see.

The H2 forbidden-header set (RFC 9113 \u00a78.2.2) is also defined here; it's
a subset of the hop-by-hop list with @Proxy-Connection@ added.
-}
module Network.HTTP.Internal.Validation (
  -- * Errors
  HeaderError (..),

  -- * Field-name / field-value grammar
  isTchar,
  isValidHeaderName,
  isValidHeaderValueByte,
  isValidHeaderValue,
  validateHeaderName,
  validateHeaderValue,
  mkHeaderName,
  mkHeaderValue,

  -- * Hop-by-hop and forbidden sets
  hopByHopHeaders,
  isHopByHop,
  stripHopByHop,
  http2ForbiddenHeaders,
  isHttp2Forbidden,
  validateHttp2Headers,

  -- * Byte-dependent metadata
  isBodyDigestField,
  equalShortBytes,
) where

import Control.Exception (Exception)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List (foldl')
import Data.Word (Word8)
import GHC.Exts (
  Addr#,
  Int (I#),
  Int#,
  eqAddr#,
  eqWord16#,
  eqWord32#,
  eqWord64#,
  eqWord8#,
  indexWord8OffAddr#,
  indexWord8OffAddrAsWord16#,
  indexWord8OffAddrAsWord32#,
  indexWord8OffAddrAsWord64#,
  isTrue#,
  (-#),
 )
import GHC.ForeignPtr (ForeignPtr (..), unsafeWithForeignPtr)
import GHC.Ptr (Ptr (..))


-- We avoid importing "Network.HTTP.Types.Header" so that header validation
-- is below the type module in the import order; the public facing alias
-- there ('HeaderName' = 'CI ByteString') is structurally identical.
type HName = CI ByteString


type HValue = ByteString


type HRow = (HName, HValue)


-- | A digest of the content or representation bytes, including legacy fields.
isBodyDigestField :: HName -> Bool
isBodyDigestField name = case CI.foldedCase name of
  "content-digest" -> True
  "repr-digest" -> True
  "content-md5" -> True
  "digest" -> True
  _ -> False
{-# INLINE isBodyDigestField #-}


-- | Compare short fields using unaligned loads while their buffers are alive.
equalShortBytes :: ByteString -> ByteString -> Bool
equalShortBytes
  left@(BSI.BS leftPtr@(ForeignPtr leftAddr _) size@(I# width))
  right@(BSI.BS rightPtr@(ForeignPtr rightAddr _) rightSize)
  | size /= rightSize = False
  | isTrue# (eqAddr# leftAddr rightAddr) = True
  | size > 16 = left == right
  | otherwise = BSI.accursedUnutterablePerformIO $
      unsafeWithForeignPtr leftPtr $ \(Ptr a) ->
        unsafeWithForeignPtr rightPtr $ \(Ptr b) ->
          pure $! equalWindow a b width
{-# INLINE equalShortBytes #-}


-- The first and last loads cover the window. They can overlap, but both
-- are bounded by its length. The caller establishes the 16-byte maximum.
equalWindow :: Addr# -> Addr# -> Int# -> Bool
equalWindow a b width
  | I# width >= 8 = same64 0# && same64 (width -# 8#)
  | I# width >= 4 = same32 0# && same32 (width -# 4#)
  | I# width >= 2 = same16 0# && same16 (width -# 2#)
  | I# width == 1 = isTrue# (eqWord8# (indexWord8OffAddr# a 0#) (indexWord8OffAddr# b 0#))
  | otherwise = True
  where
    same64 i = isTrue# (eqWord64# (indexWord8OffAddrAsWord64# a i) (indexWord8OffAddrAsWord64# b i))
    same32 i = isTrue# (eqWord32# (indexWord8OffAddrAsWord32# a i) (indexWord8OffAddrAsWord32# b i))
    same16 i = isTrue# (eqWord16# (indexWord8OffAddrAsWord16# a i) (indexWord8OffAddrAsWord16# b i))
{-# INLINE equalWindow #-}


-- | Errors raised by the validating smart constructors.
data HeaderError
  = HeaderNameEmpty
  | -- | A byte outside the @tchar@ alphabet appeared in a field name.
    HeaderNameInvalidByte !Word8
  | {- | A byte outside @field-value@ appeared in a field value
    (typically CR\/LF\/NUL).
    -}
    HeaderValueInvalidByte !Word8
  | {- | A connection-specific header name appeared in an HTTP\/2
    message, in violation of RFC 9113 \u00a78.2.2.
    -}
    HeaderForbiddenInH2 !HName
  deriving stock (Eq, Show)


instance Exception HeaderError


-- ---------------------------------------------------------------------------
-- Field-name grammar
-- ---------------------------------------------------------------------------

-- | RFC 9110 \u00a75.6.2 @tchar@: token characters.
isTchar :: Word8 -> Bool
isTchar w =
  (w >= 0x30 && w <= 0x39) -- 0-9
    || (w >= 0x41 && w <= 0x5A) -- A-Z
    || (w >= 0x61 && w <= 0x7A) -- a-z
    || w == 0x21 -- !
    || (w >= 0x23 && w <= 0x27) -- # $ % & '
    || w == 0x2A
    || w == 0x2B
    -- \* +
    || w == 0x2D
    || w == 0x2E -- - .
    || w == 0x5E
    || w == 0x5F
    -- \^ _
    || w == 0x60 -- `
    || w == 0x7C
    || w == 0x7E
-- \| ~
{-# INLINE isTchar #-}


-- | A field name is non-empty and entirely composed of @tchar@.
isValidHeaderName :: ByteString -> Bool
isValidHeaderName bs = not (BS.null bs) && BS.all isTchar bs


{- | RFC 9110 \u00a75.5 @field-value@ byte: VCHAR \/ obs-text \/ SP \/ HTAB.
Rejects CR (0x0D), LF (0x0A), and NUL (0x00) in particular, which are
the bytes that enable header smuggling.
-}
isValidHeaderValueByte :: Word8 -> Bool
isValidHeaderValueByte w =
  w == 0x09 -- HTAB
    || (w >= 0x20 && w <= 0x7E) -- VCHAR + SP
    || w >= 0x80 -- obs-text
{-# INLINE isValidHeaderValueByte #-}


-- | A field value contains only valid bytes (above).
isValidHeaderValue :: ByteString -> Bool
isValidHeaderValue = BS.all isValidHeaderValueByte


{- | Validating smart constructor for header names. Lower-cases the result
through 'CI.mk' so case-insensitive comparison is preserved.
-}
validateHeaderName :: ByteString -> Either HeaderError HName
validateHeaderName bs
  | BS.null bs = Left HeaderNameEmpty
  | otherwise = case BS.find (not . isTchar) bs of
      Nothing -> Right (CI.mk bs)
      Just w -> Left (HeaderNameInvalidByte w)


validateHeaderValue :: ByteString -> Either HeaderError HValue
validateHeaderValue bs = case BS.find (not . isValidHeaderValueByte) bs of
  Nothing -> Right bs
  Just w -> Left (HeaderValueInvalidByte w)


-- | Alias for 'validateHeaderName'.
mkHeaderName :: ByteString -> Either HeaderError HName
mkHeaderName = validateHeaderName


mkHeaderValue :: ByteString -> Either HeaderError HValue
mkHeaderValue = validateHeaderValue


-- ---------------------------------------------------------------------------
-- Hop-by-hop headers (RFC 9110 \u00a77.6.1, RFC 7230 \u00a76.1)
-- ---------------------------------------------------------------------------

{- | Headers scoped to a single connection; MUST NOT be forwarded by
intermediaries. RFC 9110 \u00a77.6.1 plus the legacy entries preserved
from RFC 7230 \u00a76.1 (Keep-Alive, Proxy-Authenticate,
Proxy-Authorization).
-}
hopByHopHeaders :: HashSet HName
hopByHopHeaders =
  HS.fromList
    [ CI.mk "Connection"
    , CI.mk "Keep-Alive"
    , CI.mk "Proxy-Authenticate"
    , CI.mk "Proxy-Authorization"
    , CI.mk "TE"
    , CI.mk "Trailer"
    , CI.mk "Transfer-Encoding"
    , CI.mk "Upgrade"
    ]


isHopByHop :: HName -> Bool
isHopByHop = (`HS.member` hopByHopHeaders)


{- | Drop hop-by-hop headers, /and/ any header named in a @Connection:@
field of the same message (RFC 9110 \u00a77.6.1: \"Connection enumerates
the field names of headers that are scoped to this connection only\").
-}
stripHopByHop :: [HRow] -> [HRow]
stripHopByHop hdrs = filter (not . (`HS.member` banned) . fst) hdrs
  where
    banned = foldl' addConnectionOptions hopByHopHeaders hdrs
    addConnectionOptions names (name, value)
      | name == connection = foldl' addOption names (BS.split 0x2C value)
      | otherwise = names
    addOption names option
      | BS.null stripped = names
      | otherwise = HS.insert (CI.mk stripped) names
      where
        stripped = BS.dropWhile isWS (BS.dropWhileEnd isWS option)
    isWS w = w == 0x20 || w == 0x09
    connection = CI.mk "Connection"


-- ---------------------------------------------------------------------------
-- HTTP/2 forbidden headers (RFC 9113 \u00a78.2.2)
-- ---------------------------------------------------------------------------

{- | Connection-specific headers that MUST NOT appear in HTTP\/2 messages.
The single exception is @TE: trailers@, which is allowed; we filter that
specifically in 'validateHttp2Headers'.
-}
http2ForbiddenHeaders :: HashSet HName
http2ForbiddenHeaders =
  HS.fromList
    [ CI.mk "Connection"
    , CI.mk "Keep-Alive"
    , CI.mk "Proxy-Connection"
    , CI.mk "Transfer-Encoding"
    , CI.mk "Upgrade"
    ]


isHttp2Forbidden :: HName -> Bool
isHttp2Forbidden = (`HS.member` http2ForbiddenHeaders)


{- | Validate a header list against the HTTP\/2 forbidden-header rule.
Returns @Left@ on the first offending header. @TE@ is permitted only
with the value @trailers@.
-}
validateHttp2Headers :: [HRow] -> Either HeaderError ()
validateHttp2Headers = go
  where
    teName = CI.mk "TE"
    go [] = Right ()
    go ((n, v) : rest)
      | HS.member n http2ForbiddenHeaders =
          Left (HeaderForbiddenInH2 n)
      | n == teName && BS.map toLower8 v /= "trailers" =
          Left (HeaderForbiddenInH2 n)
      | otherwise = go rest
    toLower8 w
      | w >= 0x41 && w <= 0x5A = w + 0x20
      | otherwise = w
