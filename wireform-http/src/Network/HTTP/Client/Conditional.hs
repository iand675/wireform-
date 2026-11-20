{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Conditional-request helpers (RFC 9110 \u00a713).

Builders for the @If-Match@, @If-None-Match@, @If-Modified-Since@,
@If-Unmodified-Since@, and @If-Range@ headers, plus a small ETag
parser \/ renderer.

The ETag and If-Match grammars are parsed and rendered by hermes
('Network.HTTP.Headers.ETag', 'Network.HTTP.Headers.IfMatch',
'Network.HTTP.Headers.IfNoneMatch'); we wrap them in builders that
produce raw header bytes plus combinators on 'Request'. This module
is the API surface application code uses; the heavy lifting (the
@token@ \/ @entity-tag@ grammar and the @M.Builder@ formatters)
lives in hermes.
-}
module Network.HTTP.Client.Conditional (
  -- * ETag values (re-exported from hermes)
  EntityTag (..),
  strongETag,
  weakETag,
  parseETag,
  renderETag,

  -- * Validator comparison (RFC 9110 §8.8.3)
  strongMatch,
  weakMatch,

  -- * If-Match / If-None-Match
  ifMatchHeader,
  ifNoneMatchHeader,
  ifMatchAny,
  ifNoneMatchAny,

  -- * Date-bearing precondition headers
  ifModifiedSinceHeader,
  ifUnmodifiedSinceHeader,

  -- * If-Range
  IfRange (..),
  ifRangeHeader,

  -- * Combinators on Request
  ifMatch,
  ifNoneMatch,
  ifModifiedSince,
  ifUnmodifiedSince,
  ifRange,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Short qualified as SBS
import Data.List.NonEmpty qualified as NE
import Data.Time (UTCTime)
import Network.HTTP.Client.Request (Request, setHeader)
import Network.HTTP.Headers.ETag (EntityTag (..), strongMatch, weakMatch)
import Network.HTTP.Headers.ETag qualified as Hermes
import Network.HTTP.Headers.IfRange qualified as HermesIfRange
import Network.HTTP.Headers.Mason qualified as M
import Network.HTTP.HttpDate (formatHttpDate)
import Network.HTTP.Types.Header qualified as H


-- ---------------------------------------------------------------------------
-- ETag
-- ---------------------------------------------------------------------------

{- | Build a strong entity tag from raw opaque bytes (without the
surrounding double quotes). Bytes that aren't valid @etagc@ will
be rejected on parse round-trip.
-}
strongETag :: ByteString -> EntityTag
strongETag = StrongETagBytes . SBS.toShort


-- | Build a weak entity tag (will render with the @W\/@ prefix).
weakETag :: ByteString -> EntityTag
weakETag = WeakETagBytes . SBS.toShort


-- | Render an entity tag to its on-the-wire form.
renderETag :: EntityTag -> ByteString
renderETag = M.toStrictByteString . Hermes.renderEntityTag


{- | Parse a single entity tag (strong or weak).  Wraps hermes's
'Hermes.parseETag' and projects out the entity-tag part.
-}
parseETag :: ByteString -> Maybe EntityTag
parseETag bs = case Hermes.parseETag bs of
  Right etag -> Just (Hermes.etag etag)
  Left _ -> Nothing


-- ---------------------------------------------------------------------------
-- If-Match / If-None-Match
-- ---------------------------------------------------------------------------

{- | Build an @If-Match@ value from a list of tags.  An empty list
collapses to the wildcard form (@\"*\"@); a non-empty list is
rendered as a comma-separated 'EntityTag' list, formatted via
hermes's 'Hermes.renderEntityTag'.
-}
ifMatchHeader :: [EntityTag] -> ByteString
ifMatchHeader tags = case NE.nonEmpty tags of
  Nothing -> ifMatchAny
  Just ne -> renderEntityTagList ne


ifNoneMatchHeader :: [EntityTag] -> ByteString
ifNoneMatchHeader tags = case NE.nonEmpty tags of
  Nothing -> ifNoneMatchAny
  Just ne -> renderEntityTagList ne


renderEntityTagList :: NE.NonEmpty EntityTag -> ByteString
renderEntityTagList ne =
  M.toStrictByteString (M.intersperse ", " (fmap Hermes.renderEntityTag ne))


{- | The wildcard @*@ value for both @If-Match@ and @If-None-Match@:
\"any current representation\".
-}
ifMatchAny :: ByteString
ifMatchAny = "*"


ifNoneMatchAny :: ByteString
ifNoneMatchAny = "*"


-- ---------------------------------------------------------------------------
-- If-Modified-Since / If-Unmodified-Since
-- ---------------------------------------------------------------------------

ifModifiedSinceHeader :: UTCTime -> ByteString
ifModifiedSinceHeader = formatHttpDate


ifUnmodifiedSinceHeader :: UTCTime -> ByteString
ifUnmodifiedSinceHeader = formatHttpDate


-- ---------------------------------------------------------------------------
-- If-Range
-- ---------------------------------------------------------------------------

{- | RFC 9110 \u00a713.1.5: @If-Range@ takes either an entity tag or an
HTTP-date. Sending it with a range header makes the server return
the partial response only when the validator still matches.
-}
data IfRange
  = IfRangeETag !EntityTag
  | IfRangeDate !UTCTime
  deriving stock (Eq, Show)


-- | Render an If-Range wire value. Use 'ifRange' to attach it to a request.
ifRangeHeader :: IfRange -> ByteString
ifRangeHeader value = M.toStrictByteString (HermesIfRange.renderIfRange validator)
  where
    validator = case value of
      IfRangeETag tag -> HermesIfRange.IfRangeETag tag
      IfRangeDate date -> HermesIfRange.IfRangeDate date


-- ---------------------------------------------------------------------------
-- Request combinators
-- ---------------------------------------------------------------------------

ifMatch :: [EntityTag] -> Request a -> Request a
ifMatch tags = setHeader H.hIfMatch (ifMatchHeader tags)


ifNoneMatch :: [EntityTag] -> Request a -> Request a
ifNoneMatch tags = setHeader H.hIfNoneMatch (ifNoneMatchHeader tags)


ifModifiedSince :: UTCTime -> Request a -> Request a
ifModifiedSince t = setHeader H.hIfModifiedSince (ifModifiedSinceHeader t)


ifUnmodifiedSince :: UTCTime -> Request a -> Request a
ifUnmodifiedSince t = setHeader H.hIfUnmodifiedSince (ifUnmodifiedSinceHeader t)


{- | Attach a validator for a request with a @Range@ field. The built-in
client transports omit both fields when the range or validator is unusable,
requesting the full representation. Supply a date only when there is no
entity tag for the representation and the date is known to be a strong
validator under RFC 9110 §8.8.2.2.
-}
ifRange :: IfRange -> Request a -> Request a
ifRange validator = setHeader H.hIfRange (ifRangeHeader validator)
