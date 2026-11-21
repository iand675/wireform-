{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | The 'Request' type and its smart constructors.

A 'Request' is a value-level description of a request. It has a
phantom-typed body slot ('body'); strict 'ByteString' is the
fully-materialised case, 'BodyStream' is the streaming case, '()'
or 'Void' for no body. The shipped media-type tags (JSON,
form-urlencoded, ...) attach a body via 'withBody'.

The 'Request' itself carries no execution policy. Timeouts,
retries, redirect counts, proxies are middleware concerns.

@
get [uri|\/users\/{userId}|]
  & bindVar "userId" (42 :: Int)
  & setHeader hAccept "application/json"
  & withBody \@JSON newUser
@
-}
module Network.HTTP.Client.Request (
  -- * The Request type
  Request (..),

  -- * Constructors
  request,
  get,
  post,
  put,
  delete,
  patch,
  options,
  head_,
  setMethod,

  -- * Modifiers
  setHeader,
  addHeader,
  removeHeader,
  addRequestHeader,
  withRequestTrailers,
  withoutRequestTrailers,
  RequestTrailerError (..),
  withBody,
  withRawBody,
  addSpanAttribute,
  addProtocolHint,
  mapBody,

  -- * Span attributes
  SpanAttribute (..),
) where

import Control.Exception (Exception, throwIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.CaseInsensitive qualified as CI
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Network.HTTP.Client.Media (Encode, encode, mediaType, renderMediaType)
import Network.HTTP.Client.Protocol
import Network.HTTP.Client.URI
import Network.HTTP.Headers.Trailer (isForbiddenTrailerName)
import Network.HTTP.Types.Header qualified as H
import Network.HTTP.Types.Method qualified as M
import Network.HTTP.Types.Version (Version)


{- | A value tag for tracing attributes.

Kept open by way of a small sum type instead of an existential —
that way assertions, VCR sanitizers, and mock transports can pattern
match on attribute values without dynamic typing.
-}
data SpanAttribute
  = AttrText !Text
  | AttrBytes !ByteString
  | AttrInt !Int64
  | AttrDouble !Double
  | AttrBool !Bool
  deriving stock (Eq, Show)


{- | A request is a pure description. The 'body' parameter determines
the serialization mode; see "Network.HTTP.Client.Body".
-}
data Request body = Request
  { method :: !M.Method
  , requestURI :: !RequestURI
  , headers :: !H.Headers
  , body :: !body
  , protocolHints :: !ProtocolHints
  , spanAttributes :: ![(Text, SpanAttribute)]
  }


instance Functor Request where
  fmap f r = r {body = f (body r)}


instance Show body => Show (Request body) where
  show r =
    "Request "
      <> show (method r)
      <> " "
      <> show (requestURI r)
      <> " "
      <> show (headers r)
      <> " "
      <> show (body r)


{- | Build a request explicitly from its parts. Mostly used by the
shorthand constructors below.
-}
request :: M.Method -> RequestURI -> body -> Request body
request m u b =
  Request
    { method = m
    , requestURI = u
    , headers = []
    , body = b
    , protocolHints = defaultHints
    , spanAttributes = []
    }


get, head_, options :: UriTemplate -> Request ()
get t = request M.mGet (templateURI t) ()
head_ t = request M.mHead (templateURI t) ()
options t = request M.mOptions (templateURI t) ()


{- | Body-bearing constructors start with no body. Use 'withBody' to
attach one. The phantom type stays at @()@ until then so callers
can't forget to set the body.
-}
post, put, patch, delete :: UriTemplate -> Request ()
post t = request M.mPost (templateURI t) ()
put t = request M.mPut (templateURI t) ()
patch t = request M.mPatch (templateURI t) ()
delete t = request M.mDelete (templateURI t) ()


-- | Replace the method of a request.
setMethod :: M.Method -> Request a -> Request a
setMethod m r = r {method = m}


{- | Replace any existing entry for a header name. To allow duplicates
(e.g. @Set-Cookie@), use 'addRequestHeader'.
-}
setHeader :: H.HeaderName -> H.HeaderValue -> Request a -> Request a
setHeader n v r = r {headers = H.insertHeader n v (headers r)}


{- | Append a header without overwriting existing entries with the
same name.
-}
addRequestHeader :: H.HeaderName -> H.HeaderValue -> Request a -> Request a
addRequestHeader n v r = r {headers = H.addHeader n v (headers r)}


-- | Alias for 'addRequestHeader' (keeps the spec's name).
addHeader :: H.HeaderName -> H.HeaderValue -> Request a -> Request a
addHeader = addRequestHeader


removeHeader :: H.HeaderName -> Request a -> Request a
removeHeader n r = r {headers = H.deleteHeader n (headers r)}


data RequestTrailerError
  = InvalidRequestTrailerName !H.HeaderName
  | InvalidRequestTrailerValue !H.HeaderName
  | ForbiddenRequestTrailer !H.HeaderName
  | RequestTrailersUnsupportedVersion !Version
  deriving stock (Eq, Show)

instance Exception RequestTrailerError


{- | Announce the supplied field names and attach a trailer producer.
The built-in transports evaluate it after the body reaches EOF. Use
`pure fields` for precomputed trailers. A body-preserving retry can
send the request again, so the action must be repeatable.
Only announce fields whose definitions allow trailer use.
-}
withRequestTrailers
  :: NonEmpty H.HeaderName
  -> IO H.Headers
  -> Request a
  -> Either RequestTrailerError (Request a)
withRequestTrailers names produce req = do
  mapM_ validateTrailerName names
  let announcement = BS.intercalate ", " (map CI.original (NE.toList names))
      checkedTrailers = do
        fields <- produce
        mapM_ (either throwIO pure . validateTrailerField) fields
        pure fields
  pure (setHeader H.hTrailer announcement (addProtocolHint (withProtocolRequestTrailers (Just checkedTrailers)) req))
  where
    validateTrailerField (name, value) = do
      validateTrailerName name
      if H.isValidHeaderValue value then Right () else Left (InvalidRequestTrailerValue name)
    validateTrailerName name
      | not (H.isValidHeaderName (CI.original name)) = Left (InvalidRequestTrailerName name)
      | isForbiddenTrailerName (CI.foldedCase name) =
          Left (ForbiddenRequestTrailer name)
      | otherwise = Right ()


-- | Clear both the announcement and the producer.
withoutRequestTrailers :: Request a -> Request a
withoutRequestTrailers = removeHeader H.hTrailer . addProtocolHint (withProtocolRequestTrailers Nothing)



{- | Encode a value as the request body, setting @Content-Type@ from
the tag's media type. The new request has its body type set to
strict 'ByteString'.
-}
withBody
  :: forall tag a
   . (Encode tag a)
  => a -> Request () -> Request ByteString
withBody a r =
  r
    { body = encode @tag a
    , headers =
        H.insertHeader
          H.hContentType
          (renderMediaType (mediaType @tag))
          (headers r)
    }


{- | Attach a raw 'ByteString' body without setting @Content-Type@.
Useful when the caller knows the media type out of band.
-}
withRawBody :: ByteString -> Request () -> Request ByteString
withRawBody bs r = r {body = bs}


addSpanAttribute :: Text -> SpanAttribute -> Request a -> Request a
addSpanAttribute k v r = r {spanAttributes = spanAttributes r <> [(k, v)]}


addProtocolHint :: (ProtocolHints -> ProtocolHints) -> Request a -> Request a
addProtocolHint f r = r {protocolHints = f (protocolHints r)}


{- | Map over the body. The 'Functor' instance does the same thing
but having a name helps readability for body conversions.
-}
mapBody :: (a -> b) -> Request a -> Request b
mapBody = fmap
