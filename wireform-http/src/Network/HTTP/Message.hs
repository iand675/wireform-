{- | Unified request and response records for the wireform HTTP API.

These types are protocol-agnostic: they carry the negotiated
'Version' but the rest of the record looks the same regardless of
whether the underlying wire is HTTP\/1.x or HTTP\/2.

Conversion to and from the version-specific records lives in
"Network.HTTP.Client" / "Network.HTTP.Server"; callers should
generally only need these unified types.
-}
module Network.HTTP.Message (
  Request (..),
  Response (..),
  ResponsePushPromise (..),
  Scheme (..),
) where

import Control.DeepSeq (NFData (..))
import Data.ByteString (ByteString)
import Data.Word (Word32)
import GHC.Generics (Generic)
-- RawResponse is defined in the same package; no circular dependency.
-- Message.hs does not import Client.Response transitively.
import Network.HTTP.Client.Response (RawResponse)
import Network.HTTP.Types.Body
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version


{- | URL scheme.  HTTP\/2 carries this as the @:scheme@ pseudo-header;
HTTP\/1.x doesn't, but the unified API still tracks it so callers
can write protocol-agnostic logic.
-}
data Scheme = SchemeHttp | SchemeHttps
  deriving stock (Eq, Show, Generic)


instance NFData Scheme


{- | A unified HTTP request.

The @requestTarget@ is the same thing as HTTP\/1.x's request-target
(typically @\"/path?query\"@) and HTTP\/2's @:path@. The optional
@requestAuthority@ supplies the @Host@ header (HTTP\/1) or
@:authority@ pseudo-header (HTTP\/2).
-}
data Request = Request
  { requestMethod :: !Method
  , requestTarget :: !ByteString
  , requestAuthority :: !(Maybe ByteString)
  , requestScheme :: !Scheme
  , requestHeaders :: !Headers
  , requestBody :: !Body
  , requestVersion :: !Version
  {- ^ For an outgoing client request this is the /preferred/ version
  (the actual on-wire version comes out of negotiation). For an
  incoming server request it is the version that the peer
  actually spoke.
  -}
  , requestTrailers :: !(IO Headers)
  {- ^ Block on the trailer block carried after the request body.

  HTTP\/2 trailers arrive as a final HEADERS frame with
  @END_STREAM@; HTTP\/1.x trailers arrive in the field block
  after the chunked terminator.
  Returns @[]@ when the request had no trailers.  Must be called
  after 'requestBody' has been fully drained.
  -}
  }


instance Show Request where
  show r =
    "Request "
      <> show (requestMethod r)
      <> " "
      <> show (requestTarget r)
      <> " "
      <> show (requestVersion r)
      <> " "
      <> show (requestHeaders r)


instance NFData Request where
  rnf (Request m t a s h b v _) =
    rnf m `seq` rnf t `seq` rnf a `seq` rnf s `seq` rnf h `seq` rnf b `seq` rnf v


data Response = Response
  { responseStatus :: !Status
  , responseVersion :: !Version
  , responseHeaders :: !Headers
  , responseBody :: !Body
  , responseTrailers :: !(IO Headers)
  {- ^ Trailing headers carried after the body.

  HTTP\/1.x trailers arrive as part of the chunked body's
  terminator block; HTTP\/2 trailers arrive as a final HEADERS
  frame with @END_STREAM@.  The unified API exposes them as an
  'IO' action because, in either case, the trailers aren't
  materialised until the body has finished streaming.  Returns
  the empty list when the response had no trailers.

  The server evaluates this action after sending the body on HTTP\/2
  and on chunked HTTP\/1.1 responses. Declare the expected fields
  with @Trailer@ to select chunked framing for buffered HTTP\/1.1
  responses that carry trailers.
  -}
  , responseH2StreamId :: !Word32
  {- ^ HTTP\/2 stream id this response was carried on, or @0@
  for non-H2 responses or callers that don't surface it.
  -}
  , responseCancel :: !(IO ())
  {- ^ Best-effort cancellation. On HTTP\/2 this emits
  @RST_STREAM(CANCEL)@ to the peer. On HTTP\/1.x and on
  transports that have already drained the body, it's a
  no-op. Idempotent.
  -}
  , responsePushPromises :: !(IO [ResponsePushPromise])
  {- ^ Push promises delivered on this HTTP\/2 stream, in arrival
  order. Always @pure []@ for HTTP\/1.x responses. The list
  may grow as the body is consumed (push promises can arrive
  interleaved with DATA frames).
  -}
  }


-- | A push promise announced by the server on this HTTP\/2 stream.
data ResponsePushPromise = ResponsePushPromise
  { rppPromisedStreamId :: !Word32
  -- ^ The server-assigned promised stream ID.
  , rppHeaders :: ![(HeaderName, HeaderValue)]
  -- ^ Decoded push-promise request headers.
  , rppFulfil :: !(IO RawResponse)
  {- ^ Block until the pushed response arrives and return it
  fully materialised.  The body is drained before returning
  so callers do not need to manage the underlying HTTP\/2
  stream lifetime.  Only valid for the lifetime of the
  surrounding HTTP\/2 connection.
  -}
  }


instance Show ResponsePushPromise where
  show pp =
    "ResponsePushPromise "
      <> show (rppPromisedStreamId pp)
      <> " "
      <> show (map fst (rppHeaders pp))


instance Show Response where
  show r =
    "Response "
      <> show (responseStatus r)
      <> " "
      <> show (responseVersion r)
      <> " "
      <> show (responseHeaders r)


instance NFData Response where
  rnf (Response s v h b _ sid _ _) =
    rnf s `seq` rnf v `seq` rnf h `seq` rnf b `seq` rnf sid
