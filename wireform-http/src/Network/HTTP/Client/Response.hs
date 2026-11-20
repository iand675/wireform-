{- | Response types.

'RawResponse' is the wire-level shape that 'Transport' produces; it
carries the status, headers, popper, and the negotiated protocol
metadata. 'Response' is the value the caller receives after the
chosen 'ResponseBody' has been folded and the chosen
'ResponseDecoder' has decoded the bytes.
-}
module Network.HTTP.Client.Response (
  -- * Raw response
  RawResponse (..),
  rawResponseBytes,
  rawResponseTrailers,
  cancelStream,

  -- * Decoded response
  Response (..),
  responseTrailers,
  mapResponse,
) where

import Data.ByteString (ByteString)
import Network.HTTP.Client.BodyStream (Popper, popperBytes)
import Network.HTTP.Client.Protocol (
  Http2Info (..),
  ProtocolInfo (..),
  protocolTrailers,
 )
import Network.HTTP.Client.Request (Request)
import Network.HTTP.Types.Header qualified as H
import Network.HTTP.Types.Status qualified as S


{- | A raw wire response. The body is a 'Popper'; the recipient is
responsible for draining it before its scoped lifetime ends.
'protocolInfo' carries any HTTP\/2 push promises announced on the
stream (when the negotiated version supports them); the
'PushPromise' payload references both 'Request' and 'RawResponse'
itself, so the parameterisation closes the loop here.
-}
data RawResponse = RawResponse
  { statusCode :: !S.Status
  , headers :: !H.Headers
  , bodyPopper :: !Popper
  , protocolInfo :: !(ProtocolInfo Request RawResponse)
  }


instance Show RawResponse where
  show r =
    "RawResponse "
      <> show (statusCode r)
      <> " "
      <> show (headers r)
      <> " <body>"
      <> " "
      <> show (protocolInfo r)


{- | Materialise the raw response body to a strict 'ByteString'.
Convenience for tests and assertions. After this returns, the
popper is exhausted.
-}
rawResponseBytes :: RawResponse -> IO ByteString
rawResponseBytes = popperBytes . bodyPopper


-- | Read the trailer section after draining 'bodyPopper'. A streaming
-- transport may need the caller to consume the body to make progress.
rawResponseTrailers :: RawResponse -> IO H.Headers
rawResponseTrailers = protocolTrailers . protocolInfo


{- | Best-effort stream cancellation. On HTTP\/2 streaming
transports, this emits @RST_STREAM(CANCEL)@ to the peer. On
transports that have already drained the body, this is a no-op.
Idempotent (calling it more than once is harmless).
-}
cancelStream :: RawResponse -> IO ()
cancelStream raw = case protocolInfo raw of
  HTTP2 info -> h2CancelStream info
  _ -> pure ()


-- | The value 'send' returns. Functor in the body type.
data Response a = Response
  { responseStatus :: !S.Status
  , responseHeaders :: !H.Headers
  , responseBody :: !a
  , responseProtocolInfo :: !(ProtocolInfo Request RawResponse)
  }
  deriving stock (Functor)


instance Show a => Show (Response a) where
  show r =
    "Response "
      <> show (responseStatus r)
      <> " "
      <> show (responseHeaders r)
      <> " "
      <> show (responseBody r)
      <> " "
      <> show (responseProtocolInfo r)


-- | Read the trailer section of a response returned by 'send'.
responseTrailers :: Response a -> IO H.Headers
responseTrailers = protocolTrailers . responseProtocolInfo


mapResponse :: (a -> b) -> Response a -> Response b
mapResponse = fmap
