{-# LANGUAGE PatternSynonyms #-}

{- | Protocol-version metadata that flows alongside requests and
responses.

These are deliberately optional: a transport that doesn't care
populates 'HTTP1_1' and ignores 'ProtocolHints'. They exist so
callers that /do/ care can branch on the negotiated version (HTTP\/2
push, HTTP\/3 0-RTT) by pattern matching, without forcing every
transport to implement those features.

The 'PushPromise' payload references the request and the raw
response types from "Network.HTTP.Client.Request" and
"Network.HTTP.Client.Response", which would induce a mutual-import
cycle if we declared them all in one module. To keep the dependency
graph clean, 'PushPromise' is parameterised over its request and
response types and re-exported with the concrete types in
"Network.HTTP.Client.Protocol.Pushed".
-}
module Network.HTTP.Client.Protocol (
  ProtocolInfo (HTTP1_1, HTTP2, HTTP3),
  protocolTrailers,
  withProtocolTrailers,
  withBufferedTrailers,
  mapProtocolTrailers,
  Http2Info (..),
  Http3Info (..),
  PushPromise (..),
  ProtocolHints (ProtocolHints, h2Priority, h3EarlyData),
  H2Priority (..),
  defaultHints,
  withContinueTimeoutMicros,
  continueTimeoutMicros,
  protocolRequestTrailers,
  withProtocolRequestTrailers,

  -- * Transport capabilities
  TransportCapabilities (..),
  defaultCapabilities,
) where

import Data.Void (Void)
import Data.Word (Word32, Word64)
import Network.HTTP.Types.Header (Headers)


{- | The version actually negotiated for a particular response.
'Http2Info' / 'Http3Info' are parameterised over the request and
raw-response types so the protocol module can declare them
without depending on the higher-level Request \/ Response
modules; concrete aliases live in "Network.HTTP.Client" and
'Network.HTTP.Client.Send'.
-}
data ProtocolInfo req raw
  = ProtocolHTTP1 !(Maybe (IO Headers))
  | ProtocolHTTP2 !(Http2Info req raw) !(Maybe (IO Headers))
  | ProtocolHTTP3 !Http3Info !(Maybe (IO Headers))


pattern HTTP1_1 :: ProtocolInfo req raw
pattern HTTP1_1 <- ProtocolHTTP1 _ where
  HTTP1_1 = ProtocolHTTP1 Nothing


pattern HTTP2 :: Http2Info req raw -> ProtocolInfo req raw
pattern HTTP2 info <- ProtocolHTTP2 info _ where
  HTTP2 info = ProtocolHTTP2 info Nothing


pattern HTTP3 :: Http3Info -> ProtocolInfo req raw
pattern HTTP3 info <- ProtocolHTTP3 info _ where
  HTTP3 info = ProtocolHTTP3 info Nothing


{-# COMPLETE HTTP1_1, HTTP2, HTTP3 #-}


-- | Read the response trailer section after draining the body.
protocolTrailers :: ProtocolInfo req raw -> IO Headers
protocolTrailers protocol = case protocol of
  ProtocolHTTP1 trailers -> readTrailers trailers
  ProtocolHTTP2 _ trailers -> readTrailers trailers
  ProtocolHTTP3 _ trailers -> readTrailers trailers
  where
    readTrailers = maybe (pure []) id


-- | Attach the transport's trailer reader to the negotiated version.
withProtocolTrailers :: IO Headers -> ProtocolInfo req raw -> ProtocolInfo req raw
withProtocolTrailers trailers protocol = case protocol of
  ProtocolHTTP1 _ -> ProtocolHTTP1 (Just trailers)
  ProtocolHTTP2 info _ -> ProtocolHTTP2 info (Just trailers)
  ProtocolHTTP3 info _ -> ProtocolHTTP3 info (Just trailers)


-- | Attach a completed trailer section to a newly built response.
withBufferedTrailers :: Headers -> ProtocolInfo req raw -> ProtocolInfo req raw
withBufferedTrailers [] protocol = protocol
withBufferedTrailers trailers protocol = withProtocolTrailers (pure trailers) protocol


-- | Transform received trailers without forcing their reader.
mapProtocolTrailers :: (Headers -> Headers) -> ProtocolInfo req raw -> ProtocolInfo req raw
mapProtocolTrailers transform protocol = case protocol of
  ProtocolHTTP1 (Just trailers) -> ProtocolHTTP1 (Just (transform <$> trailers))
  ProtocolHTTP2 info (Just trailers) -> ProtocolHTTP2 info (Just (transform <$> trailers))
  ProtocolHTTP3 info (Just trailers) -> ProtocolHTTP3 info (Just (transform <$> trailers))
  _ -> protocol


instance Show (ProtocolInfo req raw) where
  show HTTP1_1 = "HTTP1_1"
  show (HTTP2 _) = "HTTP2 <Http2Info>"
  show (HTTP3 i) = "HTTP3 " <> show i


data Http2Info req raw = Http2Info
  { h2StreamId :: !Word32
  , h2PushPromises :: !(IO [PushPromise req raw])
  {- ^ Realised lazily: forcing this returns whatever push promises
  the server has announced on this stream so far.
  -}
  , h2CancelStream :: !(IO ())
  {- ^ Best-effort cancellation of this stream. Transports that
  expose @RST_STREAM(CANCEL)@ at the underlying connection
  handle populate this; the eager-drain transports default to
  'pure ()' because the body is already materialised by the
  time the caller could observe a 'RawResponse'. Streaming
  transports populate it with a real cancel.
  -}
  }


data Http3Info = Http3Info
  { h3StreamId :: !Word64
  , h3PeerAddr :: !(Maybe String)
  }
  deriving stock (Show)


{- | An HTTP\/2 push promise. 'pushPromisedRequest' is the request
the server claims it's about to fulfil (always a no-body 'req
Void' value); 'pushFulfil' blocks until the response actually
arrives.
-}
data PushPromise req raw = PushPromise
  { pushPromisedRequest :: !(req Void)
  , pushFulfil :: !(IO raw)
  }


{- | Per-request advisory hints. Transports that don't understand a
given hint ignore it.
-}
data ProtocolHints
  = ProtocolHints
      { h2Priority :: !(Maybe H2Priority)
      , h3EarlyData :: !Bool
      -- ^ Eligibility for HTTP\/3 0-RTT (RFC 9114 § 4.1.4).
      }
  | ProtocolHintsWithContinue
      { h2Priority :: !(Maybe H2Priority)
      , h3EarlyData :: !Bool
      , savedContinueTimeoutMicros :: !Int
      }
  | ProtocolHintsWithTrailers
      { h2Priority :: !(Maybe H2Priority)
      , h3EarlyData :: !Bool
      , savedContinueTimeoutMicros :: !Int
      , savedTrailerAction :: !TrailerAction
      }
  deriving stock (Show)

newtype TrailerAction = TrailerAction (IO Headers)

instance Show TrailerAction where
  show _ = "<trailer action>"


data H2Priority = H2Priority
  { h2Urgency :: !Int
  -- ^ 0 (highest) — 7 (lowest)
  , h2Incremental :: !Bool
  }
  deriving stock (Show)


defaultHints :: ProtocolHints
defaultHints =
  ProtocolHints
    { h2Priority = Nothing
    , h3EarlyData = False
    }

-- | Override the per-request wait for a 100 Continue response.
withContinueTimeoutMicros :: Int -> ProtocolHints -> ProtocolHints
withContinueTimeoutMicros us hints = case hints of
  ProtocolHintsWithTrailers {} -> hints {savedContinueTimeoutMicros = max 0 us}
  _ -> ProtocolHintsWithContinue (h2Priority hints) (h3EarlyData hints) (max 0 us)

-- | The requested wait in microseconds, defaulting to one second.
continueTimeoutMicros :: ProtocolHints -> Int
continueTimeoutMicros ProtocolHints {} = 1_000_000
continueTimeoutMicros hints = savedContinueTimeoutMicros hints

protocolRequestTrailers :: ProtocolHints -> Maybe (IO Headers)
protocolRequestTrailers ProtocolHintsWithTrailers {savedTrailerAction = TrailerAction action} = Just action
protocolRequestTrailers _ = Nothing
{-# INLINE protocolRequestTrailers #-}

withProtocolRequestTrailers :: Maybe (IO Headers) -> ProtocolHints -> ProtocolHints
withProtocolRequestTrailers (Just action) hints =
  ProtocolHintsWithTrailers (h2Priority hints) (h3EarlyData hints) (continueTimeoutMicros hints) (TrailerAction action)
withProtocolRequestTrailers Nothing hints@ProtocolHintsWithTrailers {} =
  ProtocolHintsWithContinue (h2Priority hints) (h3EarlyData hints) (continueTimeoutMicros hints)
withProtocolRequestTrailers Nothing hints = hints


-- ---------------------------------------------------------------------------
-- Transport capabilities (for infrastructure code)
-- ---------------------------------------------------------------------------

data TransportCapabilities = TransportCapabilities
  { supportsH2Push :: !Bool
  , supportsH3 :: !Bool
  , supports0RTT :: !Bool
  , maxConcurrency :: !(Maybe Int)
  }
  deriving stock (Show)


defaultCapabilities :: TransportCapabilities
defaultCapabilities =
  TransportCapabilities
    { supportsH2Push = False
    , supportsH3 = False
    , supports0RTT = False
    , maxConcurrency = Nothing
    }
