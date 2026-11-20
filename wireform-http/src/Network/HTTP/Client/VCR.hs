{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | VCR-style cassettes for recording and replaying HTTP interactions.

Cassettes serialise to YAML via @wireform-yaml@. The format is human-
readable on purpose — cassettes go in version control, so reviewers
can sanity-check them.

Workflow:

* First run (against a real server):

@
recordSession baseTransport "fixtures\/login.yaml" \\transport -\> do
  runMyClient transport
@

* Subsequent runs (in CI):

@
cassette  <- loadCassette "fixtures\/login.yaml"
transport <- replayTransport cassette sequential
runMyClient transport
@

The mock\/replay 'Transport' shares 'RecordedRequest' \/
'RecordedResponse' with "Network.HTTP.Client.Test" so VCR cassettes
and request-log assertions speak the same vocabulary.
-}
module Network.HTTP.Client.VCR (
  -- * Cassettes
  Cassette (..),
  Interaction (..),
  RecordedRequest (..),
  RecordedResponse (..),
  RecordedHeader (..),
  RecordedMethod (..),
  loadCassette,
  saveCassette,

  -- * Recording
  recordSession,
  recordSessionChunked,
  withRecording,
  withChunkedRecording,

  -- * Replay
  replayTransport,
  MatchStrategy (..),
  sequential,
  byMethodAndURI,
  byMethodURIAndHeaders,
  customStrategy,
  NoMatchingInteraction (..),

  -- * Sanitization
  Sanitizer (..),
  redactHeaders,
  redactBodyPattern,
  applySanitizer,
) where

import Control.Exception (Exception, finally, throwIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.CaseInsensitive qualified as CI
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Network.HTTP.Client.BodyStream
import Network.HTTP.Client.Protocol
import Network.HTTP.Client.Request qualified as WReq
import Network.HTTP.Client.Response
import Network.HTTP.Client.Transport
import Network.HTTP.Client.URI (requestURIToText)
import Network.HTTP.Types.Header qualified as H
import Network.HTTP.Types.Method qualified as M
import Network.HTTP.Types.Status qualified as S
import YAML.Class qualified as Y


-- ---------------------------------------------------------------------------
-- Cassette data
-- ---------------------------------------------------------------------------

newtype RecordedMethod = RecordedMethod {unRecordedMethod :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Y.ToYAML, Y.FromYAML)


methodToRec :: M.Method -> RecordedMethod
methodToRec = RecordedMethod . TE.decodeUtf8 . M.fromMethod


{- | A header pair as recorded. We avoid 'CI.CI' in the on-disk shape
because @wireform-yaml@'s 'Generic' deriver doesn't know about it.
-}
data RecordedHeader = RecordedHeader
  { rhName :: !Text
  , rhValue :: !Text
  {- ^ Base64-encoded if the original bytes aren't UTF-8;
  plain text otherwise. The 'rhBinary' flag disambiguates.
  -}
  , rhBinary :: !Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Y.ToYAML, Y.FromYAML)


headerToRec :: H.Header -> RecordedHeader
headerToRec (n, v) = case TE.decodeUtf8' v of
  Right t -> RecordedHeader (TE.decodeUtf8 (CI.original n)) t False
  Left _ ->
    RecordedHeader
      (TE.decodeUtf8 (CI.original n))
      (TE.decodeUtf8 (B64.encode v))
      True


headerFromRec :: RecordedHeader -> H.Header
headerFromRec h =
  let nameBs = TE.encodeUtf8 (rhName h)
      bytes
        | rhBinary h =
            fromMaybe
              (TE.encodeUtf8 (rhValue h))
              (eitherToMaybe (B64.decode (TE.encodeUtf8 (rhValue h))))
        | otherwise = TE.encodeUtf8 (rhValue h)
  in (CI.mk nameBs, bytes)
  where
    eitherToMaybe :: Either a b -> Maybe b
    eitherToMaybe = either (const Nothing) Just


data RecordedRequest = RecordedRequest
  { rrqMethod :: !RecordedMethod
  , rrqURI :: !Text
  , rrqHeaders :: ![RecordedHeader]
  , rrqBody :: !Text
  , rrqBodyBinary :: !Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Y.ToYAML, Y.FromYAML)


data RecordedResponse = RecordedResponse
  { rrsStatus :: !Int
  , rrsHeaders :: ![RecordedHeader]
  , rrsBody :: !Text
  , rrsBodyBinary :: !Bool
  , rrsBodyChunks :: !(Maybe [Text])
  {- ^ When 'Just', the body was recorded with chunk boundaries
  preserved and replay yields one chunk per popper read
  (modulo Base64-decoding for binary chunks). When
  'Nothing', the body is the single 'rrsBody' field.
  Older cassettes lack this key; YAML's @FromYAML@ deriver
  defaults missing keys to 'Nothing' so the format is
  backward-compatible.
  -}
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Y.ToYAML, Y.FromYAML)


data Interaction = Interaction
  { interactionRequest :: !RecordedRequest
  , interactionResponse :: !RecordedResponse
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Y.ToYAML, Y.FromYAML)


data Cassette = Cassette
  { cassetteRecordedAt :: !Text
  {- ^ ISO-8601 timestamp. Stringly typed in the cassette so
  consumers don\'t need a 'Day' \/ 'UTCTime' YAML instance.
  -}
  , cassetteMetadata :: ![(Text, Text)]
  , cassetteInteractions :: ![Interaction]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Y.ToYAML, Y.FromYAML)


newCassette :: UTCTime -> [Interaction] -> Cassette
newCassette ts xs =
  Cassette
    { cassetteRecordedAt = T.pack (show ts)
    , cassetteMetadata = []
    , cassetteInteractions = xs
    }


bodyToRec :: ByteString -> (Text, Bool)
bodyToRec bs = case TE.decodeUtf8' bs of
  Right t -> (t, False)
  Left _ -> (TE.decodeUtf8 (B64.encode bs), True)


bodyFromRec :: Text -> Bool -> ByteString
bodyFromRec t False = TE.encodeUtf8 t
bodyFromRec t True = case B64.decode (TE.encodeUtf8 t) of
  Right bs -> bs
  Left _ -> TE.encodeUtf8 t


loadCassette :: FilePath -> IO Cassette
loadCassette path = do
  bs <- BS.readFile path
  case Y.decodeYAMLBS bs of
    Right c -> pure c
    Left err -> throwIO (CassetteParseError path err)


saveCassette :: FilePath -> Cassette -> IO ()
saveCassette path c = BS.writeFile path (Y.encodeYAMLBS c)


data CassetteError = CassetteParseError !FilePath !String
  deriving stock (Show)


instance Exception CassetteError


-- ---------------------------------------------------------------------------
-- Recording
-- ---------------------------------------------------------------------------

{- | Wrap a transport so that every interaction is appended to the
given ref. Drains both bodies through a strict buffer so the
downstream callee still sees a popper. The recorded response
has @rrsBodyChunks = Nothing@.
-}
withRecording :: IORef [Interaction] -> Middleware IO
withRecording ref inner = Transport $ \req -> do
  reqBody <- bodyStreamBytes (WReq.body req)
  rebuilt <- streamFromStrict reqBody
  let req' = req {WReq.body = rebuilt}
  raw <- sendRaw inner req'
  respBody <- popperBytes (bodyPopper raw)
  let recRq = toRecordedRequest req' reqBody
      recRs = toRecordedResponse raw respBody Nothing
  modifyIORef' ref (<> [Interaction recRq recRs])
  newPopper <- popperFromStrict respBody
  pure raw {bodyPopper = newPopper}


{- | Like 'withRecording' but preserves response-body chunk
boundaries on the cassette. Each chunk the inner transport
emits is captured individually; on replay the popper yields
the same sequence one chunk at a time. The recorded request
body is still materialised, since request bodies generally
aren't chunk-sensitive.
-}
withChunkedRecording :: IORef [Interaction] -> Middleware IO
withChunkedRecording ref inner = Transport $ \req -> do
  reqBody <- bodyStreamBytes (WReq.body req)
  rebuilt <- streamFromStrict reqBody
  let req' = req {WReq.body = rebuilt}
  raw <- sendRaw inner req'
  -- Drain the response popper into a list of chunks, preserving
  -- boundaries. Then store both the joined body (for
  -- compatibility) and the chunk list.
  chunksRef <- newIORef []
  let collect = do
        chunk <- bodyPopper raw
        if BS.null chunk
          then pure ()
          else do
            modifyIORef' chunksRef (chunk :)
            collect
  collect
  chunkList <- reverse <$> readIORef chunksRef
  let joined = BS.concat chunkList
      recRq = toRecordedRequest req' reqBody
      recRs = toRecordedResponse raw joined (Just chunkList)
  modifyIORef' ref (<> [Interaction recRq recRs])
  newPopper <- popperFromList chunkList
  pure raw {bodyPopper = newPopper}


{- | Record a session: wraps the action with a recording transport,
writes the resulting cassette to disk on success.
-}
recordSession :: Transport IO -> FilePath -> (Transport IO -> IO a) -> IO a
recordSession = recordSessionWith withRecording


{- | 'recordSession' that preserves chunk boundaries on the response
body. Use when the code under test cares about chunk timing or
when downstream middleware (compression, decoders) does
chunk-aware work.
-}
recordSessionChunked
  :: Transport IO -> FilePath -> (Transport IO -> IO a) -> IO a
recordSessionChunked = recordSessionWith withChunkedRecording


recordSessionWith
  :: (IORef [Interaction] -> Middleware IO)
  -> Transport IO
  -> FilePath
  -> (Transport IO -> IO a)
  -> IO a
recordSessionWith mk real path action = do
  ref <- newIORef []
  let transport = mk ref real
  result <-
    action transport
      `finally` ( do
                    interactions <- readIORef ref
                    now <- getCurrentTime
                    saveCassette path (newCassette now interactions)
                )
  pure result


toRecordedRequest :: WReq.Request BodyStream -> ByteString -> RecordedRequest
toRecordedRequest req bs =
  let (body_, binary) = bodyToRec bs
  in RecordedRequest
       { rrqMethod = methodToRec (WReq.method req)
       , rrqURI = requestURIToText (WReq.requestURI req)
       , rrqHeaders = map headerToRec (WReq.headers req)
       , rrqBody = body_
       , rrqBodyBinary = binary
       }


toRecordedResponse
  :: RawResponse
  -> ByteString
  -> Maybe [ByteString]
  -> RecordedResponse
toRecordedResponse raw bs mChunks =
  let (body_, binary) = bodyToRec bs
      chunks = fmap (map (fst . bodyToRec)) mChunks
  in RecordedResponse
       { rrsStatus = fromIntegral (S.statusCode (statusCode raw))
       , rrsHeaders = map headerToRec (Network.HTTP.Client.Response.headers raw)
       , rrsBody = body_
       , rrsBodyBinary = binary
       , rrsBodyChunks = chunks
       }


-- ---------------------------------------------------------------------------
-- Replay
-- ---------------------------------------------------------------------------

data MatchStrategy = MatchStrategy
  { strategyName :: !Text
  , matchInteraction
      :: WReq.Request BodyStream
      -> [Interaction]
      -> IO (Maybe (Interaction, [Interaction]))
  }


{- | Replay a cassette as a standalone transport. The transport
maintains its own cursor; once exhausted (or on a non-match) it
throws 'NoMatchingInteraction'.
-}
replayTransport :: Cassette -> MatchStrategy -> IO (Transport IO)
replayTransport c strat = do
  ref <- newIORef (cassetteInteractions c)
  pure $ Transport $ \req -> do
    interactions <- readIORef ref
    matchResult <- matchInteraction strat req interactions
    case matchResult of
      Nothing -> do
        snapshot <- toRecRequestSnapshot req
        throwIO (NoMatchingInteraction snapshot (strategyName strat))
      Just (interaction, rest) -> do
        writeIORef ref rest
        toRawResponse (interactionResponse interaction)


toRecRequestSnapshot :: WReq.Request BodyStream -> IO RecordedRequest
toRecRequestSnapshot req = do
  bodyBs <- bodyStreamBytes (WReq.body req)
  pure (toRecordedRequest req bodyBs)


toRawResponse :: RecordedResponse -> IO RawResponse
toRawResponse r = do
  popper <- case rrsBodyChunks r of
    Just chunks ->
      popperFromList
        (map (\c -> bodyFromRec c (rrsBodyBinary r)) chunks)
    Nothing ->
      popperFromStrict (bodyFromRec (rrsBody r) (rrsBodyBinary r))
  pure
    RawResponse
      { statusCode = S.Status (fromIntegral (rrsStatus r))
      -- The cassette stores the header section and body only.
      , headers = filter ((/= H.hTrailer) . fst) (map headerFromRec (rrsHeaders r))
      , bodyPopper = popper
      , protocolInfo = HTTP1_1
      }


{- | Strict sequential matching: each request must match the next
interaction in the cassette.
-}
sequential :: MatchStrategy
sequential = MatchStrategy "sequential" $ \req interactions -> case interactions of
  [] -> pure Nothing
  (i : rest) -> do
    bodyBs <- bodyStreamBytes (WReq.body req)
    let rec' = toRecordedRequest req bodyBs
    if rrqMethod (interactionRequest i) == rrqMethod rec'
      && rrqURI (interactionRequest i) == rrqURI rec'
      then pure (Just (i, rest))
      else pure Nothing


{- | Match by method + URI, ignoring order. The matched interaction is
removed from the remaining list.
-}
byMethodAndURI :: MatchStrategy
byMethodAndURI = MatchStrategy "byMethodAndURI" $ \req interactions -> do
  bodyBs <- bodyStreamBytes (WReq.body req)
  let rec' = toRecordedRequest req bodyBs
      pred_ i =
        rrqMethod (interactionRequest i) == rrqMethod rec'
          && rrqURI (interactionRequest i) == rrqURI rec'
  pure (extractFirst pred_ interactions)


{- | Like 'byMethodAndURI' but additionally requires that the given
header names match in value. Header-name comparison is
case-insensitive (RFC 9110 §5.1); header /values/ compare
byte-for-byte.
-}
byMethodURIAndHeaders :: [H.HeaderName] -> MatchStrategy
byMethodURIAndHeaders names = MatchStrategy "byMethodURIAndHeaders" $ \req interactions -> do
  bodyBs <- bodyStreamBytes (WReq.body req)
  let rec' = toRecordedRequest req bodyBs
      pred_ i =
        rrqMethod (interactionRequest i) == rrqMethod rec'
          && rrqURI (interactionRequest i) == rrqURI rec'
          && headersAgreeOn
            names
            (rrqHeaders rec')
            (rrqHeaders (interactionRequest i))
  pure (extractFirst pred_ interactions)


{- | Match by method + URI + request body. Body comparison is
byte-exact; for binary bodies that's the recorded base64
payload, for text bodies it's the UTF-8 round-trip. Use this for
POST\/PATCH-heavy cassettes where the URI alone is ambiguous.
-}
byMethodURIAndBody :: MatchStrategy
byMethodURIAndBody = MatchStrategy "byMethodURIAndBody" $ \req interactions -> do
  bodyBs <- bodyStreamBytes (WReq.body req)
  let rec' = toRecordedRequest req bodyBs
      pred_ i =
        let r = interactionRequest i
        in rrqMethod r == rrqMethod rec'
             && rrqURI r == rrqURI rec'
             && rrqBodyBinary r == rrqBodyBinary rec'
             && rrqBody r == rrqBody rec'
  pure (extractFirst pred_ interactions)


-- | Match by method + URI + body + a set of header names.
byMethodURIBodyAndHeaders :: [H.HeaderName] -> MatchStrategy
byMethodURIBodyAndHeaders names = MatchStrategy "byMethodURIBodyAndHeaders" $ \req interactions -> do
  bodyBs <- bodyStreamBytes (WReq.body req)
  let rec' = toRecordedRequest req bodyBs
      pred_ i =
        let r = interactionRequest i
        in rrqMethod r == rrqMethod rec'
             && rrqURI r == rrqURI rec'
             && rrqBodyBinary r == rrqBodyBinary rec'
             && rrqBody r == rrqBody rec'
             && headersAgreeOn names (rrqHeaders rec') (rrqHeaders r)
  pure (extractFirst pred_ interactions)


{- | Internal helper: does every name in @names@ have the same
value list in both header bags? Names compared
case-insensitively; values byte-exact.
-}
headersAgreeOn
  :: [H.HeaderName]
  -> [RecordedHeader]
  -> [RecordedHeader]
  -> Bool
headersAgreeOn names left right =
  all
    ( \n ->
        let key = CI.foldedCase n
            sel = filter (\h -> T.toCaseFold (rhName h) == TE.decodeUtf8 key)
            ls = sel left
            rs = sel right
            vals = map rhValue
        in vals ls == vals rs
    )
    names


-- | Build a custom strategy.
customStrategy
  :: Text
  -> (RecordedRequest -> Interaction -> Bool)
  -> MatchStrategy
customStrategy nm p = MatchStrategy nm $ \req interactions -> do
  bodyBs <- bodyStreamBytes (WReq.body req)
  let rec' = toRecordedRequest req bodyBs
  pure (extractFirst (p rec') interactions)


extractFirst :: (a -> Bool) -> [a] -> Maybe (a, [a])
extractFirst p xs = case break p xs of
  (_, []) -> Nothing
  (l, m : r) -> Just (m, l ++ r)


data NoMatchingInteraction = NoMatchingInteraction RecordedRequest Text
  deriving stock (Show)


instance Exception NoMatchingInteraction


-- ---------------------------------------------------------------------------
-- Sanitization
-- ---------------------------------------------------------------------------

data Sanitizer = Sanitizer
  { sanitizeRequest :: RecordedRequest -> RecordedRequest
  , sanitizeResponse :: RecordedResponse -> RecordedResponse
  }


instance Semigroup Sanitizer where
  a <> b =
    Sanitizer
      { sanitizeRequest = sanitizeRequest a . sanitizeRequest b
      , sanitizeResponse = sanitizeResponse a . sanitizeResponse b
      }


instance Monoid Sanitizer where
  mempty = Sanitizer id id


{- | Replace the values of the given header names with @REDACTED@ on
both requests and responses.
-}
redactHeaders :: [H.HeaderName] -> Sanitizer
redactHeaders names = Sanitizer redactReq redactResp
  where
    nameTexts = map (TE.decodeUtf8 . CI.foldedCase) names
    redactPair p =
      if T.toCaseFold (rhName p) `elem` map T.toCaseFold nameTexts
        then p {rhValue = "REDACTED", rhBinary = False}
        else p
    redactReq r = r {rrqHeaders = map redactPair (rrqHeaders r)}
    redactResp r = r {rrsHeaders = map redactPair (rrsHeaders r)}


{- | Replace every occurrence of a byte pattern in request and response
bodies. Only operates on text bodies (binary stays intact).
-}
redactBodyPattern :: ByteString -> ByteString -> Sanitizer
redactBodyPattern needle replacement = Sanitizer fixReq fixResp
  where
    nT = TE.decodeUtf8 needle
    rT = TE.decodeUtf8 replacement
    fixT t = T.intercalate rT (T.splitOn nT t)
    fixReq r
      | rrqBodyBinary r = r
      | otherwise = r {rrqBody = fixT (rrqBody r)}
    fixResp r
      | rrsBodyBinary r = r
      | otherwise = r {rrsBody = fixT (rrsBody r)}


applySanitizer :: Sanitizer -> Cassette -> Cassette
applySanitizer s c =
  c
    { cassetteInteractions =
        [ Interaction (sanitizeRequest s rq) (sanitizeResponse s rs)
        | Interaction rq rs <- cassetteInteractions c
        ]
    }
