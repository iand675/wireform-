{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

{- |
@Accept-Encoding@ — a request header by which a client states which
content codings (compression schemes such as @gzip@, @br@, or
@deflate@) it can decode in the response, each with an optional @;q=@
quality weight. Servers use it to choose how to encode the returned
representation. A response can advertise the codings the server can
decode in requests.

The wire syntax is a comma-separated list of @coding [;q=value]@
entries, where @coding@ is a content-coding name from
"Network.HTTP.ContentCoding" (or the @*@ wildcard) and @q@ is
optional. Each entry becomes a 'WeightedEncoding'.

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-12.5.3>

See also: "Network.HTTP.Headers.Accept", "Network.HTTP.Headers.AcceptCharset", "Network.HTTP.Headers.AcceptLanguage", "Network.HTTP.Headers.ContentEncoding", "Network.HTTP.Headers.Vary".
-}
module Network.HTTP.Headers.AcceptEncoding (
  AcceptEncoding (..),
  WeightedEncoding (..),
  EncodingTag (..),
  preferredContentCoding,
  acceptEncodingParser,
  renderAcceptEncoding,
) where

import Data.List (find)
import Data.Maybe (listToMaybe)
import GHC.Exts (Double (D#), Double#, Int#, isTrue#, (+#), (<#), (==##), (>##))
import Network.HTTP.ContentCoding
import Network.HTTP.ContentNegotiation (encodeWeightedTokensChecked, negotiationList, parseNegotiationHeaders, renderWeight, weightParser)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAcceptEncoding)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | A single entry in an @Accept-Encoding@ list.
data EncodingTag
  = -- | The @*@ wildcard.
    AnyEncoding
  | NamedEncoding !ContentCoding
  deriving stock (Eq, Show)


-- | An encoding tag with an optional quality weight (defaults to 1.0).
data WeightedEncoding = WeightedEncoding
  { encodingTag :: !EncodingTag
  , encodingWeight :: {-# UNPACK #-} !Double
  }
  deriving stock (Eq, Show)


-- | The full @Accept-Encoding@ header value.
newtype AcceptEncoding = AcceptEncoding
  { acceptEncoding :: [WeightedEncoding]
  }
  deriving stock (Eq, Show)


{- | Select an available coding using RFC 9110 section 12.5.3. Pass
'Identity' in the available list when the unencoded representation can
be sent. An absent field accepts the first available coding. An implicit
identity is a fallback and an explicit identity competes by its weight.
Use field order to break ties between equal weights, then available
order when the choices match the same wildcard. For duplicate entries
that name the same coding or wildcard, use the first entry. Weights
above one are treated as one.
-}
preferredContentCoding :: Maybe AcceptEncoding -> [ContentCoding] -> Maybe ContentCoding
preferredContentCoding Nothing available = listToMaybe available
preferredContentCoding (Just (AcceptEncoding [])) available = find (== Identity) available
preferredContentCoding (Just (AcceptEncoding entries)) available
  -- The first explicit coding at the maximum weight wins every tie.
  | WeightedEncoding (NamedEncoding named) weight : _ <- entries
  , weight >= 1 = chooseNamed named available
  | otherwise = chooseFirst available
  where
    chooseNamed _ [] = chooseFirst available
    chooseNamed named (coding : rest)
      | sameContentCoding named coding = Just coding
      | otherwise = chooseNamed named rest

    chooseFirst [] = Nothing
    chooseFirst (coding : rest) = case encodingRank coding entries of
      (# (# weight, position #) | #) -> choose coding weight position rest
      (# | (# #) #) -> chooseFirst rest

    choose :: ContentCoding -> Double# -> Int# -> [ContentCoding] -> Maybe ContentCoding
    choose best _ _ [] = Just best
    choose best bestWeight bestPosition (coding : rest) =
      case encodingRank coding entries of
        (# (# weight, position #) | #)
          | isTrue# (weight >## bestWeight)
              || (isTrue# (weight ==## bestWeight) && isTrue# (position <# bestPosition)) ->
              choose coding weight position rest
        _ -> choose best bestWeight bestPosition rest


type EncodingRank = (# (# Double#, Int# #) | (# #) #)


encodingRank :: ContentCoding -> [WeightedEncoding] -> EncodingRank
encodingRank coding = go 0# 0.0## (-1#)
  where
    -- A negative wildcard position indicates that the field omitted it.
    go :: Int# -> Double# -> Int# -> [WeightedEncoding] -> EncodingRank
    go _ wildcardWeight wildcardPosition []
      | coding == Identity =
          if isTrue# (wildcardPosition <# 0#) || isTrue# (wildcardWeight >## 0.0##)
            then (# (# 0.0##, 0# #) | #)
            else (# | (# #) #)
      | otherwise = accept wildcardWeight wildcardPosition
    go position wildcardWeight wildcardPosition (WeightedEncoding tag (D# weight) : rest) =
      case tag of
        NamedEncoding named
          | sameContentCoding coding named -> accept weight position
        AnyEncoding
          | isTrue# (wildcardPosition <# 0#) -> go (position +# 1#) weight position rest
        _ -> go (position +# 1#) wildcardWeight wildcardPosition rest

    accept :: Double# -> Int# -> EncodingRank
    accept weight position
      | isTrue# (weight >## 1.0##) = (# (# 1.0##, position #) | #)
      | isTrue# (weight >## 0.0##) = (# (# weight, position #) | #)
      | otherwise = (# | (# #) #)


instance KnownHeader AcceptEncoding where
  type ParseFailure AcceptEncoding = String
  type Cardinality AcceptEncoding = 'ZeroOrOne
  type Direction AcceptEncoding = 'RequestAndResponse


  parseFromHeaders _ = fmap AcceptEncoding . parseNegotiationHeaders "Accept-Encoding" listParser


  renderToHeaders _ = M.toStrictByteString . renderAcceptEncoding


  renderToHeadersChecked _ =
    encodeWeightedTokensChecked (validEncoding . encodingTag) (encodingText . encodingTag) encodingWeight . acceptEncoding
    where
      validEncoding AnyEncoding = True
      validEncoding (NamedEncoding coding) = isValidContentCoding coding && contentCodingText coding /= "*"
      encodingText AnyEncoding = "*"
      encodingText (NamedEncoding coding) = contentCodingText coding


  headerName _ = hAcceptEncoding


acceptEncodingParser :: ParserT st String AcceptEncoding
acceptEncodingParser = AcceptEncoding <$> listParser


listParser :: ParserT st String [WeightedEncoding]
listParser = negotiationList weightedEncodingParser
  where
    weightedEncodingParser = do
      tag <- encodingTagParser
      w <- weightParser
      pure $ WeightedEncoding tag w

    encodingTagParser =
      contentCodingParser >>= \case
        Custom "*" -> pure AnyEncoding
        coding -> pure (NamedEncoding coding)


renderWeightedEncoding :: WeightedEncoding -> M.Builder
renderWeightedEncoding (WeightedEncoding tag w) =
  renderTag tag
    <> renderWeight w
  where
    renderTag AnyEncoding = "*"
    renderTag (NamedEncoding c) = renderContentCoding c


renderAcceptEncoding :: AcceptEncoding -> M.Builder
renderAcceptEncoding (AcceptEncoding xs) =
  M.intersperse ", " (map renderWeightedEncoding xs)
