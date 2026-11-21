{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

{- |
@Accept-Language@ — a request header by which a client states which
natural languages (language tags such as @en@, @en-US@, or @fr@) it
prefers for the response, each with an optional @;q=@ quality weight.
Servers use it for proactive content negotiation of localized
representations.

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-12.5.4>

See also: "Network.HTTP.Headers.Accept", "Network.HTTP.Headers.AcceptCharset", "Network.HTTP.Headers.AcceptEncoding", "Network.HTTP.Headers.ContentLanguage", "Network.HTTP.Headers.Vary".
-}
module Network.HTTP.Headers.AcceptLanguage (
  AcceptLanguage (..),
  WeightedLanguage (..),
  acceptedLanguageQuality,
  preferredLanguage,
  acceptLanguageParser,
  renderAcceptLanguage,
) where

import Data.Bits ((.|.))
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBSI
import Data.Either (isRight)
import Data.Maybe (listToMaybe)
import qualified Data.Text.Short as ST
import GHC.Exts (eqAddr#, eqChar#, indexCharOffAddr#, isTrue#, leChar#, plusAddr#, (+#), (==#))
import Network.HTTP.ContentNegotiation (encodeWeightedTokensChecked, negotiationList, normalizedQuality, parseNegotiationHeaders, preferredByQuality, renderWeight, weightParser)
import Network.HTTP.Headers
import Network.HTTP.Headers.ContentLanguage (LanguageTag (..))
import Network.HTTP.Headers.HeaderFieldName (hAcceptLanguage)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)
import Wireform.Parser.Driver (parseByteString)
import Wireform.Parser.Internal (Parser (..), pattern Fail#, pattern OK#)


-- | An RFC 4647 basic language range with a quality weight.
data WeightedLanguage = WeightedLanguage
  { languageTag :: !ST.ShortText
  , languageWeight :: !Double
  }
  deriving stock (Eq, Show)


newtype AcceptLanguage = AcceptLanguage {languages :: [WeightedLanguage]}
  deriving stock (Eq, Show)


{- | Rate a language tag using RFC 4647 Basic Filtering. An exact match
or a prefix ending at a subtag boundary is case-insensitive. The longest
matching range supplies the weight, with field order breaking equal
specificity. A wildcard applies when no specific range matches.

An absent field gives a tag quality one. An empty field or an unmatched
tag has quality zero.
-}
acceptedLanguageQuality :: Maybe AcceptLanguage -> LanguageTag -> Double
acceptedLanguageQuality Nothing _ = 1
acceptedLanguageQuality (Just (AcceptLanguage ranges)) tag = languageQuality ranges tag
{-# INLINE acceptedLanguageQuality #-}


-- | Select the available tag with the highest positive effective
-- quality. The offered order breaks ties.
preferredLanguage :: Maybe AcceptLanguage -> [LanguageTag] -> Maybe LanguageTag
preferredLanguage Nothing = listToMaybe
preferredLanguage (Just (AcceptLanguage ranges)) = chooseLanguage ranges
{-# INLINE preferredLanguage #-}


chooseLanguage :: [WeightedLanguage] -> [LanguageTag] -> Maybe LanguageTag
chooseLanguage ranges = preferredByQuality (languageQuality ranges)


languageQuality :: [WeightedLanguage] -> LanguageTag -> Double
languageQuality [] _ = 0
languageQuality ranges (LanguageTag tag) = select (-1) 0 ranges
  where
    offered = ST.toShortByteString tag
    !tagLength = SBS.length offered

    select :: Int -> Double -> [WeightedLanguage] -> Double
    select !_ !bestWeight [] = normalizedQuality bestWeight
    select !bestLength !bestWeight (WeightedLanguage range weight : rest)
      | rangeLength == 1 && SBSI.unsafeIndex prefix 0 == 42 =
          if bestLength < 0 then select 0 weight rest else select bestLength bestWeight rest
      | rangeLength > bestLength
      , rangeLength > 0
      , rangeLength <= tagLength
      , rangeLength == tagLength || SBSI.unsafeIndex offered rangeLength == 45
      , matchesPrefix prefix rangeLength =
          if rangeLength == tagLength then normalizedQuality weight else select rangeLength weight rest
      | otherwise = select bestLength bestWeight rest
      where
        prefix = ST.toShortByteString range
        rangeLength = SBS.length prefix

    matchesPrefix prefix limit = loop 0
      where
        loop i
          | i == limit = True
          | a == b || (lowerA == (b .|. 32) && 97 <= lowerA && lowerA <= 122) = loop (i + 1)
          | otherwise = False
          where
            a = SBSI.unsafeIndex prefix i
            b = SBSI.unsafeIndex offered i
            lowerA = a .|. 32


instance KnownHeader AcceptLanguage where
  type ParseFailure AcceptLanguage = String
  type Cardinality AcceptLanguage = 'ZeroOrOne
  type Direction AcceptLanguage = 'Request


  parseFromHeaders _ = fmap AcceptLanguage . parseNegotiationHeaders "Accept-Language" listParser


  renderToHeaders _ = M.toStrictByteString . renderAcceptLanguage


  renderToHeadersChecked _ =
    encodeWeightedTokensChecked valid languageTag languageWeight . languages
    where
      valid = isRight . parseByteString (languageRange <* eof) . ST.toByteString . languageTag


  headerName _ = hAcceptLanguage


acceptLanguageParser :: ParserT st String AcceptLanguage
acceptLanguageParser = AcceptLanguage <$> listParser


listParser :: ParserT st String [WeightedLanguage]
listParser = negotiationList weightedLanguageParser
  where
    weightedLanguageParser = do
      lang <- shortASCIIFromParser_ languageRange
      q <- weightParser
      pure $ WeightedLanguage lang q


{- | RFC 9110 §12.5.4 selects RFC 4647 basic ranges. The first subtag
is alphabetic, later subtags allow digits, and a lone "*" is a range.
-}
languageRange :: ParserT st String ()
languageRange = Parser $ \_ end start state ->
  let failure = (# state, Fail# #)
      finish cursor = (# state, OK# () cursor #)
      scan cursor width first
        | isTrue# (eqAddr# cursor end) = finishSubtag cursor width
        | otherwise = case indexCharOffAddr# cursor 0# of
            c
              | isLetter c || (not first && isDigitByte c) ->
                  if isTrue# (width ==# 8#)
                    then failure
                    else scan (plusAddr# cursor 1#) (width +# 1#) first
              | isTrue# (eqChar# c '-'#) && not (isTrue# (width ==# 0#)) ->
                  scan (plusAddr# cursor 1#) 0# False
              | otherwise -> finishSubtag cursor width
      finishSubtag cursor width
        | isTrue# (width ==# 0#) = failure
        | otherwise = finish cursor
      isLetter c = inRange 'a'# 'z'# c || inRange 'A'# 'Z'# c
      isDigitByte c = inRange '0'# '9'# c
      inRange lower upper c = isTrue# (leChar# lower c) && isTrue# (leChar# c upper)
  in if isTrue# (eqAddr# start end)
       then failure
       else
         if isTrue# (eqChar# (indexCharOffAddr# start 0#) '*'#)
           then finish (plusAddr# start 1#)
           else scan start 0# True
{-# INLINE languageRange #-}


renderWeightedLanguage :: WeightedLanguage -> M.Builder
renderWeightedLanguage (WeightedLanguage tag w) =
  shortText tag <> renderWeight w


renderAcceptLanguage :: AcceptLanguage -> M.Builder
renderAcceptLanguage (AcceptLanguage langs) = M.intersperse ", " $ map renderWeightedLanguage langs
