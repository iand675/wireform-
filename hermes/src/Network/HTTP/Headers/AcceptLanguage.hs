{-# LANGUAGE MagicHash #-}
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
  acceptLanguageParser,
  renderAcceptLanguage,
) where

import qualified Data.Text.Short as ST
import GHC.Exts (eqAddr#, eqChar#, indexCharOffAddr#, isTrue#, leChar#, plusAddr#, (+#), (==#))
import Network.HTTP.ContentNegotiation (negotiationList, parseNegotiationHeaders, renderWeight, weightParser)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAcceptLanguage)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)
import Wireform.Parser.Internal (Parser (..), pattern Fail#, pattern OK#)


-- | An RFC 4647 basic language range with a quality weight.
data WeightedLanguage = WeightedLanguage
  { languageTag :: !ST.ShortText
  , languageWeight :: !Double
  }
  deriving stock (Eq, Show)


newtype AcceptLanguage = AcceptLanguage {languages :: [WeightedLanguage]}
  deriving stock (Eq, Show)


instance KnownHeader AcceptLanguage where
  type ParseFailure AcceptLanguage = String
  type Cardinality AcceptLanguage = 'ZeroOrOne
  type Direction AcceptLanguage = 'Request


  parseFromHeaders _ = fmap AcceptLanguage . parseNegotiationHeaders "Accept-Language" listParser


  renderToHeaders _ = M.toStrictByteString . renderAcceptLanguage


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
