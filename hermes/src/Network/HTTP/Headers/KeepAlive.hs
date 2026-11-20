{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{- |
@Keep-Alive@ — a request and response header that hints how a persistent
connection should be managed, carrying a @timeout@ (minimum idle seconds
the connection should stay open) and\/or @max@ (maximum further requests
the connection will accept before closing). It accompanies
@Connection: keep-alive@ on HTTP\/1.x. HTTP\/2 and HTTP\/3 prohibit
this hop-by-hop field.

== Grammar

@
Keep-Alive       = #keep-alive-param
keep-alive-param = token [ "=" ( token \/ quoted-string ) ]
@

The codec accepts the historical empty list from RFC 2068 §19.7.1.1
and the extension forms from the HTTP Keep-Alive draft.
<https://datatracker.ietf.org/doc/html/draft-thomson-hybi-http-timeout-03#section-2>

See also: "Network.HTTP.Headers.Connection", "Network.HTTP.Headers.Close".
-}
module Network.HTTP.Headers.KeepAlive (
  KeepAlive (KeepAlive, keepAliveParams),
  KeepAliveParameter (..),
  keepAliveParameters,
  keepAliveFromParameters,
  keepAliveTimeout,
  keepAliveMaxRequests,
  keepAliveParser,
  renderKeepAlive,
) where

import Data.Bits ((.|.))
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBSI
import Data.Maybe (isNothing, mapMaybe)
import qualified Data.Text.Short as ST
import Data.Word (Word64)
import Network.HTTP.Grammar.Rendering (encodeHttpParameterList, renderHttpParameterList)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hKeepAlive)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | The parameters in wire order. Quoted values are decoded with a
-- one-to-one Latin-1 mapping for their wire octets.
newtype KeepAlive = KeepAliveFields {keepAliveParameters :: [KeepAliveParameter]}
  deriving stock (Eq)


-- | A parameter can be a flag or carry a value, including a quoted empty value.
data KeepAliveParameter = KeepAliveParameter
  { keepAliveParameterName :: !ST.ShortText
  , keepAliveParameterValue :: !(Maybe ST.ShortText)
  }
  deriving stock (Eq, Show)


-- | Compatibility view of assigned parameters. Use 'keepAliveParameters'
-- to inspect both flags and assigned parameters in order.
pattern KeepAlive :: [(ST.ShortText, ST.ShortText)] -> KeepAlive
pattern KeepAlive {keepAliveParams} <- (assignedParameters -> keepAliveParams)
  where
    KeepAlive pairs = KeepAliveFields (map (\(name, value) -> KeepAliveParameter name (Just value)) pairs)


{-# COMPLETE KeepAlive #-}


assignedParameters :: KeepAlive -> [(ST.ShortText, ST.ShortText)]
assignedParameters = mapMaybe assigned . keepAliveParameters
  where
    assigned (KeepAliveParameter name value) = fmap (\v -> (name, v)) value
{-# INLINE assignedParameters #-}


instance Show KeepAlive where
  showsPrec p value = showParen (p > 10) $
    if any (isNothing . keepAliveParameterValue) (keepAliveParameters value)
      then showString "KeepAlive {keepAliveParameters = " . shows (keepAliveParameters value) . showChar '}'
      else showString "KeepAlive {keepAliveParams = " . shows (keepAliveParams value) . showChar '}'


keepAliveFromParameters :: [KeepAliveParameter] -> KeepAlive
keepAliveFromParameters = KeepAliveFields


-- | Read one case-insensitive idle-timeout hint in seconds.
keepAliveTimeout :: KeepAlive -> Either String (Maybe Word64)
keepAliveTimeout = readLimit "timeout"


-- | Read one case-insensitive legacy request-count limit.
keepAliveMaxRequests :: KeepAlive -> Either String (Maybe Word64)
keepAliveMaxRequests = readLimit "max"


readLimit :: ST.ShortText -> KeepAlive -> Either String (Maybe Word64)
readLimit name = parseLimit . filter (matches . keepAliveParameterName) . keepAliveParameters
  where
    expected = ST.toShortByteString name
    len = SBS.length expected
    -- The known limit names contain only lowercase ASCII letters.
    matches key = name == key || (SBS.length bytes == len && folded 0)
      where
        bytes = ST.toShortByteString key
        folded !i =
          i == len
            || ((SBSI.unsafeIndex bytes i .|. 32) == SBSI.unsafeIndex expected i && folded (i + 1))
    parseLimit [] = Right Nothing
    parseLimit [KeepAliveParameter _ (Just value)] =
      let wire = ST.toByteString value
          fallback = Just <$> parseFieldLine "Keep-Alive limit"
            (boundedDecimalWord64 (err "Keep-Alive limit exceeds Word64")) wire
      in withShortDecimal wire (Right . Just . fromIntegral) fallback
    parseLimit [KeepAliveParameter _ Nothing] = Left "Keep-Alive limit requires a decimal value"
    parseLimit _ = Left "Repeated Keep-Alive limit"


instance KnownHeader KeepAlive where
  type ParseFailure KeepAlive = String
  type Cardinality KeepAlive = 'ZeroOrOne
  type Direction KeepAlive = 'RequestAndResponse


  parseFromHeaders _ = parseFieldLine "Keep-Alive" keepAliveParser . combineFieldLines ", "


  renderToHeaders _ =
    encodeHttpParameterList keepAliveParameterName keepAliveParameterValue . keepAliveParameters


  headerName _ = hKeepAlive


keepAliveParser :: ParserT st String KeepAlive
keepAliveParser = KeepAliveFields <$> httpList keepAliveParam


keepAliveParam :: ParserT st e KeepAliveParameter
keepAliveParam = do
  key <- rfc9110Token
  value <- branch $(char '=') (Just <$> valueParser) (pure Nothing)
  pure (KeepAliveParameter key value)
  where
    valueParser = rfc9110Token <|> (shortTextFromLatin1 <$> httpQuotedBytes)


renderKeepAlive :: KeepAlive -> M.Builder
renderKeepAlive =
  renderHttpParameterList keepAliveParameterName keepAliveParameterValue . keepAliveParameters
