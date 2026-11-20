{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

{- |
@Origin@ request header naming the origin (scheme, host, and optional port — but
never a path) that caused the request. Browsers attach it to CORS requests and to
all requests with unsafe methods (@POST@, @PUT@, etc.), letting the server decide
whether to honour the cross-origin access and what to return in
@Access-Control-Allow-Origin@. The serialized value may be the literal @null@ for
opaque origins (e.g. @data:@ URLs or sandboxed iframes). RFC 6454 also
defines an ordered, space-separated list of serialized origins. Fetch
uses the single-origin or null forms.

Spec: <https://www.rfc-editor.org/rfc/rfc6454#section-7>

See also: "Network.HTTP.Headers.AccessControlAllowOrigin", "Network.HTTP.Headers.AccessControlAllowCredentials", "Network.HTTP.Headers.Referer", "Network.HTTP.Headers.CrossOriginResourcePolicy", "Network.HTTP.Headers.Host".
-}
module Network.HTTP.Headers.Origin (
  Origin (..),
  OriginValue (..),
  originParser,
  renderOrigin,
) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Data.Word (Word16)
import Network.HTTP.Grammar.Authority (SerializedOrigin (..), renderSerializedOrigin, serializedOriginParser)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hOrigin)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | An opaque origin, one origin, or the RFC 6454 form with several origins.
data OriginValue
  = OriginNull
  | Origin
      { originScheme :: !ST.ShortText
      -- ^ e.g. "https"
      , originHost :: !ST.ShortText
      -- ^ e.g. "example.com"
      , originPort :: !(Maybe Word16)
      -- ^ Optional port
      }
  | OriginList !SerializedOrigin !(NonEmpty SerializedOrigin)
  deriving stock (Eq, Show)


-- | Origin header containing origin information
newtype Origin = OriginHeader {originValue :: OriginValue}
  deriving stock (Eq, Show)


instance KnownHeader Origin where
  type ParseFailure Origin = String
  type Cardinality Origin = 'ZeroOrOne
  type Direction Origin = 'Request


  parseFromHeaders _ = parseSingleFieldLine "Origin" originParser


  renderToHeaders _ = M.toStrictByteString . renderOrigin


  headerName _ = hOrigin


originParser :: ParserT st String Origin
originParser = OriginHeader <$> (origins <|> (OriginNull <$ $(string "null")))
  where
    origins = do
      first <- serializedOriginParser
      rest <- many ($(char ' ') *> serializedOriginParser)
      pure $ case NE.nonEmpty rest of
        Nothing -> case first of SerializedOrigin scheme host port -> Origin scheme host port
        Just others -> OriginList first others


renderOrigin :: Origin -> M.Builder
renderOrigin (OriginHeader OriginNull) = "null"
renderOrigin (OriginHeader (Origin scheme host port)) =
  renderSerializedOrigin (SerializedOrigin scheme host port)
renderOrigin (OriginHeader (OriginList first rest)) =
  renderSerializedOrigin first <> foldMap (\origin -> M.char7 ' ' <> renderSerializedOrigin origin) rest
