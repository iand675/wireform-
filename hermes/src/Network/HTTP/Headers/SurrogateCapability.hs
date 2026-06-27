{-# LANGUAGE TemplateHaskell #-}

{- |
@Surrogate-Capability@ — Edge Architecture (Surrogate/1.0) request header by
which a surrogate (edge cache \/ CDN node) advertises to the origin server the
processing capabilities it offers.

== Grammar (Edge Architecture Specification §2.1)

@
Surrogate-Capability = 1#capability
capability           = device-token \"=\" DQUOTE capability-set DQUOTE
device-token         = token
capability-set       = capability *( SP capability )      ; opaque, kept raw
@

Each entry pairs a @device-token@ (a unique surrogate identifier) with a
quoted capability set such as @\"Surrogate\/1.0 ESI\/1.0\"@.  The capability
set is host-specific, so it is preserved verbatim ('Data.Text.Short.ShortText')
rather than decomposed into individual @name\/version@ tokens — light structure
over a raw value.

See <https://www.w3.org/TR/edge-arch> for the specification.

See also: "Network.HTTP.Headers.SurrogateControl", "Network.HTTP.Headers.CDNCacheControl", "Network.HTTP.Headers.CacheControl".
-}
module Network.HTTP.Headers.SurrogateCapability (
  SurrogateCapability (..),
  SurrogateCapabilityEntry (..),
  surrogateCapabilityParser,
  renderSurrogateCapability,
) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSurrogateCapability)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | A @Surrogate-Capability@ value: a non-empty list of capability entries.
newtype SurrogateCapability = SurrogateCapability
  { surrogateCapabilityEntries :: NonEmpty SurrogateCapabilityEntry
  }
  deriving stock (Eq, Show)


{- | A single @device-token=\"capability-set\"@ pairing.  The capability set is
stored as the raw (unquoted) quote contents and is re-quoted on render.
-}
data SurrogateCapabilityEntry = SurrogateCapabilityEntry
  { surrogateCapabilityDevice :: !ST.ShortText
  -- ^ The device token identifying the surrogate.
  , surrogateCapabilitySet :: !ST.ShortText
  -- ^ The opaque, (usually space-separated) capability set.
  }
  deriving stock (Eq, Show)


instance KnownHeader SurrogateCapability where
  type ParseFailure SurrogateCapability = String
  type Cardinality SurrogateCapability = 'ZeroOrOne
  type Direction SurrogateCapability = 'Request


  parseFromHeaders _ headers = case runParser surrogateCapabilityParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Surrogate-Capability header: " <> show rest
    Fail -> Left "Failed to parse Surrogate-Capability header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderSurrogateCapability


  headerName _ = hSurrogateCapability


surrogateCapabilityParser :: ParserT st String SurrogateCapability
surrogateCapabilityParser = do
  ows
  first <- entryParser
  rest <- many (ows *> $(char ',') *> ows *> entryParser)
  ows
  pure (SurrogateCapability (first :| rest))


entryParser :: ParserT st String SurrogateCapabilityEntry
entryParser = do
  device <- rfc9110Token
  $(char '=')
  value <- quotedString <|> rfc9110Token
  pure (SurrogateCapabilityEntry device value)


renderSurrogateCapability :: SurrogateCapability -> M.Builder
renderSurrogateCapability (SurrogateCapability entries) =
  M.intersperse ", " (map renderEntry (NE.toList entries))


renderEntry :: SurrogateCapabilityEntry -> M.Builder
renderEntry (SurrogateCapabilityEntry device value) =
  shortText device <> M.char7 '=' <> renderQuotedString value


-- | Render a quoted-string, escaping embedded DQUOTE and backslash.
renderQuotedString :: ST.ShortText -> M.Builder
renderQuotedString t = M.char7 '"' <> ST.foldl' escape mempty t <> M.char7 '"'
  where
    escape b = \case
      '"' -> b <> M.char7 '\\' <> M.char7 '"'
      '\\' -> b <> M.char7 '\\' <> M.char7 '\\'
      c -> b <> M.char7 c
