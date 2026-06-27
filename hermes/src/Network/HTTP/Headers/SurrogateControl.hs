{-# LANGUAGE TemplateHaskell #-}

{- |
@Surrogate-Control@ — Edge Architecture (Surrogate/1.0) response header by
which an origin server controls the behaviour of downstream surrogates
(edge caches \/ CDN nodes), analogous to @Cache-Control@.

== Grammar (Edge Architecture Specification §2.2)

@
Surrogate-Control = 1#directive
directive         = directive-name [ \"=\" ( token \/ quoted-string ) ]
directive-name    = token
@

Common directives include @max-age@, @no-store@, @no-store-remote@ and
@content=\"ESI\/1.0\"@.  Each directive is preserved as a name plus an optional
raw value (light structure over a raw value); on render the value is emitted
bare when it is a valid token and quoted otherwise.

See <https://www.w3.org/TR/edge-arch> for the specification.

See also: "Network.HTTP.Headers.SurrogateCapability", "Network.HTTP.Headers.CDNCacheControl", "Network.HTTP.Headers.CacheControl".
-}
module Network.HTTP.Headers.SurrogateControl (
  SurrogateControl (..),
  SurrogateControlDirective (..),
  surrogateControlParser,
  renderSurrogateControl,
) where

import Control.Monad.Combinators (option)
import qualified Data.CharSet as CharSet
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSurrogateControl)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | A @Surrogate-Control@ value: a non-empty list of control directives.
newtype SurrogateControl = SurrogateControl
  { surrogateControlDirectives :: NonEmpty SurrogateControlDirective
  }
  deriving stock (Eq, Show)


-- | A single control directive: a name with an optional value.
data SurrogateControlDirective = SurrogateControlDirective
  { surrogateControlName :: !ST.ShortText
  -- ^ The directive name (token), e.g. @max-age@ or @no-store@.
  , surrogateControlValue :: !(Maybe ST.ShortText)
  -- ^ The optional directive value (raw token-or-quoted contents).
  }
  deriving stock (Eq, Show)


instance KnownHeader SurrogateControl where
  type ParseFailure SurrogateControl = String
  type Cardinality SurrogateControl = 'ZeroOrOne
  type Direction SurrogateControl = 'Response


  parseFromHeaders _ headers = case runParser surrogateControlParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Surrogate-Control header: " <> show rest
    Fail -> Left "Failed to parse Surrogate-Control header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderSurrogateControl


  headerName _ = hSurrogateControl


surrogateControlParser :: ParserT st String SurrogateControl
surrogateControlParser = do
  ows
  first <- directiveParser
  rest <- many (ows *> $(char ',') *> ows *> directiveParser)
  ows
  pure (SurrogateControl (first :| rest))


directiveParser :: ParserT st String SurrogateControlDirective
directiveParser = do
  name <- rfc9110Token
  value <- option Nothing (Just <$> ($(char '=') *> (rfc9110Token <|> quotedString)))
  pure (SurrogateControlDirective name value)


renderSurrogateControl :: SurrogateControl -> M.Builder
renderSurrogateControl (SurrogateControl directives) =
  M.intersperse ", " (map renderDirective (NE.toList directives))


renderDirective :: SurrogateControlDirective -> M.Builder
renderDirective (SurrogateControlDirective name value) =
  shortText name <> case value of
    Nothing -> mempty
    Just v
      | isToken v -> M.char7 '=' <> shortText v
      | otherwise -> M.char7 '=' <> renderQuotedString v


-- | True when the text is a non-empty RFC 9110 token (so it needs no quoting).
isToken :: ST.ShortText -> Bool
isToken t = not (ST.null t) && ST.all (`CharSet.member` tokenCharSet) t


-- | Render a quoted-string, escaping embedded DQUOTE and backslash.
renderQuotedString :: ST.ShortText -> M.Builder
renderQuotedString t = M.char7 '"' <> ST.foldl' escape mempty t <> M.char7 '"'
  where
    escape b = \case
      '"' -> b <> M.char7 '\\' <> M.char7 '"'
      '\\' -> b <> M.char7 '\\' <> M.char7 '\\'
      c -> b <> M.char7 c
