{-# LANGUAGE TemplateHaskell #-}

{- |
@Strict-Transport-Security@ (HSTS) — a response header by which a
host declares that user agents must only ever interact with it over
secure transport (HTTPS), for a duration of @max-age@ seconds.

== Grammar

@
Strict-Transport-Security = directive *( OWS ";" OWS directive )
directive                 = directive-name [ "=" directive-value ]
directive-name            = token
directive-value           = token / quoted-string
@

The defined directives are @max-age=<delta-seconds>@ (REQUIRED), and
the valueless flags @includeSubDomains@ and @preload@. Directive
names are case-insensitive; the first occurrence of @max-age@ wins
and unrecognised directives are ignored (RFC 6797 §6.1).

Spec: <https://www.rfc-editor.org/rfc/rfc6797#section-6.1>

See also: "Network.HTTP.Headers.PublicKeyPins", "Network.HTTP.Headers.ExpectCT", "Network.HTTP.Headers.ContentSecurityPolicy".
-}
module Network.HTTP.Headers.StrictTransportSecurity (
  StrictTransportSecurity (..),
  strictTransportSecurityParser,
  renderStrictTransportSecurity,
) where

import qualified Control.Applicative
import Control.Monad (void)
import qualified Data.ByteString as B
import Data.Char (toLower)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hStrictTransportSecurity)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | Parsed @Strict-Transport-Security@ policy (RFC 6797 §6.1).
data StrictTransportSecurity = StrictTransportSecurity
  { stsMaxAge :: !Word
  -- ^ @max-age@ delta-seconds: how long the host is to be treated as known-HSTS.
  , stsIncludeSubDomains :: !Bool
  -- ^ Whether the policy also applies to every subdomain of the host.
  , stsPreload :: !Bool
  -- ^ Non-standard but widely honoured @preload@ flag (HSTS preload lists).
  }
  deriving stock (Eq, Show)


instance KnownHeader StrictTransportSecurity where
  type ParseFailure StrictTransportSecurity = String
  type Cardinality StrictTransportSecurity = 'ZeroOrOne
  type Direction StrictTransportSecurity = 'Response


  parseFromHeaders _ headers = case runParser strictTransportSecurityParser (NE.head headers) of
    OK sts leftover
      | B.null (dropOws leftover) -> Right sts
      | otherwise ->
          Left ("Unconsumed input after parsing Strict-Transport-Security: " <> show leftover)
    Fail -> Left "Failed to parse Strict-Transport-Security header"
    Err err -> Left err
    where
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


  renderToHeaders _ = M.toStrictByteString . renderStrictTransportSecurity


  headerName _ = hStrictTransportSecurity


-- | One parsed directive (internal).
data Directive
  = DMaxAge !Word
  | DIncludeSubDomains
  | DPreload
  | DOther


strictTransportSecurityParser :: ParserT st String StrictTransportSecurity
strictTransportSecurityParser = do
  ows
  first <- directive
  rest <- many (ows *> $(char ';') *> ows *> directive)
  ows
  maybe failed pure (applyDirectives (first : rest))
  where
    directive = do
      name <- rfc9110Token
      case map toLower (ST.toString name) of
        "max-age" -> DMaxAge <$> (ows *> $(char '=') *> ows *> maxAgeValue)
        "includesubdomains" -> pure DIncludeSubDomains
        "preload" -> pure DPreload
        _ -> DOther <$ optional (ows *> $(char '=') *> ows *> directiveValue)
    maxAgeValue =
      ($(char '"') *> anyAsciiDecimalWord <* $(char '"'))
        <|> anyAsciiDecimalWord
    directiveValue = void quotedString <|> void rfc9110Token


{- | Fold parsed directives into the structured record. @max-age@ is
required (first occurrence wins); booleans accumulate.
-}
applyDirectives :: [Directive] -> Maybe StrictTransportSecurity
applyDirectives = go Nothing False False
  where
    go (Just ma) inc pre [] = Just (StrictTransportSecurity ma inc pre)
    go Nothing _ _ [] = Nothing
    go ma inc pre (d : ds) = case d of
      DMaxAge n -> go (ma Control.Applicative.<|> Just n) inc pre ds
      DIncludeSubDomains -> go ma True pre ds
      DPreload -> go ma inc True ds
      DOther -> go ma inc pre ds


renderStrictTransportSecurity :: StrictTransportSecurity -> M.Builder
renderStrictTransportSecurity (StrictTransportSecurity ma inc pre) =
  "max-age="
    <> M.wordDec ma
    <> (if inc then "; includeSubDomains" else mempty)
    <> (if pre then "; preload" else mempty)
