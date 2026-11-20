{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
@Strict-Transport-Security@ (HSTS) — a response header by which a
host declares that user agents must only ever interact with it over
secure transport (HTTPS), for a duration of @max-age@ seconds.

== Grammar

@
Strict-Transport-Security = [ directive ] *( OWS ";" OWS [ directive ] )
directive                 = directive-name [ "=" directive-value ]
directive-name            = token
directive-value           = token / quoted-string
@

The defined directives are @max-age=<delta-seconds>@ (REQUIRED), and
the valueless @includeSubDomains@ flag. The @preload@ extension is
also supported. Directive names are case-insensitive and unique.
Unknown directives are ignored.

Spec: <https://www.rfc-editor.org/rfc/rfc6797#section-6.1>

See also: "Network.HTTP.Headers.PublicKeyPins", "Network.HTTP.Headers.ExpectCT", "Network.HTTP.Headers.ContentSecurityPolicy".
-}
module Network.HTTP.Headers.StrictTransportSecurity (
  StrictTransportSecurity (..),
  strictTransportSecurityParser,
  renderStrictTransportSecurity,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.CaseInsensitive (CI)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hStrictTransportSecurity)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.TransportPolicy (directiveEnd, extension, policyAge, valueless)


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


  -- RFC 6797 section 8.1 uses the first occurrence of this field.
  parseFromHeaders _ = parseSTSField . NE.head


  renderToHeaders _ = M.toStrictByteString . renderStrictTransportSecurity


  headerName _ = hStrictTransportSecurity


data Directive
  = MaxAge !Word
  | IncludeSubDomains
  | Preload
  | Extension !(CI ByteString)


parseSTSField :: ByteString -> Either String StrictTransportSecurity
parseSTSField wire
  | BS.length wire <= BS.length prefix + 9
  , Just digits <- BS.stripPrefix prefix wire =
      withShortDecimal digits (\n -> Right (StrictTransportSecurity (fromIntegral n) False False)) fallback
  | otherwise = fallback
  where
    prefix = "max-age="
    fallback = parseFieldLine "Strict-Transport-Security" strictTransportSecurityParser wire


strictTransportSecurityParser :: ParserT st String StrictTransportSecurity
strictTransportSecurityParser =
  listWithEmptySlots $(char ';') directive >>= applyDirectives
  where
    directive =
      $( switchAsciiCI
           [|
             case _ of
               "max-age" -> known (MaxAge <$> policyAge)
               "includesubdomains" -> known (IncludeSubDomains <$ valueless)
               "preload" -> known (Preload <$ valueless)
             |]
       )
        <|> (Extension <$> extension)
    known action = directiveEnd *> (action <|> err "Invalid Strict-Transport-Security directive")


applyDirectives :: [Directive] -> ParserT st String StrictTransportSecurity
applyDirectives = go Nothing False False Set.empty
  where
    go !age !subdomains !preload !seen = \case
      [] -> case age of
        Just n -> pure (StrictTransportSecurity n subdomains preload)
        Nothing -> err "Strict-Transport-Security requires max-age"
      d : ds -> case d of
        MaxAge n -> case age of
          Just _ -> err "Duplicate max-age directive"
          Nothing -> go (Just n) subdomains preload seen ds
        IncludeSubDomains
          | subdomains -> err "Duplicate includeSubDomains directive"
          | otherwise -> go age True preload seen ds
        Preload
          | preload -> err "Duplicate preload directive"
          | otherwise -> go age subdomains True seen ds
        Extension name
          | Set.member name seen -> err "Duplicate Strict-Transport-Security extension"
          | otherwise -> go age subdomains preload (Set.insert name seen) ds


renderStrictTransportSecurity :: StrictTransportSecurity -> M.Builder
renderStrictTransportSecurity (StrictTransportSecurity ma inc pre) =
  "max-age="
    <> M.wordDec ma
    <> (if inc then "; includeSubDomains" else mempty)
    <> (if pre then "; preload" else mempty)
