{-# LANGUAGE TemplateHaskell #-}

{- |
W3C @Permissions-Policy@ — a response header letting a server allow or deny
the use of browser features (geolocation, camera, fullscreen, …) in its own
frame and in embedded frames.

The value is an RFC 8941 structured-field __dictionary__: a comma-separated
list of @feature=allowlist@ members, where each allowlist is a parenthesised
inner-list whose members are the tokens @*@ (any origin), @self@
(the document's own origin), @src@ (the embedding @iframe@'s @src@ origin),
or quoted serialized origins such as @\"https:\/\/example.com\"@.

== Examples

@
Permissions-Policy: geolocation=(self \"https:\/\/example.com\"), camera=(), fullscreen=*
@

We model the value faithfully as an ordered list of @(feature, allowlist)@
pairs preserving member order. The bare-token shorthand (@camera=*@,
@geolocation=self@) is accepted on parse and normalised on render to the
canonical single-element inner-list form (@camera=(*)@).

Spec: <https://www.w3.org/TR/permissions-policy/> (serialization grammar:
<https://www.w3.org/TR/permissions-policy/#serialized-policy-directive>).

See also: "Network.HTTP.Headers.ContentSecurityPolicy", "Network.HTTP.Headers.CrossOriginEmbedderPolicy", "Network.HTTP.Headers.CrossOriginOpenerPolicy", "Network.HTTP.Headers.ReportingEndpoints".
-}
module Network.HTTP.Headers.PermissionsPolicy (
  PermissionsPolicy (..),
  AllowListItem (..),
  permissionsPolicyParser,
  renderPermissionsPolicy,
) where

import Control.Monad.Combinators (sepBy)
import qualified Data.ByteString as B
import Data.Char (isAsciiLower)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hPermissionsPolicy)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- | A single member of a feature's allowlist.
data AllowListItem
  = -- | The @*@ token: the feature is allowed in documents of any origin.
    AllowAll
  | -- | The @self@ token: allowed in same-origin documents.
    AllowSelf
  | -- | The @src@ token: allowed in the origin of the embedding @iframe@'s @src@.
    AllowSrc
  | -- | An explicit serialized origin, e.g. @\"https:\/\/example.com\"@.
    AllowOrigin !ST.ShortText
  deriving stock (Eq, Show)


{- | A @Permissions-Policy@ value: an ordered dictionary mapping each feature
name to its allowlist. Order is preserved in both directions.
-}
newtype PermissionsPolicy = PermissionsPolicy
  { permissionsPolicyDirectives :: [(ST.ShortText, [AllowListItem])]
  }
  deriving stock (Eq, Show)


instance KnownHeader PermissionsPolicy where
  type ParseFailure PermissionsPolicy = String
  type Cardinality PermissionsPolicy = 'ZeroOrMore
  type Direction PermissionsPolicy = 'Response


  parseFromHeaders _ headers =
    PermissionsPolicy . concatMap permissionsPolicyDirectives
      <$> traverse parseLine (NE.toList headers)
    where
      parseLine bs = case runParser permissionsPolicyParser bs of
        OK pp leftover
          | B.null (dropOws leftover) -> Right pp
          | otherwise ->
              Left ("Unconsumed input after parsing Permissions-Policy: " <> show leftover)
        Fail -> Left "Failed to parse Permissions-Policy header"
        Err e -> Left e
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


  renderToHeaders _ = pure . M.toStrictByteString . renderPermissionsPolicy


  headerName _ = hPermissionsPolicy


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

permissionsPolicyParser :: ParserT st String PermissionsPolicy
permissionsPolicyParser = do
  ows
  ds <- directive `sepBy` (ows *> $(char ',') *> ows)
  pure (PermissionsPolicy ds)
  where
    directive = do
      k <- sfKey
      $(char '=')
      items <- allowList
      pure (k, items)


-- | An RFC 8941 dictionary key: @( lcalpha \/ \"*\" ) *( lcalpha \/ DIGIT \/ \"_\" \/ \"-\" \/ \".\" \/ \"*\" )@.
sfKey :: ParserT st e ST.ShortText
sfKey = shortASCIIFromParser_ (firstChar *> skipMany restChar)
  where
    firstChar = skipSatisfyAscii (\c -> isAsciiLower c || c == '*')
    restChar = skipSatisfyAscii isKeyChar
    isKeyChar c =
      isAsciiLower c
        || isDigit c
        || c == '_'
        || c == '-'
        || c == '.'
        || c == '*'


-- | An inner-list allowlist @( item* )@, or the bare-token shorthand.
allowList :: ParserT st String [AllowListItem]
allowList = innerList <|> ((: []) <$> allowItem)
  where
    innerList = do
      $(char '(')
      ows
      items <- allowItem `sepBy` rws
      ows
      $(char ')')
      pure items


allowItem :: ParserT st String AllowListItem
allowItem =
  (AllowOrigin . unsafeToRFC8941String <$> rfc8941String) <|> keyword
  where
    keyword = do
      RFC8941Token t <- rfc8941Token
      case ST.toString t of
        "*" -> pure AllowAll
        "self" -> pure AllowSelf
        "src" -> pure AllowSrc
        _ -> failed


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderPermissionsPolicy :: PermissionsPolicy -> M.Builder
renderPermissionsPolicy (PermissionsPolicy ds) =
  M.intersperse ", " (map renderDirective ds)
  where
    renderDirective (k, items) =
      R.shortText k <> M.char7 '=' <> renderAllowList items
    renderAllowList items =
      M.char7 '(' <> M.intersperse (M.char7 ' ') (map renderItem items) <> M.char7 ')'
    renderItem = \case
      AllowAll -> "*"
      AllowSelf -> "self"
      AllowSrc -> "src"
      AllowOrigin o -> R.rfc8941String (RFC8941String o)
