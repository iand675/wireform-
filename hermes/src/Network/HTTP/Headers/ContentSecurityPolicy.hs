{-# LANGUAGE TemplateHaskell #-}

{- |
W3C Content Security Policy Level 3 @Content-Security-Policy@ — the
response header by which a server declares the policy a user agent
must enforce for a given resource.

== Grammar

@
serialized-policy = directive-set
directive-set     = [ directive *( OWS \";\" OWS [ directive ] ) ]
directive         = directive-name [ RWS directive-value ]
directive-name    = 1*( ALPHA / DIGIT / \"-\" )
directive-value   = *( RWS / ( %x21-2B / %x2D-3A / %x3C-7E ) )
@

A policy is an ordered list of directives; each directive is a
name followed by zero or more whitespace-separated values. Values
are preserved verbatim (the value grammar is any VCHAR except
@\",\"@ and @\";\"@), so callers can route directive-specific
syntax (source lists, sandbox flags, report endpoints, …) through
their own logic. Directive order is preserved.

Spec: <https://www.w3.org/TR/CSP3/>

See also: "Network.HTTP.Headers.ContentSecurityPolicyReportOnly", "Network.HTTP.Headers.ReportingEndpoints", "Network.HTTP.Headers.XFrameOptions", "Network.HTTP.Headers.XXSSProtection", "Network.HTTP.Headers.PermissionsPolicy".
-}
module Network.HTTP.Headers.ContentSecurityPolicy (
  ContentSecurityPolicy (..),
  Directive (..),
  contentSecurityPolicyParser,
  renderContentSecurityPolicy,
  directiveParser,
  renderDirective,
  directiveSetParser,
  renderDirectiveSet,
) where

import Control.Monad.Combinators (sepBy)
import qualified Data.ByteString as B
import Data.CharSet (CharSet)
import qualified Data.CharSet as CharSet
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hContentSecurityPolicy)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- ---------------------------------------------------------------------------
-- Data
-- ---------------------------------------------------------------------------

{- | A single CSP directive: a @directive-name@ plus zero or more
whitespace-separated @directive-value@ tokens, preserved verbatim.
-}
data Directive = Directive
  { directiveName :: !ST.ShortText
  , directiveValues :: ![ST.ShortText]
  }
  deriving stock (Eq, Show)


-- | A serialized policy: an ordered list of directives.
newtype ContentSecurityPolicy = ContentSecurityPolicy
  { cspDirectives :: [Directive]
  }
  deriving stock (Eq, Show)


instance KnownHeader ContentSecurityPolicy where
  type ParseFailure ContentSecurityPolicy = String
  type Cardinality ContentSecurityPolicy = 'ZeroOrOne
  type Direction ContentSecurityPolicy = 'Response


  parseFromHeaders _ headers = case runParser contentSecurityPolicyParser (NE.head headers) of
    OK csp leftover
      | B.null (dropOws leftover) -> Right csp
      | otherwise ->
          Left ("Unconsumed input after parsing Content-Security-Policy: " <> show leftover)
    Fail -> Left "Failed to parse Content-Security-Policy header"
    Err err -> Left err
    where
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


  renderToHeaders _ = M.toStrictByteString . renderContentSecurityPolicy


  headerName _ = hContentSecurityPolicy


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

-- | @directive-name@ characters: @ALPHA / DIGIT / \"-\"@.
directiveNameCharSet :: CharSet
directiveNameCharSet =
  CharSet.range 'A' 'Z'
    <> CharSet.range 'a' 'z'
    <> CharSet.range '0' '9'
    <> CharSet.singleton '-'


{- | @directive-value@ characters: any VCHAR (@%x21-7E@) except
@\",\"@ (@%x2C@) and @\";\"@ (@%x3B@), which delimit, respectively,
multiple policies and multiple directives.
-}
cspValueCharSet :: CharSet
cspValueCharSet =
  CharSet.range '\x21' '\x2B'
    <> CharSet.range '\x2D' '\x3A'
    <> CharSet.range '\x3C' '\x7E'


-- | Parse a single @directive@: a name optionally followed by values.
directiveParser :: ParserT st String Directive
directiveParser = do
  name <- shortASCIIFromParser_ (some (satisfyAscii (`CharSet.member` directiveNameCharSet)))
  values <- many (rws *> cspValue)
  ows
  pure Directive {directiveName = name, directiveValues = values}
  where
    cspValue = shortASCIIFromParser_ (some (satisfyAscii (`CharSet.member` cspValueCharSet)))


{- | Parse a @directive-set@: a @\";\"@-separated list of directives,
tolerating empty directives (e.g. a trailing @\";\"@).
-}
directiveSetParser :: ParserT st String [Directive]
directiveSetParser = do
  ows
  ds <- optional directiveParser `sepBy` ($(char ';') *> ows)
  pure (catMaybes ds)


-- | Parse an entire @Content-Security-Policy@ value.
contentSecurityPolicyParser :: ParserT st String ContentSecurityPolicy
contentSecurityPolicyParser = ContentSecurityPolicy <$> directiveSetParser


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

-- | Render a single directive as @name value1 value2 …@.
renderDirective :: Directive -> M.Builder
renderDirective (Directive name values) =
  M.intersperse " " (R.shortText name : map R.shortText values)


-- | Render a directive list, joining directives with @\"; \"@.
renderDirectiveSet :: [Directive] -> M.Builder
renderDirectiveSet = M.intersperse "; " . map renderDirective


-- | Render an entire @Content-Security-Policy@ value.
renderContentSecurityPolicy :: ContentSecurityPolicy -> M.Builder
renderContentSecurityPolicy = renderDirectiveSet . cspDirectives
