{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{- |
@Content-Security-Policy@ carries an ordered list of independent policies.
Use 'cspPolicies' to access the whole field and 'effectiveDirectives' to
project the first occurrence of each directive within one policy.

<https://www.w3.org/TR/CSP3/#csp-header Content Security Policy Level 3>

See also: "Network.HTTP.Headers.ContentSecurityPolicyReportOnly",
"Network.HTTP.Headers.ReportingEndpoints", "Network.HTTP.Headers.PermissionsPolicy".
-}
module Network.HTTP.Headers.ContentSecurityPolicy (
  ContentSecurityPolicy (ContentSecurityPolicy, cspDirectives, ContentSecurityPolicyList, cspPolicies),
  CSPPolicy (..),
  Directive (..),
  effectiveDirectives,
  contentSecurityPolicyParser,
  renderContentSecurityPolicy,
  directiveParser,
  renderDirective,
  directiveSetParser,
  renderDirectiveSet,
  policyListParser,
  parsePolicyFieldLines,
  renderPolicyList,
) where

import Data.ByteString (ByteString)
import Data.Char (isAsciiUpper, toLower)
import qualified Data.CharSet as CharSet
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hContentSecurityPolicy)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- | A directive's wire spelling and its whitespace-separated tokens.
data Directive = Directive
  { directiveName :: !ST.ShortText
  , directiveValues :: ![ST.ShortText]
  }
  deriving stock (Eq, Show)


-- | One policy, retaining all serialized directives in wire order.
newtype CSPPolicy = CSPPolicy {policyDirectives :: [Directive]}
  deriving stock (Eq, Show)


newtype ContentSecurityPolicy = ContentSecurityPolicyList {cspPolicies :: NonEmpty CSPPolicy}
  deriving stock (Eq, Show)


{- | Construct one policy or read the first policy's directives. Use
'cspPolicies' to inspect a field that may contain multiple policies.
-}
pattern ContentSecurityPolicy :: [Directive] -> ContentSecurityPolicy
pattern ContentSecurityPolicy {cspDirectives} <- (policyDirectives . NE.head . cspPolicies -> cspDirectives)
  where
    ContentSecurityPolicy directives = ContentSecurityPolicyList (CSPPolicy directives :| [])


{-# COMPLETE ContentSecurityPolicy #-}


{- | CSP3 normalizes directive names and uses the first value of each name.
Apply this within a single policy so directives from different policies
remain independent.
-}
effectiveDirectives :: CSPPolicy -> [Directive]
effectiveDirectives = go Set.empty . policyDirectives
  where
    go _ [] = []
    go seen (directive : rest)
      | Set.member name seen = go seen rest
      | otherwise = directive {directiveName = name} : go (Set.insert name seen) rest
      where
        spelling = directiveName directive
        name
          | ST.any isAsciiUpper spelling = ST.fromText (T.map lowerAscii (ST.toText spelling))
          | otherwise = spelling
        lowerAscii c
          | isAsciiUpper c = toLower c
          | otherwise = c


instance KnownHeader ContentSecurityPolicy where
  type ParseFailure ContentSecurityPolicy = String
  type Cardinality ContentSecurityPolicy = 'ZeroOrOne
  type Direction ContentSecurityPolicy = 'Response


  parseFromHeaders _ = fmap ContentSecurityPolicyList . parsePolicyFieldLines "Content-Security-Policy"


  renderToHeaders _ = M.toStrictByteString . renderContentSecurityPolicy


  headerName _ = hContentSecurityPolicy


contentSecurityPolicyParser :: ParserT st String ContentSecurityPolicy
contentSecurityPolicyParser = ContentSecurityPolicyList <$> policyListParser


parsePolicyFieldLines :: String -> NonEmpty ByteString -> Either String (NonEmpty CSPPolicy)
parsePolicyFieldLines name = fmap explicitEmptyPolicy . parseListFieldLines name nonemptyPoliciesParser


policyListParser :: ParserT st String (NonEmpty CSPPolicy)
policyListParser = explicitEmptyPolicy <$> nonemptyPoliciesParser


explicitEmptyPolicy :: [CSPPolicy] -> NonEmpty CSPPolicy
explicitEmptyPolicy [] = CSPPolicy [] :| []
explicitEmptyPolicy (first : rest) = first :| rest


-- HTTP field values admit SP and HTAB. Commas always delimit policies and
-- never occur inside a directive, so physical lines can be parsed separately.
nonemptyPoliciesParser :: ParserT st String [CSPPolicy]
nonemptyPoliciesParser = do
  directives <- directiveSetWith isOWS
  rest <- branch $(char ',') nonemptyPoliciesParser (pure [])
  pure $ case directives of
    [] -> rest
    _ -> CSPPolicy directives : rest


-- | Read one serialized CSP directive using Infra ASCII whitespace.
directiveParser :: ParserT st String Directive
directiveParser = directiveWith isCSPWhitespace


-- | Read a single serialized policy, including policies from HTML sources.
directiveSetParser :: ParserT st String [Directive]
directiveSetParser = directiveSetWith isCSPWhitespace


isCSPWhitespace :: Char -> Bool
isCSPWhitespace c = isOWS c || c == '\n' || c == '\r' || c == '\f'


directiveSetWith :: (Char -> Bool) -> ParserT st String [Directive]
directiveSetWith isSpace = space *> go
  where
    space = skipMany (skipSatisfyAscii isSpace)
    separator = $(char ';') *> space
    go =
      branch separator go $
        withOption
          (directiveWith isSpace)
          (\directive -> (directive :) <$> branch separator go (pure []))
          (pure [])
{-# INLINE directiveSetWith #-}


directiveWith :: (Char -> Bool) -> ParserT st String Directive
directiveWith isSpace = do
  name <- shortASCIIFromParser_ (asciiSetBytes directiveNameChars)
  values <- branch (skipSome space) (many (value <* skipMany space)) (pure [])
  pure (Directive name values)
  where
    space = skipSatisfyAscii isSpace
    value = shortASCIIFromParser_ (asciiSetBytes directiveValueChars)
{-# INLINE directiveWith #-}


directiveNameChars :: AsciiSet
directiveNameChars = asciiSet (CharSet.range 'A' 'Z' <> CharSet.range 'a' 'z' <> CharSet.range '0' '9' <> "-")
{-# NOINLINE directiveNameChars #-}


directiveValueChars :: AsciiSet
directiveValueChars = asciiSet (CharSet.range '\x21' '\x2b' <> CharSet.range '\x2d' '\x3a' <> CharSet.range '\x3c' '\x7e')
{-# NOINLINE directiveValueChars #-}


renderDirective :: Directive -> M.Builder
renderDirective (Directive name values) = R.shortText name <> foldMap (\value -> " " <> R.shortText value) values


renderDirectiveSet :: [Directive] -> M.Builder
renderDirectiveSet = R.renderSeparated "; " renderDirective


renderPolicyList :: NonEmpty CSPPolicy -> M.Builder
renderPolicyList = R.renderSeparated ", " (renderDirectiveSet . policyDirectives) . filter (not . null . policyDirectives) . NE.toList


renderContentSecurityPolicy :: ContentSecurityPolicy -> M.Builder
renderContentSecurityPolicy = renderPolicyList . cspPolicies
