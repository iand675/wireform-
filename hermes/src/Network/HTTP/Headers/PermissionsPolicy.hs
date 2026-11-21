{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{- |
@Permissions-Policy@ declares where browser features may be used.
The field is an ordered Structured Fields Dictionary. The codec retains
unknown members and parameters. 'permissionsPolicyDeclarations' exposes
the HTTP allowlists and typed @report-to@ endpoints.

<https://www.w3.org/TR/permissions-policy/#structured-header-serialization W3C Permissions Policy §5.2>
-}
module Network.HTTP.Headers.PermissionsPolicy (
  PermissionsPolicy (PermissionsPolicy, PermissionsPolicyDictionary, permissionsPolicyDirectives, permissionsPolicyDictionary),
  PermissionsPolicyDeclaration (..),
  AllowListItem (..),
  permissionsPolicyDeclarations,
  permissionsPolicyParser,
  renderPermissionsPolicy,
  renderPermissionsPolicyChecked,
) where

import qualified Data.CharSet as CS
import Data.Either (isRight)
import Data.Maybe (mapMaybe)
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.Authority (pctEncoded, uriSchemeBytes)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hPermissionsPolicy)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R
import Wireform.Parser.Driver (parseByteString)


-- | An allowlist member.
data AllowListItem
  = -- | The @*@ Token matches every origin.
    AllowAll
  | -- | The @self@ Token refers to the document's origin.
    AllowSelf
  | -- | The @src@ keyword used by iframe policies and the legacy view.
    AllowSrc
  | -- | The spelling of a permissions source expression.
    AllowOrigin !ST.ShortText
  deriving stock (Eq, Show)


-- | A feature's allowlist and optional reporting endpoint.
data PermissionsPolicyDeclaration = PermissionsPolicyDeclaration
  { permissionsPolicyFeature :: !ST.ShortText
  , permissionsPolicyAllowlist :: !(Maybe [AllowListItem])
  -- ^ An empty list denies the feature. 'Nothing' denotes an unsupported member value.
  , permissionsPolicyReportTo :: !(Maybe RFC8941Token)
  }
  deriving stock (Eq, Show)


-- | The normalized dictionary, including members unknown to this module.
newtype PermissionsPolicy = PermissionsPolicyDictionary
  { permissionsPolicyDictionary :: [StructuredFieldDictionaryEntry]
  }
  deriving stock (Eq, Show)


{- | The original allowlist view and constructor. Construction emits
inner lists. Use 'permissionsPolicyDictionary' to retain member forms
and extension parameters, or 'permissionsPolicyDeclarations' for the
HTTP allowlist and reporting projection.
-}
pattern PermissionsPolicy :: [(ST.ShortText, [AllowListItem])] -> PermissionsPolicy
pattern PermissionsPolicy {permissionsPolicyDirectives} <- (legacyDirectives -> permissionsPolicyDirectives)
  where
    PermissionsPolicy directives =
      PermissionsPolicyDictionary (orderedFieldEntries structuredDictionaryKey (map fromDirective directives))
      where
        fromDirective (feature, items) = StructuredFieldDictionaryEntry feature (StructuredInnerList (map fromItem items) [])
        fromItem item = StructuredFieldItem (bare item) []
        bare AllowAll = Token (RFC8941Token "*")
        bare AllowSelf = Token (RFC8941Token "self")
        bare AllowSrc = Token (RFC8941Token "src")
        bare (AllowOrigin expression) = String (RFC8941String expression)


{-# COMPLETE PermissionsPolicy #-}


instance KnownHeader PermissionsPolicy where
  type ParseFailure PermissionsPolicy = String
  type Cardinality PermissionsPolicy = 'ZeroOrMore
  type Direction PermissionsPolicy = 'Response


  parseFromHeaders _ = parseStructuredFieldLines "Permissions-Policy" permissionsPolicyParser


  renderToHeaders _ (PermissionsPolicyDictionary []) = []
  renderToHeaders _ policy = either (const []) (pure . M.toStrictByteString) (renderPermissionsPolicyChecked policy)


  omitHeaderValue = null . permissionsPolicyDictionary
  {-# INLINE omitHeaderValue #-}


  headerName _ = hPermissionsPolicy


permissionsPolicyParser :: ParserT st String PermissionsPolicy
permissionsPolicyParser = PermissionsPolicyDictionary <$> (ows *> structuredFieldDictionary)


-- | Render the field value. An invalid dictionary produces no bytes.
renderPermissionsPolicy :: PermissionsPolicy -> M.Builder
renderPermissionsPolicy = either (const mempty) id . renderPermissionsPolicyChecked


-- | Validate and normalize the Structured Fields dictionary for emission.
renderPermissionsPolicyChecked :: PermissionsPolicy -> Either String M.Builder
renderPermissionsPolicyChecked = R.renderStructuredFieldDictionaryChecked . permissionsPolicyDictionary


{- | Project the allowlist and a Token-valued @report-to@ parameter for
each feature. Feature recognition and origin matching depend on the
recipient's environment.
-}
permissionsPolicyDeclarations :: PermissionsPolicy -> [PermissionsPolicyDeclaration]
permissionsPolicyDeclarations = map declaration . normalizedPolicyEntries
  where
    declaration (StructuredFieldDictionaryEntry feature member) =
      PermissionsPolicyDeclaration feature (projectAllowlist allowItem member) (reportTo (parameters member))
    parameters (StructuredItem (StructuredFieldItem _ params)) = params
    parameters (StructuredInnerList _ params) = params
    reportTo = foldl' readParameter Nothing
    readParameter _ (StructuredFieldParameter "report-to" (Token endpoint)) = Just endpoint
    readParameter _ (StructuredFieldParameter "report-to" _) = Nothing
    readParameter current _ = current


normalizedPolicyEntries :: PermissionsPolicy -> [StructuredFieldDictionaryEntry]
normalizedPolicyEntries = orderedFieldEntries structuredDictionaryKey . permissionsPolicyDictionary
{-# INLINE normalizedPolicyEntries #-}


legacyDirectives :: PermissionsPolicy -> [(ST.ShortText, [AllowListItem])]
legacyDirectives = mapMaybe directive . normalizedPolicyEntries
  where
    directive (StructuredFieldDictionaryEntry feature member) = fmap (\allowed -> (feature, allowed)) (projectAllowlist legacyItem member)
    legacyItem (Token (RFC8941Token "src")) = Just AllowSrc
    legacyItem item = allowItem item


projectAllowlist :: (ItemValue -> Maybe AllowListItem) -> StructuredFieldMember -> Maybe [AllowListItem]
projectAllowlist item (StructuredItem (StructuredFieldItem value _)) = (: []) <$> item value
projectAllowlist item (StructuredInnerList values _) = Just (mapMaybe (item . structuredItemValue) values)


allowItem :: ItemValue -> Maybe AllowListItem
allowItem (Token (RFC8941Token "*")) = Just AllowAll
allowItem (Token (RFC8941Token "self")) = Just AllowSelf
allowItem (String (RFC8941String source))
  | isRight (parseByteString sourceExpression (ST.toByteString source)) = Just (AllowOrigin source)
allowItem _ = Nothing


-- CSP3 scheme-source or host-source, as referenced by Permissions Policy.
sourceExpression :: ParserT st String ()
sourceExpression = (uriSchemeBytes *> $(char ':') <* eof) <|> (hostSource <* eof)
  where
    hostSource = optional_ (uriSchemeBytes *> $(string "://")) *> hostPart *> optional_ port *> optional_ path
    hostPart = (optional_ $(string "*.") *> label *> skipMany ($(char '.') *> label) *> optional_ $(char '.')) <|> $(char '*')
    label = skipSome (skipSatisfyAscii (\c -> isLatinLetter c || isDigit c || c == '-'))
    port = $(char ':') *> ($(char '*') <|> skipSome (skipSatisfyAscii isDigit))
    path = $(char '/') *> notFollowedBy $(char '/') *> skipMany (pctEncoded <|> skipSatisfyAscii sourcePathChar)


sourcePathChar :: Char -> Bool
sourcePathChar = case sourcePathChars of
  AsciiSet low high -> asciiMember low high
{-# INLINE sourcePathChar #-}


sourcePathChars :: AsciiSet
sourcePathChars = asciiSet (CS.range 'a' 'z' <> CS.range 'A' 'Z' <> CS.range '0' '9' <> "-._~!$&'()*+=:@/")
{-# NOINLINE sourcePathChars #-}
