{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

{- |
@X-Robots-Tag@ — de-facto response header letting an origin server apply
robots indexing directives to an individual resource (the HTTP-header
equivalent of the @robots@ @\<meta\>@ tag), e.g. @noindex, nofollow@ or
@googlebot: noindex@.

This header is __not__ IANA-registered. Its value is an optional leading
@user-agent ":"@ prefix followed by a non-empty, comma-separated list of
directives. A directive is either a bare keyword (@noindex@, @nofollow@,
@none@, @noarchive@, @nosnippet@, @noimageindex@, @notranslate@, ...) or a
@key ":" value@ pair for the handful of valued directives — @max-snippet@,
@max-image-preview@, @max-video-preview@, and @unavailable_after@.
Unavailable-after keeps its date and time spelling, including a weekday
comma when present. Each physical field line has its own bot scope.

Spec: no IANA registration (de-facto); see Google Search Central,
<https://developers.google.com/search/docs/crawling-indexing/robots-meta-tag>.

See also: "Network.HTTP.Headers.Link", "Network.HTTP.Headers.XPoweredBy".
-}
module Network.HTTP.Headers.XRobotsTag (
  XRobotsTag (XRobotsTag, xRobotsBotName, xRobotsDirectives),
  RobotsRuleGroup (..),
  RobotsDirective (..),
  xRobotsRuleGroups,
  xRobotsTagFromGroups,
  xRobotsTagParser,
  renderXRobotsTag,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXRobotsTag)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | A single robots directive: a keyword, optionally with a token value.
data RobotsDirective = RobotsDirective
  { robotsDirectiveName :: !ST.ShortText
  , robotsDirectiveValue :: !(Maybe ST.ShortText)
  }
  deriving stock (Eq, Show)


-- | One field line and its optional crawler scope.
data RobotsRuleGroup = RobotsRuleGroup
  { robotsGroupBotName :: !(Maybe ST.ShortText)
  , robotsGroupDirectives :: !(NonEmpty RobotsDirective)
  }
  deriving stock (Eq, Show)


-- | The existing record fields describe the first group.
data XRobotsTag
  = XRobotsTag
      { xRobotsBotName :: !(Maybe ST.ShortText)
      , xRobotsDirectives :: !(NonEmpty RobotsDirective)
      }
  | XRobotsTagWithMoreGroups
      { xRobotsBotName :: !(Maybe ST.ShortText)
      , xRobotsDirectives :: !(NonEmpty RobotsDirective)
      , xRobotsAdditionalGroups :: !(NonEmpty RobotsRuleGroup)
      }
  deriving stock (Eq, Show)


xRobotsRuleGroups :: XRobotsTag -> NonEmpty RobotsRuleGroup
xRobotsRuleGroups (XRobotsTag bot directives) = RobotsRuleGroup bot directives :| []
xRobotsRuleGroups value@XRobotsTagWithMoreGroups {} =
  RobotsRuleGroup (xRobotsBotName value) (xRobotsDirectives value) NE.<| xRobotsAdditionalGroups value


xRobotsTagFromGroups :: NonEmpty RobotsRuleGroup -> XRobotsTag
xRobotsTagFromGroups (RobotsRuleGroup bot directives :| rest) =
  case NE.nonEmpty rest of
    Nothing -> XRobotsTag bot directives
    Just additional -> XRobotsTagWithMoreGroups bot directives additional


instance KnownHeader XRobotsTag where
  type ParseFailure XRobotsTag = String
  type Cardinality XRobotsTag = 'ZeroOrMore
  type Direction XRobotsTag = 'Response


  parseFromHeaders _ (line :| []) = parseFieldLine "X-Robots-Tag" xRobotsTagParser line
  parseFromHeaders _ fields =
    xRobotsTagFromGroups <$> traverse (parseFieldLine "X-Robots-Tag" groupParser) fields


  renderToHeaders _ (XRobotsTag bot directives) = [M.toStrictByteString (renderGroup bot directives)]
  renderToHeaders _ (XRobotsTagWithMoreGroups bot directives rest) =
    M.toStrictByteString (renderGroup bot directives) : map renderRuleGroup (NE.toList rest)
    where
      renderRuleGroup (RobotsRuleGroup name rules) = M.toStrictByteString (renderGroup name rules)


  headerName _ = hXRobotsTag


xRobotsTagParser :: ParserT st String XRobotsTag
xRobotsTagParser = do
  RobotsRuleGroup bot directives <- groupParser
  pure (XRobotsTag bot directives)


groupParser :: ParserT st String RobotsRuleGroup
groupParser = do
  ows
  skipMany ($(char ',') *> ows)
  name <- rfc9110TokenBS
  ows
  let spelling = STU.fromByteStringUnsafe name
      first kind = do
        value <- directiveValue kind
        unscoped (RobotsDirective spelling (Just value))
  case directiveKind name of
    RobotsFlag ->
      branch $(char ':')
        (RobotsRuleGroup (Just spelling) <$> httpList1 "X-Robots-Tag" directive)
        (unscoped (RobotsDirective spelling Nothing))
    kind -> $(char ':') *> ows *> first kind
  where
    unscoped first = do
      ows
      rest <- withOption $(char ',') (const (httpList directive)) (pure [])
      pure (RobotsRuleGroup Nothing (first :| rest))
    directive = do
      name <- rfc9110TokenBS
      value <- case directiveKind name of
        RobotsFlag -> pure Nothing
        kind -> Just <$> (ows *> $(char ':') *> ows *> directiveValue kind)
      pure (RobotsDirective (STU.fromByteStringUnsafe name) value)


data DirectiveKind = RobotsFlag | RobotsToken | RobotsDate


directiveKind :: ByteString -> DirectiveKind
directiveKind input = case B.length input of
  11 | input == "max-snippet" || folded == "max-snippet" -> RobotsToken
  17
    | input == "unavailable_after" || folded == "unavailable_after" -> RobotsDate
    | folded == "max-image-preview" || folded == "max-video-preview" -> RobotsToken
  _ -> RobotsFlag
  where
    folded = CI.foldCase input


directiveValue :: DirectiveKind -> ParserT st String ST.ShortText
directiveValue RobotsDate = STU.fromByteStringUnsafe . B.dropWhileEnd isOws <$> byteStringOf scanDate
  where
    isOws w = w == 32 || w == 9
    scanDate = optional_ weekdayPrefix *> skipSome (satisfyAscii_ dateChar)
    dateChar c = c == '\t' || (c >= ' ' && c <= '~' && c /= ',')
    weekdayPrefix = do
      weekday <- CI.foldCase <$> rfc9110TokenBS
      if weekday `elem` ["mon", "tue", "wed", "thu", "fri", "sat", "sun", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"]
        then $(char ',') *> ows *> lookahead (satisfyAscii_ isDigit)
        else failed
directiveValue _ = rfc9110Token


-- | Render the first field line. Use 'renderToHeaders' for all scopes.
renderXRobotsTag :: XRobotsTag -> M.Builder
renderXRobotsTag value = renderGroup (xRobotsBotName value) (xRobotsDirectives value)


renderGroup :: Maybe ST.ShortText -> NonEmpty RobotsDirective -> M.Builder
renderGroup mBot (first :| rest) =
  prefix <> renderDirective first <> foldMap (\item -> ", " <> renderDirective item) rest
  where
    prefix = case mBot of
      Nothing -> mempty
      Just bot -> shortText bot <> ": "
    renderDirective (RobotsDirective name Nothing) = shortText name
    renderDirective (RobotsDirective name (Just v)) = shortText name <> ": " <> shortText v
