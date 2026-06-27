{-# LANGUAGE TemplateHaskell #-}

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
@max-image-preview@, @max-video-preview@, and @unavailable_after@ — whose
value is captured as a single token (a date/time value spanning whitespace is
not split out, by design).

Spec: no IANA registration (de-facto); see Google Search Central,
<https://developers.google.com/search/docs/crawling-indexing/robots-meta-tag>.

See also: "Network.HTTP.Headers.Link", "Network.HTTP.Headers.XPoweredBy".
-}
module Network.HTTP.Headers.XRobotsTag (
  XRobotsTag (..),
  RobotsDirective (..),
  xRobotsTagParser,
  renderXRobotsTag,
) where

import Control.Monad (when)
import Control.Monad.Combinators.NonEmpty (sepBy1)
import qualified Data.ByteString as B
import Data.Char (toLower)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXRobotsTag)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (sepByCommas1, shortText)


-- | A single robots directive: a keyword, optionally with a token value.
data RobotsDirective = RobotsDirective
  { robotsDirectiveName :: !ST.ShortText
  , robotsDirectiveValue :: !(Maybe ST.ShortText)
  }
  deriving stock (Eq, Show)


{- | An @X-Robots-Tag@ value: an optional leading user-agent (bot) name and the
non-empty, comma-separated list of directives that applies to it.
-}
data XRobotsTag = XRobotsTag
  { xRobotsBotName :: !(Maybe ST.ShortText)
  , xRobotsDirectives :: !(NonEmpty RobotsDirective)
  }
  deriving stock (Eq, Show)


-- | Directive keys that carry a value (compared case-insensitively).
isValuedKey :: ST.ShortText -> Bool
isValuedKey n =
  map toLower (ST.toString n)
    `elem` ["max-snippet", "max-image-preview", "max-video-preview", "unavailable_after"]


instance KnownHeader XRobotsTag where
  type ParseFailure XRobotsTag = String
  type Cardinality XRobotsTag = 'ZeroOrOne
  type Direction XRobotsTag = 'Response


  parseFromHeaders _ headers = case runParser xRobotsTagParser (NE.head headers) of
    OK v leftover
      | B.null (dropOws leftover) -> Right v
      | otherwise -> Left ("Unconsumed input after parsing X-Robots-Tag header: " <> show leftover)
    Fail -> Left "Failed to parse X-Robots-Tag header"
    Err e -> Left e
    where
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


  renderToHeaders _ = M.toStrictByteString . renderXRobotsTag


  headerName _ = hXRobotsTag


xRobotsTagParser :: ParserT st String XRobotsTag
xRobotsTagParser = do
  ows
  mBot <- optional botName
  ds <- directive `sepBy1` (ows *> $(char ',') *> ows)
  pure (XRobotsTag mBot ds)
  where
    -- A leading @bot-name ":"@ prefix. A valued-directive key (e.g.
    -- @max-snippet@) is never treated as a bot name, so it backtracks here
    -- and is parsed as a directive instead.
    botName = do
      name <- rfc9110Token
      when (isValuedKey name) failed
      ows
      _ <- $(char ':')
      ows
      pure name
    directive = do
      name <- rfc9110Token
      mval <-
        if isValuedKey name
          then Just <$> (ows *> $(char ':') *> ows *> rfc9110Token)
          else pure Nothing
      pure (RobotsDirective name mval)


renderXRobotsTag :: XRobotsTag -> M.Builder
renderXRobotsTag (XRobotsTag mBot ds) = prefix <> sepByCommas1 (fmap renderDirective ds)
  where
    prefix = case mBot of
      Nothing -> mempty
      Just bot -> shortText bot <> ": "
    renderDirective (RobotsDirective name Nothing) = shortText name
    renderDirective (RobotsDirective name (Just v)) = shortText name <> ": " <> shortText v
