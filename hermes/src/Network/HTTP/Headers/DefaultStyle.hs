{- |
@Default-Style@ — HTML 4.01 entity-header that names the preferred style sheet
set for the served document (matching the @title@ of a @LINK@/@STYLE@ style
sheet), the HTTP-header form of @META http-equiv=\"Default-Style\"@.

== Value

A single opaque style-sheet title, e.g. @compact@. Per the depth guidance for
obsolete/free-text values this is a faithful raw-preserving newtype: the title
is captured verbatim and re-emitted unchanged.

Spec: <https://www.w3.org/TR/html401/present/styles.html#h-14.3.2>

See also: "Network.HTTP.Headers.ContentStyleType", "Network.HTTP.Headers.ContentScriptType".
-}
module Network.HTTP.Headers.DefaultStyle (
  DefaultStyle (..),
  defaultStyleParser,
  renderDefaultStyle,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hDefaultStyle)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | @Default-Style@ value: the preferred style-sheet title, verbatim.
newtype DefaultStyle = DefaultStyle {defaultStyleTitle :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader DefaultStyle where
  type ParseFailure DefaultStyle = String
  type Cardinality DefaultStyle = 'ZeroOrOne
  type Direction DefaultStyle = 'Response


  parseFromHeaders _ headers = case runParser defaultStyleParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Default-Style header: " <> show rest
    Fail -> Left "Failed to parse Default-Style header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderDefaultStyle


  headerName _ = hDefaultStyle


defaultStyleParser :: ParserT st String DefaultStyle
defaultStyleParser = DefaultStyle <$> takeRestShortText


renderDefaultStyle :: DefaultStyle -> M.Builder
renderDefaultStyle (DefaultStyle title) = shortText title
