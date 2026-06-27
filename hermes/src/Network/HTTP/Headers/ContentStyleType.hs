{- |
@Content-Style-Type@ — HTML 4.01 entity-header that names the default style
sheet language (a media type, e.g. @text/css@) for the served document, when no
@META http-equiv=\"Content-Style-Type\"@ declaration is present.

== Value

A single media type token such as @text/css@. Per the depth guidance for
obsolete/host-specific values this is a faithful raw-preserving newtype: the
media-type string is kept verbatim and re-emitted unchanged.

Spec: <https://www.w3.org/TR/html401/present/styles.html#h-14.2.1>

See also: "Network.HTTP.Headers.ContentScriptType", "Network.HTTP.Headers.DefaultStyle", "Network.HTTP.Headers.ContentType".
-}
module Network.HTTP.Headers.ContentStyleType (
  ContentStyleType (..),
  contentStyleTypeParser,
  renderContentStyleType,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hContentStyleType)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | @Content-Style-Type@ value: the default style sheet media type, verbatim.
newtype ContentStyleType = ContentStyleType {contentStyleType :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader ContentStyleType where
  type ParseFailure ContentStyleType = String
  type Cardinality ContentStyleType = 'ZeroOrOne
  type Direction ContentStyleType = 'Response


  parseFromHeaders _ headers = case runParser contentStyleTypeParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Content-Style-Type header: " <> show rest
    Fail -> Left "Failed to parse Content-Style-Type header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderContentStyleType


  headerName _ = hContentStyleType


contentStyleTypeParser :: ParserT st String ContentStyleType
contentStyleTypeParser = ContentStyleType <$> takeRestShortText


renderContentStyleType :: ContentStyleType -> M.Builder
renderContentStyleType (ContentStyleType mediaType) = shortText mediaType
