{- |
@Content-Script-Type@ — HTML 4.01 entity-header that names the default
scripting language (a media type, e.g. @text/javascript@) for scripts in the
served document, when no @META http-equiv=\"Content-Script-Type\"@ declaration
is present.

== Value

A single media type token such as @text/javascript@, @text/ecmascript@,
@text/tcl@ or @text/vbscript@. Per the depth guidance for obsolete/host-specific
values this is a faithful raw-preserving newtype: the media-type string is kept
verbatim and re-emitted unchanged (a full media-type grammar is intentionally
not duplicated here).

Spec: <https://www.w3.org/TR/html401/interact/scripts.html#h-18.2.2>

See also: "Network.HTTP.Headers.ContentStyleType", "Network.HTTP.Headers.DefaultStyle", "Network.HTTP.Headers.ContentType".
-}
module Network.HTTP.Headers.ContentScriptType (
  ContentScriptType (..),
  contentScriptTypeParser,
  renderContentScriptType,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hContentScriptType)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | @Content-Script-Type@ value: the default scripting media type, verbatim.
newtype ContentScriptType = ContentScriptType {contentScriptType :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader ContentScriptType where
  type ParseFailure ContentScriptType = String
  type Cardinality ContentScriptType = 'ZeroOrOne
  type Direction ContentScriptType = 'Response


  parseFromHeaders _ headers = case runParser contentScriptTypeParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Content-Script-Type header: " <> show rest
    Fail -> Left "Failed to parse Content-Script-Type header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderContentScriptType


  headerName _ = hContentScriptType


contentScriptTypeParser :: ParserT st String ContentScriptType
contentScriptTypeParser = ContentScriptType <$> takeRestShortText


renderContentScriptType :: ContentScriptType -> M.Builder
renderContentScriptType (ContentScriptType mediaType) = shortText mediaType
