{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- |
@Content-Script-Type@ — HTML 4.01 entity-header that names the default
scripting language (a media type, e.g. @text/javascript@) for scripts in the
served document, when no @META http-equiv=\"Content-Script-Type\"@ declaration
is present.

A media type such as @text/javascript@, with optional parameters. On repeated
field lines the last value takes precedence. The wire spelling is preserved.

Spec: <https://www.w3.org/TR/html401/interact/scripts.html#h-18.2.2>

See also: "Network.HTTP.Headers.ContentStyleType", "Network.HTTP.Headers.DefaultStyle", "Network.HTTP.Headers.ContentType".
-}
module Network.HTTP.Headers.ContentScriptType (
  ContentScriptType (ContentScriptType, contentScriptType),
  contentScriptTypeBytes,
  contentScriptMediaType,
  contentScriptTypeParser,
  renderContentScriptType,
) where

import qualified Data.ByteString.Short as SBS
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.ContentType (ContentType, MediaTypeField, mediaTypeFieldBytes, mediaTypeFieldContentType, mediaTypeFieldFromText, mediaTypeFieldParser, mediaTypeFieldText)
import Network.HTTP.Headers.HeaderFieldName (hContentScriptType)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | The default scripting media type and its wire spelling.
newtype ContentScriptType = ContentScriptTypeField MediaTypeField
  deriving stock (Eq, Show)


pattern ContentScriptType :: ST.ShortText -> ContentScriptType
pattern ContentScriptType {contentScriptType} <- ContentScriptTypeField (mediaTypeFieldText -> contentScriptType)
  where
    ContentScriptType value = ContentScriptTypeField (mediaTypeFieldFromText value)


{-# COMPLETE ContentScriptType #-}


contentScriptTypeBytes :: ContentScriptType -> SBS.ShortByteString
contentScriptTypeBytes (ContentScriptTypeField value) = mediaTypeFieldBytes value


contentScriptMediaType :: ContentScriptType -> Either String ContentType
contentScriptMediaType (ContentScriptTypeField value) = mediaTypeFieldContentType value


instance KnownHeader ContentScriptType where
  type ParseFailure ContentScriptType = String
  type Cardinality ContentScriptType = 'ZeroOrOne
  type Direction ContentScriptType = 'Response


  parseFromHeaders _ = parseFieldLine "Content-Script-Type" contentScriptTypeParser . NE.last


  renderToHeaders _ = SBS.fromShort . contentScriptTypeBytes


  headerName _ = hContentScriptType


contentScriptTypeParser :: ParserT st String ContentScriptType
contentScriptTypeParser = ContentScriptTypeField <$> mediaTypeFieldParser


renderContentScriptType :: ContentScriptType -> M.Builder
renderContentScriptType = M.shortByteString . contentScriptTypeBytes
