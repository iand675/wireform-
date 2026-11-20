{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- |
@Content-Style-Type@ — HTML 4.01 entity-header that names the default style
sheet language (a media type, e.g. @text/css@) for the served document, when no
@META http-equiv=\"Content-Style-Type\"@ declaration is present.

A media type such as @text/css@, with optional parameters. On repeated
field lines the last value takes precedence. The wire spelling is preserved.

Spec: <https://www.w3.org/TR/html401/present/styles.html#h-14.2.1>

See also: "Network.HTTP.Headers.ContentScriptType", "Network.HTTP.Headers.DefaultStyle", "Network.HTTP.Headers.ContentType".
-}
module Network.HTTP.Headers.ContentStyleType (
  ContentStyleType (ContentStyleType, contentStyleType),
  contentStyleTypeBytes,
  contentStyleMediaType,
  contentStyleTypeParser,
  renderContentStyleType,
) where

import qualified Data.ByteString.Short as SBS
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.ContentType (ContentType, MediaTypeField, mediaTypeFieldBytes, mediaTypeFieldContentType, mediaTypeFieldFromText, mediaTypeFieldParser, mediaTypeFieldText)
import Network.HTTP.Headers.HeaderFieldName (hContentStyleType)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | The default style sheet media type and its wire spelling.
newtype ContentStyleType = ContentStyleTypeField MediaTypeField
  deriving stock (Eq, Show)


pattern ContentStyleType :: ST.ShortText -> ContentStyleType
pattern ContentStyleType {contentStyleType} <- ContentStyleTypeField (mediaTypeFieldText -> contentStyleType)
  where
    ContentStyleType value = ContentStyleTypeField (mediaTypeFieldFromText value)


{-# COMPLETE ContentStyleType #-}


contentStyleTypeBytes :: ContentStyleType -> SBS.ShortByteString
contentStyleTypeBytes (ContentStyleTypeField value) = mediaTypeFieldBytes value


contentStyleMediaType :: ContentStyleType -> Either String ContentType
contentStyleMediaType (ContentStyleTypeField value) = mediaTypeFieldContentType value


instance KnownHeader ContentStyleType where
  type ParseFailure ContentStyleType = String
  type Cardinality ContentStyleType = 'ZeroOrOne
  type Direction ContentStyleType = 'Response


  parseFromHeaders _ = parseFieldLine "Content-Style-Type" contentStyleTypeParser . NE.last


  renderToHeaders _ = SBS.fromShort . contentStyleTypeBytes


  headerName _ = hContentStyleType


contentStyleTypeParser :: ParserT st String ContentStyleType
contentStyleTypeParser = ContentStyleTypeField <$> mediaTypeFieldParser


renderContentStyleType :: ContentStyleType -> M.Builder
renderContentStyleType = M.shortByteString . contentStyleTypeBytes
