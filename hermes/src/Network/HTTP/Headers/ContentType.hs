{- |
@Content-Type@ states the media type of the associated representation — for
example @text/html; charset=utf-8@ or @application/json@ — so the recipient
knows how to interpret the body, including parameters such as @charset@ or the
multipart @boundary@. It is representation metadata used on both requests and
responses.

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-8.3>

See also: "Network.HTTP.Headers.ContentEncoding", "Network.HTTP.Headers.ContentLanguage", "Network.HTTP.Headers.ContentDisposition", "Network.HTTP.Headers.Accept".
-}
module Network.HTTP.Headers.ContentType (
  ContentType (..),
  contentTypeParameters,
  contentTypeParser,
  renderContentType,
  MediaTypeField,
  mediaTypeFieldParser,
  mediaTypeFieldFromText,
  mediaTypeFieldText,
  mediaTypeFieldBytes,
  encodeMediaTypeFieldChecked,
  mediaTypeFieldContentType,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.Either (isRight)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Network.HTTP.ContentNegotiation (MediaRange (..), MediaType, encodeMediaTypesChecked, mediaTypeSpelling, mediaTypeWithParametersParser, renderMediaRange, renderMediaType)
import Network.HTTP.Grammar.Rendering (opaqueShortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hContentType)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Wireform.Parser.Driver (parseByteString)


data ContentType
  = ContentType {contentType :: !MediaType}
  | ParameterizedContentType
      { contentType :: !MediaType
      , mediaParameters :: !(NonEmpty (ST.ShortText, ST.ShortText))
      }
  deriving stock (Eq, Show)


instance KnownHeader ContentType where
  type ParseFailure ContentType = String
  type Cardinality ContentType = 'ZeroOrOne
  type Direction ContentType = 'RequestAndResponse


  parseFromHeaders _ (header :| []) = parseFieldLine "Content-Type" contentTypeParser header
  parseFromHeaders _ _ = Left "Multiple Content-Type field lines"


  renderToHeaders _ = M.toStrictByteString . renderContentType


  renderToHeadersChecked _ value =
    encodeMediaTypesChecked [MediaRange (contentType value) (contentTypeParameters value)]


  headerName _ = hContentType


contentTypeParser :: ParserT st String ContentType
contentTypeParser = ows *> (fromMediaRange <$> mediaTypeWithParametersParser) <* ows


fromMediaRange :: MediaRange -> ContentType
fromMediaRange (MediaRange mt []) = ContentType mt
fromMediaRange (MediaRange mt (p : ps)) = ParameterizedContentType mt (p :| ps)


-- | Media type parameters, in field order. Their values are unescaped.
contentTypeParameters :: ContentType -> [(ST.ShortText, ST.ShortText)]
contentTypeParameters (ContentType _) = []
contentTypeParameters (ParameterizedContentType _ (p :| ps)) = p : ps


renderContentType :: ContentType -> M.Builder
renderContentType (ContentType mt) = renderMediaType mt
renderContentType (ParameterizedContentType mt (p :| ps)) = renderMediaRange (MediaRange mt (p : ps))


-- | A media type field that retains its original spelling for forwarding.
data MediaTypeField
  = ParsedMediaTypeField !SBS.ShortByteString !ContentType
  | MediaTypeFieldText !ST.ShortText
  deriving stock (Show)


instance Eq MediaTypeField where
  left == right = mediaTypeFieldBytes left == mediaTypeFieldBytes right


mediaTypeFieldParser :: ParserT st String MediaTypeField
mediaTypeFieldParser = ows *> withByteString mediaTypeWithParametersParser save <* ows
  where
    save parsed wire = pure (ParsedMediaTypeField (SBS.toShort (BS.dropWhileEnd isOwsByte wire)) (fromMediaRange parsed))
    isOwsByte byte = byte == 9 || byte == 32


{- | Construct using the text interface. Validation is available through
'mediaTypeFieldContentType'.
-}
mediaTypeFieldFromText :: ST.ShortText -> MediaTypeField
mediaTypeFieldFromText = MediaTypeFieldText


mediaTypeFieldText :: MediaTypeField -> ST.ShortText
mediaTypeFieldText (MediaTypeFieldText value) = value
mediaTypeFieldText (ParsedMediaTypeField bytes _) = opaqueShortText bytes


mediaTypeFieldBytes :: MediaTypeField -> SBS.ShortByteString
mediaTypeFieldBytes (MediaTypeFieldText value) = ST.toShortByteString value
mediaTypeFieldBytes (ParsedMediaTypeField bytes _) = bytes


-- | Keep parsed wire spelling and validate a newly authored field value.
encodeMediaTypeFieldChecked :: MediaTypeField -> Maybe BS.ByteString
encodeMediaTypeFieldChecked (ParsedMediaTypeField bytes _) = Just (SBS.fromShort bytes)
encodeMediaTypeFieldChecked (MediaTypeFieldText value)
  | isRight (parseByteString (ows *> mediaTypeSpelling <* ows <* eof) wire) = Just wire
  | otherwise = Nothing
  where
    wire = ST.toByteString value


mediaTypeFieldContentType :: MediaTypeField -> Either String ContentType
mediaTypeFieldContentType (MediaTypeFieldText value) = parseFieldLine "media type" contentTypeParser (ST.toByteString value)
mediaTypeFieldContentType (ParsedMediaTypeField _ value) = Right value
