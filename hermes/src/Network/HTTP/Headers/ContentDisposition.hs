{-# LANGUAGE TemplateHaskell #-}

{- |
@Content-Disposition@ tells a user agent how to present the enclosed
representation: rendered @inline@ in the page or offered as an @attachment@ to
download, optionally suggesting a @filename@. In HTTP it is chiefly a response
header; the same field also labels the individual parts of a
@multipart/form-data@ request body.

Spec: <https://www.rfc-editor.org/rfc/rfc6266#section-4>

See also: "Network.HTTP.Headers.ContentType", "Network.HTTP.Headers.ContentEncoding", "Network.HTTP.Headers.ContentLanguage", "Network.HTTP.Headers.ContentLocation".
-}
module Network.HTTP.Headers.ContentDisposition (
  ContentDisposition (..),
  ContentDispositionFilenameError (..),
  mkContentDispositionFilename,
  contentDispositionFilename,
  contentDispositionParser,
  renderContentDisposition,
) where

import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBSI
import Data.Char (isControl, isHexDigit, toLower)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Network.HTTP.ContentNegotiation (renderMediaParameterValue)
import Network.HTTP.Grammar.ExtendedValue (ExtendedValue (..), decodeExtendedValue, extendedValueBytes, parseExtendedValue, renderExtendedValue)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hContentDisposition)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | Content-Disposition value consists of a disposition type and optional parameters
data ContentDisposition = ContentDisposition
  { dispositionType :: !ST.ShortText
  , dispositionParams :: ![(ST.ShortText, ST.ShortText)]
  }
  deriving stock (Eq, Show)


-- | An invalid disposition or filename supplied for authoring.
data ContentDispositionFilenameError
  = InvalidDispositionType
  | InvalidFilenameCharacters
  | InvalidFilenameFallback
  deriving stock (Eq, Show)


{- | Author a disposition with an advisory filename. An ordinary printable
ASCII filename uses @filename@. A name that needs UTF-8, contains a backslash,
or resembles a percent escape uses @filename*@. An explicitly supplied
fallback goes first as @filename@ and must be printable ASCII without
backslashes or percent escapes. The primary name must be free of control
characters.
-}
mkContentDispositionFilename
  :: ST.ShortText -- ^ Disposition type, such as @attachment@ or @inline@.
  -> Text -- ^ Preferred filename.
  -> Maybe Text -- ^ Legacy ASCII fallback.
  -> Either ContentDispositionFilenameError ContentDisposition
mkContentDispositionFilename disposition filename fallback
  | ST.null disposition || not (ST.all isTokenChar disposition) = Left InvalidDispositionType
  | style == InvalidFilename = Left InvalidFilenameCharacters
  | otherwise = case fallback of
      Just ascii
        | filenameStyle ascii == PlainFilename -> Right (ContentDisposition disposition [plain ascii, extended])
        | otherwise -> Left InvalidFilenameFallback
      Nothing
        | style == PlainFilename -> Right (ContentDisposition disposition [plain filename])
        | otherwise -> Right (ContentDisposition disposition [extended])
  where
    style = filenameStyle filename
    plain name = ("filename", ST.fromText name)
    extended =
      ("filename*", STU.fromByteStringUnsafe (M.toStrictByteString (renderExtendedValue value)))
    value = ExtendedValue "UTF-8" Nothing (TE.encodeUtf8 filename)


data FilenameStyle = PlainFilename | ExtendedFilename | InvalidFilename
  deriving stock (Eq)


filenameStyle :: Text -> FilenameStyle
filenameStyle name = case T.uncons (T.dropWhile plainChar name) of
  Nothing -> PlainFilename
  Just ('%', rest)
    | startsWithHexPair rest -> extendedStyle rest
    | otherwise -> filenameStyle rest
  Just (c, rest)
    | isControl c -> InvalidFilename
    | otherwise -> extendedStyle rest
  where
    plainChar c = ' ' <= c && c <= '~' && c /= '%' && c /= '\\'
    startsWithHexPair value
      | Just (hi, rest) <- T.uncons value
      , Just (lo, _) <- T.uncons rest = isHexDigit hi && isHexDigit lo
      | otherwise = False
    extendedStyle rest
      | T.any isControl rest = InvalidFilename
      | otherwise = ExtendedFilename


instance KnownHeader ContentDisposition where
  type ParseFailure ContentDisposition = String
  type Cardinality ContentDisposition = 'ZeroOrOne
  type Direction ContentDisposition = 'Response


  parseFromHeaders _ = parseSingleFieldLine "Content-Disposition" contentDispositionParser


  renderToHeaders _ = M.toStrictByteString . renderContentDisposition


  headerName _ = hContentDisposition


contentDispositionParser :: ParserT st String ContentDisposition
contentDispositionParser = do
  dtype <- ows *> rfc9110Token
  params <- many (ows *> $(char ';') *> ows *> param) <* ows
  case params of
    _ : _ : _ | not (uniqueNames Set.empty params) -> err "Duplicate Content-Disposition parameter name"
    _ -> pure (ContentDisposition dtype params)
  where
    param = do
      name <- rfc9110Token <* ows <* $(char '=') <* ows
      value <-
        if isExtendedName name
          then if name == "*" then failed else shortASCIIFromParser_ extendedValueBytes
          else rfc9110Token <|> (ST.fromText <$> httpQuotedString)
      pure (name, value)
    uniqueNames _ [] = True
    uniqueNames seen ((name, _) : rest) =
      let key = foldedName name
      in not (Set.member key seen) && uniqueNames (Set.insert key seen) rest


-- | The advisory filename. A decodable extended value takes precedence.
contentDispositionFilename :: ContentDisposition -> Maybe Text
contentDispositionFilename (ContentDisposition _ params) = case extended of
  Just value -> Just value
  Nothing -> ST.toText <$> findParam "filename" params
  where
    extended = do
      spelling <- findParam "filename*" params
      value <- either (const Nothing) Just (parseExtendedValue (ST.toByteString spelling))
      decodeExtendedValue value

    findParam _ [] = Nothing
    findParam expected ((name, value) : rest)
      | matchesName expected (ST.toShortByteString name) = Just value
      | otherwise = findParam expected rest


-- The expected filename parameter name is ASCII lowercase.
matchesName :: SBS.ShortByteString -> SBS.ShortByteString -> Bool
matchesName expected name = SBS.length name == len && go 0
  where
    len = SBS.length expected
    go i
      | i == len = True
      | otherwise =
          let byte = SBSI.unsafeIndex name i
              lower = if 65 <= byte && byte <= 90 then byte .|. 32 else byte
          in lower == SBSI.unsafeIndex expected i && go (i + 1)
{-# INLINE matchesName #-}


foldedName :: ST.ShortText -> ByteString
foldedName name =
  let bytes = ST.toByteString name
  in if BS.any (\w -> 65 <= w && w <= 90) bytes then BC.map toLower bytes else bytes


isExtendedName :: ST.ShortText -> Bool
isExtendedName name = not (SBS.null bytes) && SBS.last bytes == 42
  where
    bytes = ST.toShortByteString name


renderContentDisposition :: ContentDisposition -> M.Builder
renderContentDisposition (ContentDisposition dtype params) =
  shortText dtype <> foldMap renderParam params
  where
    renderParam (k, v) =
      M.char7 ';' <> shortText k <> M.char7 '=' <> (if isExtendedName k then shortText v else renderMediaParameterValue v)
