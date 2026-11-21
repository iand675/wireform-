{-# LANGUAGE BangPatterns #-}
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
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBSI
import Data.Char (isControl, isHexDigit)
import Data.Either (isRight)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Data.Word (Word8)
import Network.HTTP.ContentNegotiation (renderMediaParameterValue)
import Network.HTTP.Grammar.ExtendedValue (ExtendedValue (..), decodeExtendedValueText, extendedValueBytes, renderExtendedValue)
import Network.HTTP.Grammar.Rendering (copyText, shortText, writeParameterValue)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hContentDisposition)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Wireform.Encode.Direct (dWord8)
import Wireform.Parser.Driver (parseByteString)


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
  :: ST.ShortText
  -- ^ Disposition type, such as @attachment@ or @inline@.
  -> Text
  -- ^ Preferred filename.
  -> Maybe Text
  -- ^ Legacy ASCII fallback.
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
      , Just (lo, _) <- T.uncons rest =
          isHexDigit hi && isHexDigit lo
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


  renderToHeadersChecked _ = encodeContentDispositionChecked


  headerName _ = hContentDisposition


contentDispositionParser :: ParserT st String ContentDisposition
contentDispositionParser = do
  dtype <- ows *> rfc9110Token
  params <- many (ows *> $(char ';') *> ows *> param) <* ows
  if uniqueNames params
    then pure (ContentDisposition dtype params)
    else err "Duplicate Content-Disposition parameter name"
  where
    param = do
      name <- rfc9110Token <* ows <* $(char '=') <* ows
      value <-
        if isExtendedName name
          then if name == "*" then failed else shortASCIIFromParser_ extendedValueBytes
          else rfc9110Token <|> (ST.fromText <$> httpQuotedString)
      pure (name, value)


uniqueNames :: [(ST.ShortText, ST.ShortText)] -> Bool
uniqueNames [(a, _), (b, _)] = not (sameName (ST.toShortByteString a) (ST.toShortByteString b))
uniqueNames params@(_ : _ : _ : _) = go Set.empty params
  where
    go _ [] = True
    go seen ((name, _) : rest) =
      let key = foldedName name
      in not (Set.member key seen) && go (Set.insert key seen) rest
uniqueNames _ = True


-- | The advisory filename. A decodable extended value takes precedence.
contentDispositionFilename :: ContentDisposition -> Maybe Text
contentDispositionFilename (ContentDisposition _ params) = case extended of
  Just value -> Just value
  Nothing -> ST.toText <$> findParam "filename" params
  where
    extended = findParam "filename*" params >>= decodeExtendedValueText . ST.toByteString

    findParam _ [] = Nothing
    findParam expected ((name, value) : rest)
      | sameName expected (ST.toShortByteString name) = Just value
      | otherwise = findParam expected rest


sameName :: SBS.ShortByteString -> SBS.ShortByteString -> Bool
sameName a b = SBS.length b == len && go 0
  where
    len = SBS.length a
    go i
      | i == len = True
      | otherwise =
          lowerAscii (SBSI.unsafeIndex a i) == lowerAscii (SBSI.unsafeIndex b i) && go (i + 1)
{-# INLINE sameName #-}


foldedName :: ST.ShortText -> ByteString
foldedName name =
  let bytes = ST.toByteString name
  in if BS.any isUpperAscii bytes then BS.map lowerAscii bytes else bytes


isUpperAscii :: Word8 -> Bool
isUpperAscii w = 65 <= w && w <= 90
{-# INLINE isUpperAscii #-}


lowerAscii :: Word8 -> Word8
lowerAscii w = if isUpperAscii w then w .|. 32 else w
{-# INLINE lowerAscii #-}


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


encodeContentDispositionChecked :: ContentDisposition -> Maybe ByteString
encodeContentDispositionChecked (ContentDisposition dtype params)
  | isHttpTokenText dtype, uniqueNames params = measure (width dtype) params
  | otherwise = Nothing
  where
    add = BSI.checkedAdd "Content-Disposition"
    width = SBS.length . ST.toShortByteString
    measure !size [] = Just $ BSI.unsafeCreateUptoN size $ \ptr ->
      copyText ptr 0 dtype >>= write ptr params
    measure !size ((name, value) : rest)
      | not (isHttpTokenText name) = Nothing
      | isExtendedName name =
          if name /= "*" && isRight (parseByteString (extendedValueBytes <* eof) (ST.toByteString value))
            then measure (add start (width value)) rest
            else Nothing
      | isHttpQuotedText value = measure (add (add start 2) (add (width value) (width value))) rest
      | otherwise = Nothing
      where
        start = add (add size 2) (width name)
    write _ [] !offset = pure offset
    write ptr ((name, value) : rest) !offset = do
      start <- dWord8 ptr offset 59
      nameEnd <- copyText ptr start name
      valueStart <- dWord8 ptr nameEnd 61
      end <-
        if isExtendedName name
          then copyText ptr valueStart value
          else writeParameterValue ptr valueStart value
      write ptr rest end
