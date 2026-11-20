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
  contentDispositionFilename,
  contentDispositionParser,
  renderContentDisposition,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Short as SBS
import Data.Char (toLower)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.Short as ST
import Network.HTTP.ContentNegotiation (renderMediaParameterValue)
import Network.HTTP.Grammar.ExtendedValue (decodeExtendedValue, extendedValueBytes, parseExtendedValue)
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
      | foldedName name == expected = Just value
      | otherwise = findParam expected rest


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
