{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
RFC 9112 §6.1 @Transfer-Encoding@ — names the transfer codings (e.g.
@chunked@, @gzip@, @deflate@) applied to the message body to form the
payload, in the order they were applied; the recipient reverses them to
recover the content. A final @chunked@ coding delimits the body. Requests
with this field require a final @chunked@ coding. Responses may instead
use connection-close framing. It is a request and response
header, primarily an HTTP\/1.1 mechanism, and is forbidden in HTTP\/2 and
HTTP\/3.

== Grammar

@
Transfer-Encoding = #transfer-coding
transfer-coding   = token *( OWS ";" OWS transfer-parameter )
@

Spec: <https://www.rfc-editor.org/rfc/rfc9112#section-6.1>

See also: "Network.HTTP.Headers.TE", "Network.HTTP.Headers.Trailer",
"Network.HTTP.Headers.ContentLength", "Network.HTTP.Headers.ContentEncoding".
-}
module Network.HTTP.Headers.TransferEncoding (
  TransferEncoding (..),
  TransferCoding (..),
  TransferParameter (..),
  TransferParameterValue (..),
  transferCodings,
  validateRequestTransferEncoding,
  transferEncodingEndsInChunked,
  transferCodingNameParser,
  transferCodingIs,
  transferParameterParser,
  renderTransferParameter,
  transferCodingSupportsParameters,
  encodeTransferCodingsChecked,
  transferEncodingParser,
  renderTransferEncoding,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short.Internal as SBS
import Data.Char (ord)
import qualified Data.CharSet as CharSet
import Data.Either (isRight)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Internal as T
import qualified Data.Text.Short as ST
import Foreign.Ptr (plusPtr)
import Network.HTTP.ContentNegotiation (sameNegotiationToken, writeExplicitWeight)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hTransferEncoding)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (encodeCommaSeparatedChecked, encodeCommaSeparatedText, foldHttpQuotedText, renderHttpQuotedString, sepByCommas1, shortText)
import Wireform.Encode.Direct (dWord8)


-- | An ordered list of transfer codings, retaining any extension parameters.
data TransferEncoding
  = TransferEncoding {encodings :: NonEmpty ST.ShortText}
  | ParameterizedTransferEncoding !(NonEmpty TransferCoding)
  | TransferEncodingEmpty
  deriving stock (Eq, Show)


data TransferCoding = TransferCoding
  { transferCodingName :: !ST.ShortText
  , transferCodingParameters :: ![TransferParameter]
  }
  deriving stock (Eq, Show)


data TransferParameter = TransferParameter
  { transferParameterName :: !ST.ShortText
  , transferParameterValue :: !TransferParameterValue
  }
  deriving stock (Eq, Show)


data TransferParameterValue
  = TransferToken !ST.ShortText
  | TransferQuoted !Text
  deriving stock (Eq, Show)


instance KnownHeader TransferEncoding where
  type ParseFailure TransferEncoding = String
  type Cardinality TransferEncoding = 'ZeroOrOne
  type Direction TransferEncoding = 'RequestAndResponse


  parseFromHeaders _ fields =
    parseListFieldLines "Transfer-Encoding" (httpList transferCodingParser) fields >>= fromCodings


  renderToHeaders _ = M.toStrictByteString . renderTransferEncoding


  renderToHeadersChecked _ TransferEncodingEmpty = Just ""
  renderToHeadersChecked _ (TransferEncoding ("chunked" :| [])) = Just "chunked"
  renderToHeadersChecked _ (TransferEncoding names)
    | isRight (validateCodingsWith id (const False) isHttpTokenText (NE.toList names)) = Just (encodeCommaSeparatedText names)
    | otherwise = Nothing
  renderToHeadersChecked _ (ParameterizedTransferEncoding codings)
    | isRight (validateCodings (NE.toList codings)) =
        encodeTransferCodingsChecked transferCodingName transferCodingParameters (const Nothing) (NE.toList codings)
    | otherwise = Nothing


  headerName _ = hTransferEncoding


transferEncodingParser :: ParserT st String TransferEncoding
transferEncodingParser = do
  codings <- httpList transferCodingParser
  either err pure (fromCodings codings)


transferCodingParser :: ParserT st String TransferCoding
transferCodingParser = TransferCoding <$> transferCodingNameParser <*> many (ows *> $(char ';') *> ows *> transferParameterParser)


-- | Match common complete coding names before copying an extension token.
transferCodingNameParser :: ParserT st e ST.ShortText
transferCodingNameParser = known <|> rfc9110Token
  where
    known =
      $( switch
           [|
             case _ of
               "chunked" -> whole "chunked"
               "gzip" -> whole "gzip"
               "deflate" -> whole "deflate"
               "compress" -> whole "compress"
               "trailers" -> whole "trailers"
             |]
       )
    whole name = name <$ notFollowedBy (skipSatisfyAscii (`CharSet.member` tokenCharSet))
{-# INLINE transferCodingNameParser #-}


transferParameterParser :: ParserT st e TransferParameter
transferParameterParser =
  TransferParameter <$> rfc9110Token <*> (bws *> $(char '=') *> bws *> value)
  where
    value = (TransferToken <$> rfc9110Token) <|> (TransferQuoted <$> httpQuotedString)
{-# INLINE transferParameterParser #-}


fromCodings :: [TransferCoding] -> Either String TransferEncoding
fromCodings [] = Right TransferEncodingEmpty
fromCodings (first : rest) = do
  validateCodings (first : rest)
  let codings = first :| rest
  pure $
    if all (null . transferCodingParameters) codings
      then TransferEncoding (fmap transferCodingName codings)
      else ParameterizedTransferEncoding codings


validateCodings :: [TransferCoding] -> Either String ()
validateCodings = validateCodingsWith transferCodingName (not . null . transferCodingParameters) (const True)


validateCodingsWith :: (a -> ST.ShortText) -> (a -> Bool) -> (ST.ShortText -> Bool) -> [a] -> Either String ()
validateCodingsWith codingName hasParameters validName = go False
  where
    go _ [] = Right ()
    go seenChunked (entry : rest)
      | not (validName name) = Left "Invalid transfer-coding name"
      | seenChunked && chunked = Left "Transfer-Encoding repeats chunked"
      | hasParameters entry && not (transferCodingSupportsParameters name) = Left "Transfer coding does not allow parameters"
      | otherwise = go (seenChunked || chunked) rest
      where
        name = codingName entry
        chunked = transferCodingIs "chunked" name
{-# INLINE validateCodingsWith #-}


-- | Compare a coding name to a known lowercase token.
transferCodingIs :: ST.ShortText -> ST.ShortText -> Bool
transferCodingIs = sameNegotiationToken
{-# INLINE transferCodingIs #-}


transferCodingSupportsParameters :: ST.ShortText -> Bool
transferCodingSupportsParameters name =
  not (transferCodingIs "chunked" name
    || transferCodingIs "compress" name
    || transferCodingIs "deflate" name
    || transferCodingIs "gzip" name
    || transferCodingIs "x-compress" name
    || transferCodingIs "x-gzip" name)


-- | All codings, including parameters, in field order.
transferCodings :: TransferEncoding -> [TransferCoding]
transferCodings = \case
  TransferEncoding names -> map (\name -> TransferCoding name []) (NE.toList names)
  ParameterizedTransferEncoding codings -> NE.toList codings
  TransferEncodingEmpty -> []


transferEncodingEndsInChunked :: TransferEncoding -> Bool
transferEncodingEndsInChunked = \case
  TransferEncoding names -> isChunked (NE.last names)
  ParameterizedTransferEncoding codings -> isChunked (transferCodingName (NE.last codings))
  TransferEncodingEmpty -> False
  where
    isChunked = transferCodingIs "chunked"


-- | Apply the HTTP/1.1 request-side framing rule after decoding this field.
validateRequestTransferEncoding :: TransferEncoding -> Either String ()
validateRequestTransferEncoding value
  | transferEncodingEndsInChunked value = validateCodings (transferCodings value)
  | otherwise = Left "Request Transfer-Encoding requires a final chunked coding"


-- | Check lexical spelling and write a coding list with its explicit weights.
-- Callers enforce the field-specific rules for coding names and parameters.
encodeTransferCodingsChecked
  :: (a -> ST.ShortText)
  -> (a -> [TransferParameter])
  -> (a -> Maybe Double)
  -> [a]
  -> Maybe ByteString
encodeTransferCodingsChecked name parameters weight = encodeCommaSeparatedChecked entryBound writeEntry
  where
    add = BSI.checkedAdd "HTTP transfer coding"
    width = SBS.length . ST.toShortByteString
    entryBound entry
      | isHttpTokenText token =
          parameterBound (add (width token) (maybe 0 (const 8) (weight entry))) (parameters entry)
      | otherwise = -1
      where
        token = name entry
    parameterBound size [] = size
    parameterBound size (TransferParameter key value : rest)
      | not (isHttpTokenText key) = -1
      | otherwise = case value of
          TransferToken token
            | isHttpTokenText token -> parameterBound (add start (width token)) rest
          TransferQuoted text@(T.Text _ _ len)
            | T.all (isOpaqueByte . fromIntegral . ord) text ->
                parameterBound (add (add start 2) (add len len)) rest
          _ -> -1
      where
        start = add (add size 2) (width key)

    writeEntry ptr offset entry = do
      named <- writeText ptr offset (name entry)
      end <- writeParameters ptr named (parameters entry)
      case weight entry of
        Nothing -> pure end
        Just quality -> writeExplicitWeight ptr end quality
    writeParameters _ !offset [] = pure offset
    writeParameters ptr !offset (TransferParameter key value : rest) = do
      start <- dWord8 ptr offset 59
      named <- writeText ptr start key
      payload <- dWord8 ptr named 61
      end <- case value of
        TransferToken token -> writeText ptr payload token
        TransferQuoted text -> foldHttpQuotedText (dWord8 ptr) payload text
      writeParameters ptr end rest
    writeText ptr offset text = do
      let bytes = ST.toShortByteString text
          len = SBS.length bytes
      SBS.copyToPtr bytes 0 (ptr `plusPtr` offset) len
      pure $! offset + len
{-# INLINE encodeTransferCodingsChecked #-}


renderTransferEncoding :: TransferEncoding -> M.Builder
renderTransferEncoding = \case
  TransferEncoding codings -> sepByCommas1 (fmap shortText codings)
  ParameterizedTransferEncoding codings -> sepByCommas1 (fmap renderCoding codings)
  TransferEncodingEmpty -> mempty
  where
    renderCoding (TransferCoding name params) = shortText name <> foldMap renderTransferParameter params


renderTransferParameter :: TransferParameter -> M.Builder
renderTransferParameter (TransferParameter name value) =
  M.char7 ';' <> shortText name <> M.char7 '=' <> case value of
    TransferToken token -> shortText token
    TransferQuoted quoted -> renderHttpQuotedString quoted
