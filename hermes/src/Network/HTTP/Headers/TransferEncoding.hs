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
  transferEncodingParser,
  renderTransferEncoding,
) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Short as SBS
import Data.Char (isAsciiUpper, toLower)
import qualified Data.CharSet as CharSet
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hTransferEncoding)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (renderHttpQuotedString, sepByCommas1, shortText)


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
validateCodings = go False
  where
    go _ [] = Right ()
    go seenChunked (TransferCoding name params : rest)
      | seenChunked && chunked = Left "Transfer-Encoding repeats chunked"
      | not (null params) && not (transferCodingSupportsParameters name) = Left "Transfer coding does not allow parameters"
      | otherwise = go (seenChunked || chunked) rest
      where
        chunked = transferCodingIs "chunked" name


-- | A transfer-coding or parameter token's case-insensitive comparison key.
transferCodingKey :: ST.ShortText -> ST.ShortText
transferCodingKey name
  | ST.any isAsciiUpper name = STU.fromByteStringUnsafe (BC.map toLower (ST.toByteString name))
  | otherwise = name
{-# INLINE transferCodingKey #-}


-- | Compare a coding name to a known lowercase token.
transferCodingIs :: ST.ShortText -> ST.ShortText -> Bool
transferCodingIs expected actual =
  actual == expected
    || ( SBS.length (ST.toShortByteString expected) == SBS.length (ST.toShortByteString actual)
           && transferCodingKey actual == expected
       )
{-# INLINE transferCodingIs #-}


transferCodingSupportsParameters :: ST.ShortText -> Bool
transferCodingSupportsParameters = supportsParameters . transferCodingKey


supportsParameters :: ST.ShortText -> Bool
supportsParameters name =
  name /= "chunked"
    && name /= "compress"
    && name /= "deflate"
    && name /= "gzip"
    && name /= "x-compress"
    && name /= "x-gzip"


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
