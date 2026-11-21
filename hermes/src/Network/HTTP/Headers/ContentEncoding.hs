{- |
@Content-Encoding@ lists the content codings (e.g. @gzip@, @br@, @zstd@) that
have been applied to the representation on top of its media type, in the order
they were applied; the recipient reverses them to recover the original data. It
is representation metadata that may appear on both requests and responses, and
is the counterpart to the @Accept-Encoding@ negotiation header.

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-8.4>

See also: "Network.HTTP.Headers.ContentType", "Network.HTTP.Headers.ContentLanguage", "Network.HTTP.Headers.AcceptEncoding", "Network.HTTP.Headers.TransferEncoding".
-}
module Network.HTTP.Headers.ContentEncoding (
  ContentEncoding (..),
  contentCodings,
  contentEncodingParser,
  renderContentEncoding,
) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import qualified Data.List.NonEmpty as NE
import Network.HTTP.ContentCoding
import Network.HTTP.ContentNegotiation (negotiationList, parseNegotiationHeaders)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hContentEncoding)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (encodeCommaSeparatedBytes, sepByCommas1)


{- | A present @Content-Encoding@ field, possibly with no applied codings.
Use 'contentCodings' to read the field in all cases. The 'contentEncoding'
record selector applies to the populated constructors.
-}
data ContentEncoding
  = ContentEncoding {contentEncoding :: !ContentCoding}
  | ContentEncodingChain
      { contentEncoding :: !ContentCoding
      , additionalContentCodings :: !(NonEmpty ContentCoding)
      }
  | ContentEncodingEmpty
  deriving stock (Eq, Show)


instance KnownHeader ContentEncoding where
  type ParseFailure ContentEncoding = String
  type Cardinality ContentEncoding = 'ZeroOrOne
  type Direction ContentEncoding = 'RequestAndResponse


  parseFromHeaders _ headers =
    fromCodings <$> parseNegotiationHeaders "Content-Encoding" codingList headers


  renderToHeaders _ = M.toStrictByteString . renderContentEncoding


  renderToHeadersChecked _ ContentEncodingEmpty = Just ""
  renderToHeadersChecked _ (ContentEncoding coding)
    | isValidContentCoding coding = Just (ST.toByteString (contentCodingText coding))
    | otherwise = Nothing
  renderToHeadersChecked _ (ContentEncodingChain first rest)
    | isValidContentCoding first && all isValidContentCoding rest =
        Just (encodeCommaSeparatedBytes (ST.toShortByteString . contentCodingText) (first :| NE.toList rest))
    | otherwise = Nothing


  headerName _ = hContentEncoding


contentEncodingParser :: ParserT st String ContentEncoding
contentEncodingParser = fromCodings <$> codingList


codingList :: ParserT st e [ContentCoding]
codingList = negotiationList contentCodingParser


fromCodings :: [ContentCoding] -> ContentEncoding
fromCodings [] = ContentEncodingEmpty
fromCodings [coding] = ContentEncoding coding
fromCodings (first : second : rest) = ContentEncodingChain first (second :| rest)


-- | Return the codings in the order in which they were applied.
contentCodings :: ContentEncoding -> [ContentCoding]
contentCodings ContentEncodingEmpty = []
contentCodings (ContentEncoding coding) = [coding]
contentCodings (ContentEncodingChain first rest) = first : NE.toList rest


renderContentEncoding :: ContentEncoding -> M.Builder
renderContentEncoding ContentEncodingEmpty = mempty
renderContentEncoding (ContentEncoding coding) = renderContentCoding coding
renderContentEncoding (ContentEncodingChain first rest) =
  renderContentCoding first <> ", " <> sepByCommas1 (renderContentCoding <$> rest)
