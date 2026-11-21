{-# LANGUAGE TemplateHaskell #-}

{- |
@Content-ID@ has three registered or widely used forms. MIME uses a message
identifier in angle brackets. OData multipart batches use an unreserved
request identifier. The historical Distribution and Replication Protocol
(DRP) uses one or more comma-separated absolute URIs as a content identifier.

Parsing validates the field syntax and retains the identifier's spelling.

See <https://www.rfc-editor.org/rfc/rfc2045#section-7 RFC 2045 §7>,
<https://www.rfc-editor.org/rfc/rfc5322#section-3.6.4 RFC 5322 §3.6.4>,
<https://docs.oasis-open.org/odata/odata/v4.01/os/abnf/odata-abnf-construction-rules.txt OData ABNF §8>,
and <https://www.w3.org/TR/NOTE-drp-19970825 the W3C DRP Note §2.1>.

See also: "Network.HTTP.Headers.DifferentialID", "Network.HTTP.Headers.ContentMD5", "Network.HTTP.Headers.ContentLocation".
-}
module Network.HTTP.Headers.ContentID (
  ContentID (..),
  contentIDParser,
  drpContentIdentifier,
  outboundDRPContentIdentifierBytes,
  renderContentID,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.CharSet as CS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Network.HTTP.Grammar.Authority (uriSchemeBytes)
import Network.HTTP.Grammar.URIReference (absoluteURIBytes, outboundAbsoluteURIBytes)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hContentID)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | @Content-ID@ value: the opaque content identifier, preserved verbatim.
newtype ContentID = ContentID {contentID :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader ContentID where
  type ParseFailure ContentID = String
  type Cardinality ContentID = 'ZeroOrOne
  type Direction ContentID = 'RequestAndResponse


  parseFromHeaders _ (wire :| [])
    | isBatchIdentifier wire = Right $! ContentID (STU.fromByteStringUnsafe wire)
  parseFromHeaders _ fields = parseSingleFieldLine "Content-ID" contentIDParser fields


  renderToHeaders _ = ST.toByteString . contentID


  renderToHeadersChecked _ (ContentID ident)
    | isBatchIdentifier wire = Just wire
    | otherwise =
        either (const Nothing) (const (Just wire)) $
          parseSingleFieldLine "Content-ID" (contentIdentifierBytes outboundDRPContentIdentifierBytes) (wire :| [])
    where
      wire = ST.toByteString ident


  headerName _ = hContentID


contentIDParser :: ParserT st String ContentID
contentIDParser =
  ContentID <$> shortASCIIFromParser_ (contentIdentifierBytes (drpIdentifierBytes absoluteURIBytes))


contentIdentifierBytes :: ParserT st String () -> ParserT st String ()
contentIdentifierBytes drp = mimeMessageId <|> bareIdentifier
  where
    bareIdentifier =
      branch
        (lookahead (uriSchemeBytes *> $(char ':')))
        drp
        (skipSome (skipSatisfyAscii unreserved))
{-# INLINE contentIdentifierBytes #-}


isBatchIdentifier :: BS.ByteString -> Bool
isBatchIdentifier wire = not (BS.null wire) && BSC.all unreserved wire
{-# INLINE isBatchIdentifier #-}


unreserved :: Char -> Bool
unreserved c = case unreservedBytes of
  AsciiSet low high -> asciiMember low high c
{-# INLINE unreserved #-}


unreservedBytes :: AsciiSet
unreservedBytes = asciiSet (CS.range 'A' 'Z' <> CS.range 'a' 'z' <> CS.range '0' '9' <> "-._~")


-- | The DRP content identifier shared by Content-ID and Differential-ID.
drpContentIdentifier :: ParserT st String ST.ShortText
drpContentIdentifier = shortASCIIFromParser_ (drpIdentifierBytes absoluteURIBytes)


-- | Validate the outbound DRP list, including each HTTP URI authority.
outboundDRPContentIdentifierBytes :: ParserT st String ()
outboundDRPContentIdentifierBytes = drpIdentifierBytes outboundAbsoluteURIBytes


drpIdentifierBytes :: ParserT st String () -> ParserT st String ()
drpIdentifierBytes syntax = member *> skipMany ($(char ',') *> member)
  where
    member = withSpan (skipSome (skipSatisfyAscii uriOctet)) $ \() range ->
      inSpan range (syntax <* eof)
    uriOctet c = '!' <= c && c <= '~' && c /= ','
{-# INLINE drpIdentifierBytes #-}


mimeMessageId :: ParserT st String ()
mimeMessageId = do
  cfws
  $(char '<')
  word *> skipMany ($(char '.') *> word)
  $(char '@')
  domainLiteral <|> (atom *> skipMany ($(char '.') *> atom))
  $(char '>')
  skipMany (ows *> mimeComment)
  where
    word = cfws *> (atomText <|> asciiOnly httpQuotedWireBytes) <* cfws
    atom = cfws *> atomText <* cfws
    domainLiteral = cfws *> $(char '[') *> skipMany literalByte *> $(char ']') <* cfws
    literalByte =
      skipSatisfyAscii (\c -> isOWS c || ('!' <= c && c <= 'Z') || ('^' <= c && c <= '~'))
        <|> ($(char '\\') *> skipSatisfyAscii (\c -> isOWS c || ('!' <= c && c <= '~')))
    cfws = ows *> skipMany (mimeComment *> ows)
    mimeComment = asciiOnly comment
    asciiOnly parser = withByteString parser $ \_ bytes ->
      if BS.all (< 0x80) bytes then pure () else failed


atomText :: ParserT st String ()
atomText = case atomBytes of
  AsciiSet low high -> skipSome (skipSatisfyAscii (asciiMember low high))
{-# INLINE atomText #-}


atomBytes :: AsciiSet
atomBytes = asciiSet (CS.range 'A' 'Z' <> CS.range 'a' 'z' <> CS.range '0' '9' <> "!#$%&'*+-/=?^_`{|}~")


renderContentID :: ContentID -> M.Builder
renderContentID (ContentID ident) = shortText ident
