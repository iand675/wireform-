{-# LANGUAGE TemplateHaskell #-}

{- |
@X-Content-Type-Options@ — a de-facto response header whose single
meaningful value, @nosniff@, instructs the user agent to block
requests whose computed MIME type disagrees with the declared
@Content-Type@ (i.e. to disable content-type sniffing).

The only value defined by the Fetch standard is the case-insensitive
token @nosniff@. The typed codec also preserves other singleton tokens for
compatibility. 'hasNoSniff' applies Fetch's first-member rule to a received
header map.

Spec: <https://fetch.spec.whatwg.org/#x-content-type-options-header>

See also: "Network.HTTP.Headers.ContentType", "Network.HTTP.Headers.ContentSecurityPolicy", "Network.HTTP.Headers.XFrameOptions", "Network.HTTP.Headers.XDownloadOptions".
-}
module Network.HTTP.Headers.XContentTypeOptions (
  XContentTypeOptions (..),
  hasNoSniff,
  xContentTypeOptionsParser,
  renderXContentTypeOptions,
) where

import Data.Bits ((.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXContentTypeOptions)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | @X-Content-Type-Options@ value.
data XContentTypeOptions
  = -- | The @nosniff@ directive — disables MIME sniffing.
    NoSniff
  | -- | Any other (non-standard) value, preserved verbatim.
    XContentTypeOptionsOther !ST.ShortText
  deriving stock (Eq, Show)


instance KnownHeader XContentTypeOptions where
  type ParseFailure XContentTypeOptions = String
  type Cardinality XContentTypeOptions = 'ZeroOrOne
  type Direction XContentTypeOptions = 'Response


  parseFromHeaders _ (wire :| []) | bareNoSniff wire = Right NoSniff
  parseFromHeaders _ fields = parseSingleFieldLine "X-Content-Type-Options" xContentTypeOptionsParser fields


  renderToHeaders _ NoSniff = "nosniff"
  renderToHeaders _ (XContentTypeOptionsOther token) = ST.toByteString token


  renderToHeadersChecked _ NoSniff = Just "nosniff"
  renderToHeadersChecked _ (XContentTypeOptionsOther token)
    | isHttpTokenText token, not (bareNoSniff wire) = Just wire
    | otherwise = Nothing
    where
      wire = ST.toByteString token


  headerName _ = hXContentTypeOptions


xContentTypeOptionsParser :: ParserT st String XContentTypeOptions
xContentTypeOptionsParser =
  ows *> (recognized <|> (XContentTypeOptionsOther <$> rfc9110Token)) <* ows
  where
    recognized = NoSniff <$ (noSniffToken <* notFollowedBy (skipSatisfyAscii isTokenChar))


noSniffToken :: ParserT st e ()
noSniffToken = $(switchAsciiCI [|case _ of "nosniff" -> pure ()|])
{-# INLINE noSniffToken #-}


-- Check the exact length before reading the unpadded token's byte offsets.
bareNoSniff :: BS.ByteString -> Bool
bareNoSniff wire =
  BS.length wire == 7
    && byte 0 == 0x6e
    && byte 1 == 0x6f
    && byte 2 == 0x73
    && byte 3 == 0x6e
    && byte 4 == 0x69
    && byte 5 == 0x66
    && byte 6 == 0x66
  where
    byte i = BSU.unsafeIndex wire i .|. 0x20


{- | Fetch's @determine nosniff@ algorithm. Only the first comma-separated
member can match. Fetch preserves quotation marks when splitting, so any
quote before the first comma rules out a match.
-}
hasNoSniff :: HeaderMap -> Bool
hasNoSniff headers = case lookupRawHeader hXContentTypeOptions headers of
  Just (wire :| _) | bareNoSniff wire -> True
  Just (value :| _) -> case runParser firstMember value of
    OK () _ -> True
    _ -> False
  Nothing -> False
  where
    firstMember :: ParserT () String ()
    firstMember = ows *> noSniffToken *> ows *> (eof <|> $(char ','))


renderXContentTypeOptions :: XContentTypeOptions -> M.Builder
renderXContentTypeOptions = \case
  NoSniff -> "nosniff"
  XContentTypeOptionsOther v -> shortText v
