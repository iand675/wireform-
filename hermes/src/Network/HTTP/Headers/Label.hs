{- |
@Label@ (RFC 3253 §8.3) — request header used by WebDAV versioning
methods to select the version of a version-controlled resource that
carries the given label name.

@
Label = \"Label\" \":\" label-name
@

The label is URL-escaped UTF-8. The wire spelling and its case are
retained for comparison. 'labelText' returns the decoded text.

Spec: <https://www.rfc-editor.org/rfc/rfc3253.html#section-8.3>

See also: "Network.HTTP.Headers.DAV", "Network.HTTP.Headers.Depth", "Network.HTTP.Headers.If", "Network.HTTP.Headers.LockToken".
-}
module Network.HTTP.Headers.Label (
  Label (..),
  labelText,
  labelFromText,
  labelParser,
  renderLabel,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.CharSet.Posix.Ascii (alnum)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hLabel)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)
import Network.HTTP.QueryParameters (urlEncodeBuilder)
import Network.HTTP.URL.Decode (urlDecodeMaybe)
import Wireform.FFI (validateUtf8SWAR)


-- | The URL-escaped, case-sensitive spelling of the label name.
newtype Label = Label {labelName :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader Label where
  type ParseFailure Label = String
  type Cardinality Label = 'ZeroOrOne
  type Direction Label = 'Request


  parseFromHeaders _ = parseSingleFieldLine "Label" labelValue


  renderToHeaders _ = M.toStrictByteString . renderLabel


  renderToHeadersChecked _ (Label name) =
    let wire = ST.toByteString name
    in either (const Nothing) (const (Just wire)) $
         parseSingleFieldLine "Label" labelSpelling (wire :| [])


  headerName _ = hLabel


labelParser :: ParserT st String Label
labelParser = ows *> labelValue <* ows


labelValue :: ParserT st String Label
labelValue = Label . STU.fromByteStringUnsafe <$> labelSpelling


labelSpelling :: ParserT st String ByteString
labelSpelling = withByteString escapedLabel $ \() bytes ->
  if not (BS.elem 37 bytes) || maybe False validateUtf8SWAR (urlDecodeMaybe bytes)
    then pure bytes
    else err "Invalid percent-encoded UTF-8 in Label"


-- RFC 2396 uric, plus the percent introducer checked by the decoder.
escapedLabel :: ParserT st e ()
escapedLabel = case labelChars of
  AsciiSet low high -> skipMany (skipSatisfyAscii (asciiMember low high))
{-# INLINE escapedLabel #-}


labelChars :: AsciiSet
labelChars = asciiSet (alnum <> "-_.!~*'();/?:@&=+$,%")


-- | Decode a label name. This may fail for a manually supplied wire spelling.
labelText :: Label -> Maybe Text
labelText (Label name) = urlDecodeMaybe (ST.toByteString name) >>= either (const Nothing) Just . TE.decodeUtf8'


-- | Construct a label from text using uppercase percent escapes.
labelFromText :: Text -> Label
labelFromText = Label . STU.fromByteStringUnsafe . M.toStrictByteString . urlEncodeBuilder


renderLabel :: Label -> M.Builder
renderLabel (Label name) = shortText name
