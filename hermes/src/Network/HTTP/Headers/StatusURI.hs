{- |
@Status-URI@ (RFC 2518 §9.7) — response header used with the
@102 (Processing)@ status code to report, to a client, the status of
one or more resources affected by a long-running method.

@
Status-URI = \"Status-URI\" \":\" *(Status-Code Coded-URL)
Coded-URL  = \"\<\" absoluteURI \"\>\"
@

@Status-Code@ is a 3-digit HTTP status code in the range 100–599; @Coded-URL@ is an absolute
URI wrapped in angle brackets. Each pair couples a status code with the
resource it applies to.

Spec: <https://www.rfc-editor.org/rfc/rfc2518.html#section-9.7>

See also: "Network.HTTP.Headers.DAV", "Network.HTTP.Headers.LockToken", "Network.HTTP.Headers.Timeout".
-}
module Network.HTTP.Headers.StatusURI (
  StatusURI (..),
  StatusURIEntry (..),
  statusURIParser,
  renderStatusURI,
) where

import Data.Char (ord)
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.URIReference (codedURL, outboundAbsoluteURIBytes, validURIText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hStatusURI)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | A single @Status-Code Coded-URL@ pair.
data StatusURIEntry = StatusURIEntry
  { statusURICode :: !Int
  -- ^ The HTTP status code (e.g. @423@).
  , statusURIRef :: !ST.ShortText
  -- ^ The absolute URI, without its surrounding angle brackets.
  }
  deriving stock (Eq, Show)


-- | Zero or more status/URI pairs.
newtype StatusURI = StatusURI {statusURIEntries :: [StatusURIEntry]}
  deriving stock (Eq, Show)


instance KnownHeader StatusURI where
  type ParseFailure StatusURI = String
  type Cardinality StatusURI = 'ZeroOrOne
  type Direction StatusURI = 'Response


  parseFromHeaders _ = parseSingleFieldLine "Status-URI" statusURIValue


  renderToHeaders _ = M.toStrictByteString . renderStatusURI


  renderToHeadersChecked settings value@(StatusURI entries)
    | all valid entries = Just (renderToHeaders settings value)
    | otherwise = Nothing
    where
      valid (StatusURIEntry code uri) =
        code >= 100 && code <= 599 && validURIText outboundAbsoluteURIBytes uri


  headerName _ = hStatusURI


statusURIParser :: ParserT st String StatusURI
statusURIParser = ows *> statusURIValue <* ows


statusURIValue :: ParserT st String StatusURI
statusURIValue = StatusURI <$> many (statusURIEntry <* ows)
  where
    statusURIEntry = StatusURIEntry <$> statusCode <*> (ows *> codedURL)


statusCode :: ParserT st String Int
statusCode = do
  hundreds <- withSatisfyAscii (\c -> '1' <= c && c <= '5') digit
  tens <- withSatisfyAscii isDigit digit
  ones <- withSatisfyAscii isDigit digit
  pure (hundreds * 100 + tens * 10 + ones)
  where
    digit = pure . subtract (ord '0') . ord
{-# INLINE statusCode #-}


renderStatusURI :: StatusURI -> M.Builder
renderStatusURI (StatusURI entries) = M.intersperse " " (map renderEntry entries)
  where
    renderEntry (StatusURIEntry code ref) =
      M.intDec code <> " <" <> shortText ref <> ">"
