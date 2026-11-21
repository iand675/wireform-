{- |
The @Accept-CH@ HTTP response header advertises support for Client Hints: the
server lists the client-hint request header field names it is willing to
receive on subsequent requests to the origin, opting in to client-hint content
negotiation. Its value is an RFC 8941 Structured Field List of tokens, each
naming a client-hint request header (for example @Sec-CH-UA@, @DPR@, @Width@).
The codec validates member parameters and retains the names. An explicitly
empty field clears the advertised set.

Spec: <https://www.rfc-editor.org/rfc/rfc8942> (RFC 8942, HTTP Client Hints;
the Structured Field syntax is RFC 8941).

See also: "Network.HTTP.Headers.SaveData", "Network.HTTP.Headers.SecPurpose", "Network.HTTP.Headers.SecGPC", "Network.HTTP.Headers.XUACompatible", "Network.HTTP.Headers.Vary", "Network.HTTP.Headers.UserAgent".
-}
module Network.HTTP.Headers.AcceptCH (
  AcceptCH (..),
  acceptCHParser,
  renderAcceptCH,
  renderAcceptCHChecked,
) where

import Data.ByteString.Internal (w2c)
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBSI
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAcceptCH)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- | The list of client-hint header-name tokens advertised by @Accept-CH@.
newtype AcceptCH = AcceptCH {acceptCHHints :: [RFC8941Token]}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup)


instance KnownHeader AcceptCH where
  type ParseFailure AcceptCH = String
  type Cardinality AcceptCH = 'ZeroOrMore
  type Direction AcceptCH = 'Response


  parseFromHeaders _ (line :| []) = parseFieldLine "Accept-CH" acceptCHParser line
  parseFromHeaders _ fields = AcceptCH <$> parseListFieldLines "Accept-CH" populatedHintList fields
    where
      populatedHintList = do
        hints <- hintList
        case hints of
          [] -> failed
          _ -> pure hints


  renderToHeaders _ (AcceptCH []) = [""]
  renderToHeaders _ (AcceptCH hints@(first : rest))
    | validHints hints = [R.encodeCommaSeparatedText (fmap unsafeToRFC8941Token (first :| rest))]
    | otherwise = []


  headerName _ = hAcceptCH


acceptCHParser :: ParserT st String AcceptCH
acceptCHParser = AcceptCH <$> hintList


hintList :: ParserT st String [RFC8941Token]
hintList = ows *> rfc8941List hint <* ows
  where
    -- A hint is both a Structured Fields Token and an HTTP field name.
    hint = (RFC8941Token <$> shortASCIIFromParser_ hintName) <* skipStructuredFieldParameters
    hintName = lookahead (skipSatisfyAscii (\c -> isLatinLetter c || c == '*')) *> tokenBytes


-- | Render the field value. An invalid hint name produces no bytes.
renderAcceptCH :: AcceptCH -> M.Builder
renderAcceptCH = either (const mempty) id . renderAcceptCHChecked


-- | Require each hint to be both a Structured Fields Token and a field name.
renderAcceptCHChecked :: AcceptCH -> Either String M.Builder
renderAcceptCHChecked (AcceptCH hints)
  | validHints hints = Right (R.renderSeparated ", " R.rfc8941Token hints)
  | otherwise = Left "Invalid Accept-CH field name"


validHints :: [RFC8941Token] -> Bool
validHints = all validHint
  where
    validHint (RFC8941Token token) =
      size > 0 && (isLatinLetter (at 0) || at 0 == '*') && go 1
      where
        bytes = ST.toShortByteString token
        size = SBS.length bytes
        at i = w2c (SBSI.unsafeIndex bytes i)
        go i = i == size || (isTokenChar (at i) && go (i + 1))
{-# INLINE validHints #-}
