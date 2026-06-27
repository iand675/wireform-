{- |
The @Accept-CH@ HTTP response header advertises support for Client Hints: the
server lists the client-hint request header field names it is willing to
receive on subsequent requests to the origin, opting in to client-hint content
negotiation. Its value is an RFC 8941 Structured Field List of tokens, each
naming a client-hint request header (for example @Sec-CH-UA@, @DPR@, @Width@).

Spec: <https://www.rfc-editor.org/rfc/rfc8942> (RFC 8942, HTTP Client Hints;
the Structured Field syntax is RFC 8941).

See also: "Network.HTTP.Headers.SaveData", "Network.HTTP.Headers.SecPurpose", "Network.HTTP.Headers.SecGPC", "Network.HTTP.Headers.XUACompatible", "Network.HTTP.Headers.Vary", "Network.HTTP.Headers.UserAgent".
-}
module Network.HTTP.Headers.AcceptCH (
  AcceptCH (..),
  acceptCHParser,
  renderAcceptCH,
) where

import qualified Data.ByteString as B
import Data.Foldable1 (fold1)
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


  parseFromHeaders _ headers = do
    res <- traverse runAcceptCHParser headers
    pure (fold1 res)


  renderToHeaders _ = pure . M.toStrictByteString . renderAcceptCH


  headerName _ = hAcceptCH


runAcceptCHParser :: B.ByteString -> Either String AcceptCH
runAcceptCHParser bs = case runParser acceptCHParser bs of
  OK v "" -> Right v
  OK _ rest -> Left ("Unconsumed input after parsing Accept-CH header: " <> show rest)
  Fail -> Left "Failed to parse Accept-CH header"
  Err e -> Left e


acceptCHParser :: ParserT st String AcceptCH
acceptCHParser = AcceptCH <$> rfc8941List rfc8941Token


renderAcceptCH :: AcceptCH -> M.Builder
renderAcceptCH (AcceptCH hints) = M.intersperse ", " (map R.rfc8941Token hints)
