{-# LANGUAGE TemplateHaskell #-}

{- |
The @Host@ request header carries the target URI's authority — the host and
optional port the request is addressed to — letting a single server (or
intermediary) distinguish the many origins it may serve on one IP address.
It is mandatory in HTTP\/1.1; in HTTP\/2 and HTTP\/3 it corresponds to the
@:authority@ pseudo-header.

@
Host = uri-host [ ":" port ]
@

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-7.2>

See also: "Network.HTTP.Headers.AltUsed", "Network.HTTP.Headers.Via", "Network.HTTP.Headers.XForwardedHost", "Network.HTTP.Headers.Forwarded".
-}
module Network.HTTP.Headers.Host (
  Host (..),
  hostParser,
  renderHost,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Data.Word (Word16)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hHost)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | Host header value representing a hostname and optional port
data Host = Host
  { hostName :: !ST.ShortText
  -- ^ The hostname (e.g. "example.com")
  , hostPort :: !(Maybe Word16)
  -- ^ Optional port number
  }
  deriving stock (Eq, Show)


instance KnownHeader Host where
  type ParseFailure Host = String
  type Cardinality Host = 'One
  type Direction Host = 'Request


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser hostParser header of
      OK host "" -> Right host
      OK _ rest -> Left $ "Unconsumed input after parsing Host header: " <> show rest
      Fail -> Left "Failed to parse Host header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderHost


  headerName _ = hHost


hostParser :: ParserT st String Host
hostParser = do
  hostname <- rfc9110Token
  port <- optional $ do
    $(char ':')
    anyAsciiDecimalWord
  pure $ Host hostname (fromIntegral <$> port)


renderHost :: Host -> M.Builder
renderHost (Host hostname mPort) =
  shortText hostname <> maybe mempty (\port -> M.char7 ':' <> M.word16Dec port) mPort
