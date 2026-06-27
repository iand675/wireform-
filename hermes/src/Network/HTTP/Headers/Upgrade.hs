{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 9110 §7.8 @Upgrade@ — lists the application-layer protocols the
sender would prefer to switch to (e.g. for a @101 Switching Protocols@
response, or HTTP/2's @h2c@ cleartext negotiation).

== Grammar

@
Upgrade          = #protocol
protocol         = protocol-name ["/" protocol-version]
protocol-name    = token
protocol-version = token
@

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-7.8>

See also: "Network.HTTP.Headers.Connection", "Network.HTTP.Headers.HTTP2Settings",
"Network.HTTP.Headers.ALPN", "Network.HTTP.Headers.SecWebSocketKey".
-}
module Network.HTTP.Headers.Upgrade (
  Upgrade (..),
  Protocol (..),
  upgradeParser,
  renderUpgrade,
) where

import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup (sconcat)
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hUpgrade)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (sepByCommas1, shortText)


-- | A single protocol token: a name with an optional @\/version@.
data Protocol = Protocol
  { protocolName :: !ST.ShortText
  , protocolVersion :: !(Maybe ST.ShortText)
  }
  deriving stock (Eq, Show)


-- | An @Upgrade@ header value: a non-empty list of protocols.
newtype Upgrade = Upgrade {upgradeProtocols :: NonEmpty Protocol}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup)


instance KnownHeader Upgrade where
  type ParseFailure Upgrade = String
  type Cardinality Upgrade = 'ZeroOrMore
  type Direction Upgrade = 'RequestAndResponse


  parseFromHeaders _ neHeaders = sconcat <$> traverse parseLine neHeaders
    where
      parseLine bs = case runParser upgradeParser bs of
        OK v "" -> Right v
        OK _ rest -> Left $ "Unconsumed input after parsing Upgrade header: " <> show rest
        Fail -> Left "Failed to parse Upgrade header"
        Err e -> Left e
  renderToHeaders _ = pure . M.toStrictByteString . renderUpgrade
  headerName _ = hUpgrade


upgradeParser :: ParserT st String Upgrade
upgradeParser = Upgrade <$> (protocolParser `sepBy1` (ows *> $(char ',') *> ows))


protocolParser :: ParserT st String Protocol
protocolParser = do
  name <- rfc9110Token
  version <- optional ($(char '/') *> rfc9110Token)
  pure (Protocol name version)


renderUpgrade :: Upgrade -> M.Builder
renderUpgrade (Upgrade protos) = sepByCommas1 (fmap renderProtocol protos)


renderProtocol :: Protocol -> M.Builder
renderProtocol (Protocol name version) =
  shortText name <> maybe mempty (\v -> M.char7 '/' <> shortText v) version
