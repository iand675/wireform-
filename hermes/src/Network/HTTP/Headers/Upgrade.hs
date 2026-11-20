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

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
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


-- | Upgrade options in preference order. 'UpgradeEmpty' offers no protocol.
data Upgrade
  = Upgrade {upgradeProtocols :: NonEmpty Protocol}
  | UpgradeEmpty
  deriving stock (Eq, Show)


instance Semigroup Upgrade where
  UpgradeEmpty <> other = other
  other <> UpgradeEmpty = other
  Upgrade xs <> Upgrade ys = Upgrade (xs <> ys)


instance KnownHeader Upgrade where
  type ParseFailure Upgrade = String
  type Cardinality Upgrade = 'ZeroOrMore
  type Direction Upgrade = 'RequestAndResponse


  parseFromHeaders _ = fmap fromProtocols . parseListFieldLines "Upgrade" (httpList protocolParser)
  renderToHeaders _ = pure . M.toStrictByteString . renderUpgrade
  headerName _ = hUpgrade


upgradeParser :: ParserT st String Upgrade
upgradeParser = fromProtocols <$> httpList protocolParser


fromProtocols :: [Protocol] -> Upgrade
fromProtocols = maybe UpgradeEmpty Upgrade . NE.nonEmpty


protocolParser :: ParserT st String Protocol
protocolParser = do
  name <- rfc9110Token
  version <- optional ($(char '/') *> rfc9110Token)
  pure (Protocol name version)


renderUpgrade :: Upgrade -> M.Builder
renderUpgrade UpgradeEmpty = mempty
renderUpgrade (Upgrade protos) = sepByCommas1 (fmap renderProtocol protos)


renderProtocol :: Protocol -> M.Builder
renderProtocol (Protocol name version) =
  shortText name <> maybe mempty (\v -> M.char7 '/' <> shortText v) version
