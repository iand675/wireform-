{- |
The @Configuration-Context@ HTTP request header, defined by OSLC Configuration
Management and registered provisionally with IANA. A client sends it to select
the /configuration context/ — an OSLC configuration resource URI — within which
the (otherwise version-agnostic) concept-resource URIs in the request are
resolved to specific versions. A server that honours it should add
@Vary: Configuration-Context@ to its response so that caches key on the context.

The value is an opaque URI with no further structure to interpret, so it is
preserved verbatim in a newtype, mirroring the treatment of other opaque
provisional fields.

Spec: provisional IANA registration; defined by OSLC Configuration Management,
<https://docs.oasis-open-projects.org/oslc-op/config/v1.0/config-resources.html>

See also: "Network.HTTP.Headers.OSLCCoreVersion", "Network.HTTP.Headers.Vary".
-}
module Network.HTTP.Headers.ConfigurationContext (
  ConfigurationContext (..),
  configurationContextParser,
  renderConfigurationContext,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hConfigurationContext)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


{- | @Configuration-Context@ value: the opaque configuration context token,
preserved verbatim.
-}
newtype ConfigurationContext = ConfigurationContext {configurationContextValue :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader ConfigurationContext where
  type ParseFailure ConfigurationContext = String
  type Cardinality ConfigurationContext = 'ZeroOrOne
  type Direction ConfigurationContext = 'RequestAndResponse


  parseFromHeaders _ headers = case runParser configurationContextParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Configuration-Context header: " <> show rest
    Fail -> Left "Failed to parse Configuration-Context header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderConfigurationContext


  headerName _ = hConfigurationContext


configurationContextParser :: ParserT st String ConfigurationContext
configurationContextParser = ConfigurationContext <$> takeRestShortText


renderConfigurationContext :: ConfigurationContext -> M.Builder
renderConfigurationContext (ConfigurationContext v) = shortText v
