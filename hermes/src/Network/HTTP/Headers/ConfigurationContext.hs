{- |
The @Configuration-Context@ HTTP request header, defined by OSLC Configuration
Management. A client sends it to select
the /configuration context/ — an OSLC configuration resource URI — within which
the (otherwise version-agnostic) concept-resource URIs in the request are
resolved to specific versions. A server that honours it should add
@Vary: Configuration-Context@ to its response so that caches key on the context.

The codec validates an RFC 3986 URI with a scheme and optional fragment.
The URI's spelling is preserved for forwarding and comparison.

Spec: OSLC Configuration Management Part 3, section 4,
<https://docs.oasis-open-projects.org/oslc-op/config/v1.0/config-resources.html>

See also: "Network.HTTP.Headers.OSLCCoreVersion", "Network.HTTP.Headers.Vary".
-}
module Network.HTTP.Headers.ConfigurationContext (
  ConfigurationContext (..),
  configurationContextParser,
  renderConfigurationContext,
) where

import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.URIReference (checkedURIField, outboundAbsoluteURIWithFragmentBytes, uriWithFragment)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hConfigurationContext)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | @Configuration-Context@ value: the configuration resource URI.
newtype ConfigurationContext = ConfigurationContext {configurationContextValue :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader ConfigurationContext where
  type ParseFailure ConfigurationContext = String
  type Cardinality ConfigurationContext = 'ZeroOrOne
  type Direction ConfigurationContext = 'Request


  parseFromHeaders _ = parseSingleFieldLine "Configuration-Context" configurationContextParser


  renderToHeaders _ = ST.toByteString . configurationContextValue


  renderToHeadersChecked _ =
    checkedURIField outboundAbsoluteURIWithFragmentBytes . ST.toByteString . configurationContextValue


  headerName _ = hConfigurationContext


configurationContextParser :: ParserT st String ConfigurationContext
configurationContextParser = ConfigurationContext <$> uriWithFragment


renderConfigurationContext :: ConfigurationContext -> M.Builder
renderConfigurationContext (ConfigurationContext v) = shortText v
