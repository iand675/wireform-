{-# LANGUAGE TemplateHaskell #-}

{- |
@Keep-Alive@ — a request and response header that hints how a persistent
connection should be managed, carrying a @timeout@ (minimum idle seconds
the connection should stay open) and\/or @max@ (maximum further requests
the connection will accept before closing). It only takes effect
alongside @Connection: keep-alive@ on HTTP\/1.x and is prohibited in
HTTP\/2 and HTTP\/3. This field is de facto rather than IANA-registered:
it originates in the obsolete HTTP\/1.0 keep-alive proposal and is
documented chiefly by MDN.

== Grammar

@
Keep-Alive       = #keep-alive-param
keep-alive-param = token "=" ( token \/ quoted-string )
@

Spec (de facto): <https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/Keep-Alive>

See also: "Network.HTTP.Headers.Connection", "Network.HTTP.Headers.Close".
-}
module Network.HTTP.Headers.KeepAlive (
  KeepAlive (..),
  keepAliveParser,
  renderKeepAlive,
) where

import Control.Monad.Combinators (sepBy)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hKeepAlive)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | Keep-Alive header contains parameters like timeout and max.
newtype KeepAlive = KeepAlive {keepAliveParams :: [(ST.ShortText, ST.ShortText)]}
  deriving stock (Eq, Show)


instance KnownHeader KeepAlive where
  type ParseFailure KeepAlive = String
  type Cardinality KeepAlive = 'ZeroOrOne
  type Direction KeepAlive = 'RequestAndResponse


  parseFromHeaders _ headers = case runParser keepAliveParser $ NE.head headers of
    OK ka "" -> Right ka
    OK _ rest -> Left $ "Unconsumed input after parsing Keep-Alive header: " <> show rest
    Fail -> Left "Failed to parse Keep-Alive header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderKeepAlive


  headerName _ = hKeepAlive


keepAliveParser :: ParserT st String KeepAlive
keepAliveParser = KeepAlive <$> paramParser `sepBy` (ows *> $(char ',') *> ows)
  where
    paramParser = do
      key <- rfc9110Token
      $(char '=')
      val <- rfc9110Token
      pure (key, val)


renderKeepAlive :: KeepAlive -> M.Builder
renderKeepAlive (KeepAlive params) = M.intersperse ", " $ map renderParam params
  where
    renderParam (k, v) = shortText k <> "=" <> shortText v
