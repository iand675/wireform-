{- |
RFC 4918 §10.5 @Lock-Token@ header — identifies the lock to be removed
by an @UNLOCK@ request or the lock created by a successful @LOCK@ response.

== Grammar

@
Lock-Token = Coded-URL
Coded-URL  = "<" absolute-URI ">"
@

Spec: <https://www.rfc-editor.org/rfc/rfc4918#section-10.5>

See also: "Network.HTTP.Headers.If", "Network.HTTP.Headers.Timeout", "Network.HTTP.Headers.Depth", "Network.HTTP.Headers.DAV".
-}
module Network.HTTP.Headers.LockToken (
  LockToken (..),
  lockTokenParser,
  renderLockToken,
) where

import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.URIReference (codedURL, outboundAbsoluteURIBytes, validURIText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hLockToken)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | The lock token URI, stored without its surrounding @<@ @>@ delimiters.
newtype LockToken = LockToken {lockTokenUri :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader LockToken where
  type ParseFailure LockToken = String
  type Cardinality LockToken = 'ZeroOrOne
  type Direction LockToken = 'RequestAndResponse


  parseFromHeaders _ = parseSingleFieldLine "Lock-Token" lockTokenParser


  renderToHeaders _ = M.toStrictByteString . renderLockToken


  renderToHeadersChecked settings value@(LockToken uri)
    | validURIText outboundAbsoluteURIBytes uri = Just (renderToHeaders settings value)
    | otherwise = Nothing


  headerName _ = hLockToken


lockTokenParser :: ParserT st String LockToken
lockTokenParser = LockToken <$> codedURL


renderLockToken :: LockToken -> M.Builder
renderLockToken (LockToken uri) = M.char7 '<' <> shortText uri <> M.char7 '>'
