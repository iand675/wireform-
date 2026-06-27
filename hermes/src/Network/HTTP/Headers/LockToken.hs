{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 4918 §10.5 @Lock-Token@ request header — identifies, via its
lock token, the lock to be removed by an @UNLOCK@ request.

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

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
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
  type Direction LockToken = 'Request


  parseFromHeaders _ headers = case runParser lockTokenParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Lock-Token header: " <> show rest
    Fail -> Left "Failed to parse Lock-Token header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderLockToken


  headerName _ = hLockToken


lockTokenParser :: ParserT st String LockToken
lockTokenParser = do
  ows
  $(char '<')
  uri <- shortASCIIFromParser_ (skipSome (satisfyAscii (/= '>')))
  $(char '>')
  pure (LockToken uri)


renderLockToken :: LockToken -> M.Builder
renderLockToken (LockToken uri) = M.char7 '<' <> shortText uri <> M.char7 '>'
