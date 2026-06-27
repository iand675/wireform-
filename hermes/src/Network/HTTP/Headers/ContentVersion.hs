{- |
@Content-Version@ — obsolete HTTP entity-header (RFC 2068 §19.6.2.2) carrying
the version tag of an evolving entity, intended to support collaborative
authoring together with @Derived-From@.

== Grammar (RFC 2068 §19.6.2.2)

@
Content-Version = \"Content-Version\" \":\" quoted-string
@

The version tag is opaque (e.g. @\"2.1.2\"@, @\"Fred 19950116-12:26:48\"@,
@\"2.5a4-omega7\"@), so we capture its decoded contents as 'ST.ShortText' and
re-emit it as an RFC 9110 §5.6.4 quoted-string (DQUOTE / backslash escaped).

Spec: <https://www.rfc-editor.org/rfc/rfc2068#section-19.6.2.2>

See also: "Network.HTTP.Headers.DerivedFrom", "Network.HTTP.Headers.ETag", "Network.HTTP.Headers.URI".
-}
module Network.HTTP.Headers.ContentVersion (
  ContentVersion (..),
  contentVersionParser,
  renderContentVersion,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hContentVersion)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- | @Content-Version@ value: the decoded version tag from the quoted-string.
newtype ContentVersion = ContentVersion {contentVersionTag :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader ContentVersion where
  type ParseFailure ContentVersion = String
  type Cardinality ContentVersion = 'ZeroOrOne
  type Direction ContentVersion = 'RequestAndResponse


  parseFromHeaders _ headers = case runParser contentVersionParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Content-Version header: " <> show rest
    Fail -> Left "Failed to parse Content-Version header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderContentVersion


  headerName _ = hContentVersion


contentVersionParser :: ParserT st String ContentVersion
contentVersionParser = ows *> (ContentVersion . unsafeToRFC8941String <$> rfc8941String)


renderContentVersion :: ContentVersion -> M.Builder
renderContentVersion (ContentVersion tag) = R.rfc8941String (RFC8941String tag)
