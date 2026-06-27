{- |
@Derived-From@ — obsolete HTTP entity-header (RFC 2068 §19.6.2.3) indicating
the version tag of the resource an enclosed entity was derived from before the
sender's modifications; required on PUT/PATCH when the prior retrieval carried
a @Content-Version@.

== Grammar (RFC 2068 §19.6.2.3)

@
Derived-From = \"Derived-From\" \":\" quoted-string
@

As with @Content-Version@ the tag is opaque (e.g. @\"2.1.1\"@); its decoded
contents are captured as 'ST.ShortText' and re-emitted as an RFC 9110 §5.6.4
quoted-string (DQUOTE / backslash escaped).

Spec: <https://www.rfc-editor.org/rfc/rfc2068#section-19.6.2.3>

See also: "Network.HTTP.Headers.ContentVersion", "Network.HTTP.Headers.ETag", "Network.HTTP.Headers.IfMatch".
-}
module Network.HTTP.Headers.DerivedFrom (
  DerivedFrom (..),
  derivedFromParser,
  renderDerivedFrom,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hDerivedFrom)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- | @Derived-From@ value: the decoded version tag from the quoted-string.
newtype DerivedFrom = DerivedFrom {derivedFromTag :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader DerivedFrom where
  type ParseFailure DerivedFrom = String
  type Cardinality DerivedFrom = 'ZeroOrOne
  type Direction DerivedFrom = 'Request


  parseFromHeaders _ headers = case runParser derivedFromParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Derived-From header: " <> show rest
    Fail -> Left "Failed to parse Derived-From header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderDerivedFrom


  headerName _ = hDerivedFrom


derivedFromParser :: ParserT st String DerivedFrom
derivedFromParser = ows *> (DerivedFrom . unsafeToRFC8941String <$> rfc8941String)


renderDerivedFrom :: DerivedFrom -> M.Builder
renderDerivedFrom (DerivedFrom tag) = R.rfc8941String (RFC8941String tag)
