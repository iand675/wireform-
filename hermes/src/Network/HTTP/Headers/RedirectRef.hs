{- |
@Redirect-Ref@ (RFC 4437 §12.2) — response header returned in every
@3xx@ response from a WebDAV redirect reference resource, carrying the
link target that was specified when the redirect reference was created.

@
Redirect-Ref = \"Redirect-Ref:\" ( URI | relative-ref )
@

The value is a URI or relative reference (RFC 3986 §3 / §4.2),
preserved verbatim as a faithful raw newtype.

Spec: <https://www.rfc-editor.org/rfc/rfc4437.html#section-12.2>

See also: "Network.HTTP.Headers.ApplyToRedirectRef", "Network.HTTP.Headers.Destination", "Network.HTTP.Headers.Location".
-}
module Network.HTTP.Headers.RedirectRef (
  RedirectRef (..),
  redirectRefParser,
  renderRedirectRef,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hRedirectRef)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | The redirect link target (absolute or relative reference), verbatim.
newtype RedirectRef = RedirectRef {redirectRefTarget :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader RedirectRef where
  type ParseFailure RedirectRef = String
  type Cardinality RedirectRef = 'ZeroOrOne
  type Direction RedirectRef = 'Response


  parseFromHeaders _ headers = case runParser redirectRefParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Redirect-Ref header: " <> show rest
    Fail -> Left "Failed to parse Redirect-Ref header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderRedirectRef


  headerName _ = hRedirectRef


redirectRefParser :: ParserT st String RedirectRef
redirectRefParser = RedirectRef <$> takeRestShortText


renderRedirectRef :: RedirectRef -> M.Builder
renderRedirectRef (RedirectRef target) = shortText target
