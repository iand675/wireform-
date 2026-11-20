{- |
@Apply-To-Redirect-Ref@ (RFC 4437 §12.2) — optional request header
usable on any request to a WebDAV redirect reference resource. When
present and set to @T@, the server applies the request to the redirect
reference resource itself and MUST NOT return a @3xx@ response; @F@
selects the default redirecting behaviour.

@
Apply-To-Redirect-Ref = \"Apply-To-Redirect-Ref\" \":\" ( \"T\" | \"F\" )
@

Spec: <https://www.rfc-editor.org/rfc/rfc4437.html#section-12.2>

See also: "Network.HTTP.Headers.RedirectRef", "Network.HTTP.Headers.Destination", "Network.HTTP.Headers.DAV", "Network.HTTP.Headers.Location".
-}
module Network.HTTP.Headers.ApplyToRedirectRef (
  ApplyToRedirectRef (..),
  applyToRedirectRefParser,
  renderApplyToRedirectRef,
) where

import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hApplyToRedirectRef)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


{- | @True@ corresponds to the literal @T@ (apply to the redirect
reference resource itself); @False@ corresponds to @F@.
-}
newtype ApplyToRedirectRef = ApplyToRedirectRef {applyToReference :: Bool}
  deriving stock (Eq, Show)


instance KnownHeader ApplyToRedirectRef where
  type ParseFailure ApplyToRedirectRef = String
  type Cardinality ApplyToRedirectRef = 'ZeroOrOne
  type Direction ApplyToRedirectRef = 'Request


  parseFromHeaders _ = parseTFFieldLine "Apply-To-Redirect-Ref" ApplyToRedirectRef


  renderToHeaders _ = M.toStrictByteString . renderApplyToRedirectRef


  headerName _ = hApplyToRedirectRef


applyToRedirectRefParser :: ParserT st String ApplyToRedirectRef
applyToRedirectRefParser = ApplyToRedirectRef <$> (ows *> tfFlag <* ows)


renderApplyToRedirectRef :: ApplyToRedirectRef -> M.Builder
renderApplyToRedirectRef (ApplyToRedirectRef b) = if b then "T" else "F"
