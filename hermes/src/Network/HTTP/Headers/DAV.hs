{- |
RFC 4918 §10.1 @DAV@ — advertises WebDAV compliance classes, usually from
a resource in an @OPTIONS@ response. Extensions can use it in a request.

== Grammar

@
DAV              = "DAV" ":" #compliance-class
compliance-class = "1" | "2" | "3" | extend
extend           = Coded-URL | token
Coded-URL        = "<" absolute-URI ">"
@

Each comma-separated compliance class is either a bare token (the
core classes @1@, @2@, @3@, or an extension token) or a @Coded-URL@.

Spec: <https://www.rfc-editor.org/rfc/rfc4918#section-10.1>

See also: "Network.HTTP.Headers.DASL", "Network.HTTP.Headers.Depth", "Network.HTTP.Headers.LockToken", "Network.HTTP.Headers.Timeout", "Network.HTTP.Headers.Allow".
-}
module Network.HTTP.Headers.DAV (
  DAV (..),
  ComplianceClass (..),
  davClasses,
  davParser,
  renderDAV,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.URIReference (codedURL)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hDAV)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (sepByCommas1, shortText)


-- | A single advertised compliance class.
data ComplianceClass
  = -- | A bare token, e.g. @1@, @2@, @3@, or an extension token.
    ComplianceToken !ST.ShortText
  | -- | A @Coded-URL@ extension, stored without its @<@ @>@ delimiters.
    ComplianceURL !ST.ShortText
  deriving stock (Eq, Show)


-- | Advertised compliance classes, including an explicitly empty value.
data DAV
  = DAV {davComplianceClasses :: !(NE.NonEmpty ComplianceClass)}
  | DAVEmpty
  deriving stock (Eq, Show)


-- | All advertised classes, in their wire order.
davClasses :: DAV -> [ComplianceClass]
davClasses (DAV classes) = NE.toList classes
davClasses DAVEmpty = []


instance KnownHeader DAV where
  type ParseFailure DAV = String
  type Cardinality DAV = 'ZeroOrOne
  type Direction DAV = 'RequestAndResponse


  parseFromHeaders _ = fmap fromClasses . parseListFieldLines "DAV" (httpList complianceClassParser)


  renderToHeaders _ = M.toStrictByteString . renderDAV


  headerName _ = hDAV


complianceClassParser :: ParserT st String ComplianceClass
complianceClassParser = (ComplianceURL <$> codedURL) <|> (ComplianceToken <$> rfc9110Token)


davParser :: ParserT st String DAV
davParser = fromClasses <$> httpList complianceClassParser


fromClasses :: [ComplianceClass] -> DAV
fromClasses = maybe DAVEmpty DAV . NE.nonEmpty


renderDAV :: DAV -> M.Builder
renderDAV DAVEmpty = mempty
renderDAV (DAV classes) =
  sepByCommas1 (fmap renderClass classes)
  where
    renderClass (ComplianceToken t) = shortText t
    renderClass (ComplianceURL u) = M.char7 '<' <> shortText u <> M.char7 '>'
