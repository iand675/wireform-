{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 4918 §10.1 @DAV@ response header — advertises the WebDAV
compliance classes supported by a resource (sent on @OPTIONS@).

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
  davParser,
  renderDAV,
) where

import qualified Control.Monad.Combinators.NonEmpty as NEC
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hDAV)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | A single advertised compliance class.
data ComplianceClass
  = -- | A bare token, e.g. @1@, @2@, @3@, or an extension token.
    ComplianceToken !ST.ShortText
  | -- | A @Coded-URL@ extension, stored without its @<@ @>@ delimiters.
    ComplianceURL !ST.ShortText
  deriving stock (Eq, Show)


-- | The non-empty list of compliance classes advertised by the resource.
newtype DAV = DAV {davComplianceClasses :: NE.NonEmpty ComplianceClass}
  deriving stock (Eq, Show)


instance KnownHeader DAV where
  type ParseFailure DAV = String
  type Cardinality DAV = 'ZeroOrOne
  type Direction DAV = 'Response


  parseFromHeaders _ headers = case runParser davParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing DAV header: " <> show rest
    Fail -> Left "Failed to parse DAV header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderDAV


  headerName _ = hDAV


complianceClassParser :: ParserT st String ComplianceClass
complianceClassParser = codedURL <|> (ComplianceToken <$> rfc9110Token)
  where
    codedURL = do
      $(char '<')
      uri <- shortASCIIFromParser_ (skipSome (satisfyAscii (/= '>')))
      $(char '>')
      pure (ComplianceURL uri)


davParser :: ParserT st String DAV
davParser = DAV <$> (complianceClassParser `NEC.sepBy1` (ows *> $(char ',') *> ows))


renderDAV :: DAV -> M.Builder
renderDAV (DAV classes) =
  M.intersperse ", " (map renderClass (NE.toList classes))
  where
    renderClass (ComplianceToken t) = shortText t
    renderClass (ComplianceURL u) = M.char7 '<' <> shortText u <> M.char7 '>'
