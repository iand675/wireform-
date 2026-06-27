{-# LANGUAGE TemplateHaskell #-}

{- |
@X-Permitted-Cross-Domain-Policies@ — a /de-facto/ (non-IANA-registered)
response header originating with Adobe Flash/Acrobat. It controls which
cross-domain policy files (@crossdomain.xml@) a web client may honour for the
serving host, limiting cross-domain data loading by Flash, PDF, and similar
clients.

== Grammar (de-facto)

@
X-Permitted-Cross-Domain-Policies = "none"
                                  / "master-only"
                                  / "by-content-type"
                                  / "by-ftp-filename"
                                  / "all"
@

This is a /de-facto/ header with no governing RFC; see MDN,
<https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/X-Permitted-Cross-Domain-Policies>.

See also: "Network.HTTP.Headers.AccessControlAllowOrigin", "Network.HTTP.Headers.CrossOriginResourcePolicy", "Network.HTTP.Headers.XFrameOptions", "Network.HTTP.Headers.XContentTypeOptions".
-}
module Network.HTTP.Headers.XPermittedCrossDomainPolicies (
  XPermittedCrossDomainPolicies (..),
  xPermittedCrossDomainPoliciesParser,
  renderXPermittedCrossDomainPolicies,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXPermittedCrossDomainPolicies)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | A parsed @X-Permitted-Cross-Domain-Policies@ value.
data XPermittedCrossDomainPolicies
  = -- | @none@ — no policy files are permitted anywhere on the host.
    PcdpNone
  | -- | @master-only@ — only the master policy file is permitted.
    PcdpMasterOnly
  | -- | @by-content-type@ — only policy files served with a valid content type.
    PcdpByContentType
  | -- | @by-ftp-filename@ — only policy files with a valid name (FTP).
    PcdpByFtpFilename
  | -- | @all@ — all policy files on the host are permitted.
    PcdpAll
  deriving stock (Eq, Show)


instance KnownHeader XPermittedCrossDomainPolicies where
  type ParseFailure XPermittedCrossDomainPolicies = String
  type Cardinality XPermittedCrossDomainPolicies = 'ZeroOrOne
  type Direction XPermittedCrossDomainPolicies = 'Response


  parseFromHeaders _ headers = case runParser xPermittedCrossDomainPoliciesParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing X-Permitted-Cross-Domain-Policies header: " <> show rest
    Fail -> Left "Failed to parse X-Permitted-Cross-Domain-Policies header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderXPermittedCrossDomainPolicies


  headerName _ = hXPermittedCrossDomainPolicies


xPermittedCrossDomainPoliciesParser :: ParserT st String XPermittedCrossDomainPolicies
xPermittedCrossDomainPoliciesParser =
  $( switch
      [|
        case _ of
          "none" -> pure PcdpNone
          "master-only" -> pure PcdpMasterOnly
          "by-content-type" -> pure PcdpByContentType
          "by-ftp-filename" -> pure PcdpByFtpFilename
          "all" -> pure PcdpAll
        |]
   )


renderXPermittedCrossDomainPolicies :: XPermittedCrossDomainPolicies -> M.Builder
renderXPermittedCrossDomainPolicies = \case
  PcdpNone -> "none"
  PcdpMasterOnly -> "master-only"
  PcdpByContentType -> "by-content-type"
  PcdpByFtpFilename -> "by-ftp-filename"
  PcdpAll -> "all"
