{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
@X-Permitted-Cross-Domain-Policies@ carries Adobe's cross-domain
meta-policy. The response-only @none-this-response@ directive can
appear alone or alongside one domain meta-policy. The codec accepts
separate field lines, Adobe's semicolon form, and HTTP comma folding.

<https://www.adobe.com/devnet-docs/acrobatetk/tools/AppSec/xdomain.html Adobe cross-domain configuration>

See also: "Network.HTTP.Headers.CrossOriginResourcePolicy".
-}
module Network.HTTP.Headers.XPermittedCrossDomainPolicies (
  XPermittedCrossDomainPolicies (
    PcdpNone,
    PcdpMasterOnly,
    PcdpByContentType,
    PcdpByFtpFilename,
    PcdpAll,
    PcdpMetaPolicy,
    PcdpNoneThisResponse,
    PcdpWithNoneThisResponse
  ),
  CrossDomainMetaPolicy (..),
  xPermittedCrossDomainPoliciesParser,
  renderXPermittedCrossDomainPolicies,
) where

import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty (..))
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXPermittedCrossDomainPolicies)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | A domain meta-policy from Adobe's cross-domain policy format.
data CrossDomainMetaPolicy
  = MetaPolicyNone
  | MetaPolicyMasterOnly
  | MetaPolicyByContentType
  | MetaPolicyByFtpFilename
  | MetaPolicyAll
  deriving stock (Eq, Show)


data XPermittedCrossDomainPolicies
  = PcdpMetaPolicy !CrossDomainMetaPolicy
  | PcdpNoneThisResponse
  | PcdpWithNoneThisResponse !CrossDomainMetaPolicy
  deriving stock (Eq)


-- | Disallow domain policy files.
pattern PcdpNone :: XPermittedCrossDomainPolicies
pattern PcdpNone = PcdpMetaPolicy MetaPolicyNone


-- | Allow the master policy file.
pattern PcdpMasterOnly :: XPermittedCrossDomainPolicies
pattern PcdpMasterOnly = PcdpMetaPolicy MetaPolicyMasterOnly


-- | Allow files served with the cross-domain policy media type.
pattern PcdpByContentType :: XPermittedCrossDomainPolicies
pattern PcdpByContentType = PcdpMetaPolicy MetaPolicyByContentType


-- | Allow files with the FTP policy filename.
pattern PcdpByFtpFilename :: XPermittedCrossDomainPolicies
pattern PcdpByFtpFilename = PcdpMetaPolicy MetaPolicyByFtpFilename


-- | Allow domain policy files at any location.
pattern PcdpAll :: XPermittedCrossDomainPolicies
pattern PcdpAll = PcdpMetaPolicy MetaPolicyAll


{-# COMPLETE PcdpNone, PcdpMasterOnly, PcdpByContentType, PcdpByFtpFilename, PcdpAll, PcdpNoneThisResponse, PcdpWithNoneThisResponse #-}


instance Show XPermittedCrossDomainPolicies where
  showsPrec precedence = \case
    PcdpNone -> showString "PcdpNone"
    PcdpMasterOnly -> showString "PcdpMasterOnly"
    PcdpByContentType -> showString "PcdpByContentType"
    PcdpByFtpFilename -> showString "PcdpByFtpFilename"
    PcdpAll -> showString "PcdpAll"
    PcdpNoneThisResponse -> showString "PcdpNoneThisResponse"
    PcdpWithNoneThisResponse policy ->
      showParen (precedence > 10) (showString "PcdpWithNoneThisResponse " . showsPrec 11 policy)


instance KnownHeader XPermittedCrossDomainPolicies where
  type ParseFailure XPermittedCrossDomainPolicies = String
  type Cardinality XPermittedCrossDomainPolicies = 'ZeroOrOne
  type Direction XPermittedCrossDomainPolicies = 'Response


  parseFromHeaders _ (wire :| []) = parsePolicyLine wire
  parseFromHeaders _ (first :| [second]) = do
    left <- parsePolicyLine first
    right <- parsePolicyLine second
    maybe (Left conflictingPolicies) Right (combinePolicies left right)
  parseFromHeaders _ _ = Left conflictingPolicies


  renderToHeaders _ = policyBytes


  headerName _ = hXPermittedCrossDomainPolicies


parsePolicyLine :: BS.ByteString -> Either String XPermittedCrossDomainPolicies
parsePolicyLine wire =
  maybe (parseFieldLine "X-Permitted-Cross-Domain-Policies" xPermittedCrossDomainPoliciesParser wire) Right (policyWord wire)


xPermittedCrossDomainPoliciesParser :: ParserT st String XPermittedCrossDomainPolicies
xPermittedCrossDomainPoliciesParser = do
  ows
  first <- directive
  branch separator (paired first) (pure first) <* ows
  where
    separator = ows *> ($(char ';') <|> $(char ',')) *> ows
    directive = withByteString tokenBytes $ \() bytes -> maybe failed pure (policyWord bytes)
    paired first = do
      second <- directive
      maybe (err conflictingPolicies) pure (combinePolicies first second)


conflictingPolicies :: String
conflictingPolicies = "Conflicting X-Permitted-Cross-Domain-Policies directives"


policyWord :: BS.ByteString -> Maybe XPermittedCrossDomainPolicies
policyWord = \case
  "none" -> Just PcdpNone
  "master-only" -> Just PcdpMasterOnly
  "by-content-type" -> Just PcdpByContentType
  "by-ftp-filename" -> Just PcdpByFtpFilename
  "all" -> Just PcdpAll
  "none-this-response" -> Just PcdpNoneThisResponse
  _ -> Nothing


combinePolicies :: XPermittedCrossDomainPolicies -> XPermittedCrossDomainPolicies -> Maybe XPermittedCrossDomainPolicies
combinePolicies PcdpNoneThisResponse (PcdpMetaPolicy policy) = Just (PcdpWithNoneThisResponse policy)
combinePolicies (PcdpMetaPolicy policy) PcdpNoneThisResponse = Just (PcdpWithNoneThisResponse policy)
combinePolicies _ _ = Nothing


policyBytes :: XPermittedCrossDomainPolicies -> BS.ByteString
policyBytes (PcdpMetaPolicy policy) = metaPolicyBytes policy
policyBytes PcdpNoneThisResponse = "none-this-response"
policyBytes (PcdpWithNoneThisResponse policy) = metaPolicyBytes policy <> "; none-this-response"


metaPolicyBytes :: CrossDomainMetaPolicy -> BS.ByteString
metaPolicyBytes = \case
  MetaPolicyNone -> "none"
  MetaPolicyMasterOnly -> "master-only"
  MetaPolicyByContentType -> "by-content-type"
  MetaPolicyByFtpFilename -> "by-ftp-filename"
  MetaPolicyAll -> "all"


renderXPermittedCrossDomainPolicies :: XPermittedCrossDomainPolicies -> M.Builder
renderXPermittedCrossDomainPolicies (PcdpWithNoneThisResponse policy) = M.byteString (metaPolicyBytes policy) <> "; none-this-response"
renderXPermittedCrossDomainPolicies value = M.byteString (policyBytes value)
