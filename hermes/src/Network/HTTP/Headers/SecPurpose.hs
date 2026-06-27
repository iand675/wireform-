{- |
The @Sec-Purpose@ Fetch Metadata request header states the purpose for which a
request was issued. The notable value is @prefetch@, set by user agents
performing speculative prefetching (and @prerender@ for speculative rendering),
letting the origin decide how to treat such background requests. Its value is
an RFC 8941 Structured Field token, preserved here verbatim so that both
currently-defined and future purpose tokens round-trip faithfully.

Spec: <https://wicg.github.io/nav-speculation/prefetch.html#sec-purpose-header>
(WICG Speculation Rules; the token syntax is RFC 8941).

See also: "Network.HTTP.Headers.AcceptCH", "Network.HTTP.Headers.SaveData", "Network.HTTP.Headers.SecGPC", "Network.HTTP.Headers.EarlyData", "Network.HTTP.Headers.Priority".
-}
module Network.HTTP.Headers.SecPurpose (
  SecPurpose (..),
  secPurposeParser,
  renderSecPurpose,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSecPurpose)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- | The purpose token carried by @Sec-Purpose@ (for example @prefetch@).
newtype SecPurpose = SecPurpose {secPurposeValue :: RFC8941Token}
  deriving stock (Eq, Show)


instance KnownHeader SecPurpose where
  type ParseFailure SecPurpose = String
  type Cardinality SecPurpose = 'ZeroOrOne
  type Direction SecPurpose = 'Request


  parseFromHeaders _ headers = case runParser secPurposeParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left ("Unconsumed input after parsing Sec-Purpose header: " <> show rest)
    Fail -> Left "Failed to parse Sec-Purpose header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderSecPurpose


  headerName _ = hSecPurpose


secPurposeParser :: ParserT st String SecPurpose
secPurposeParser = SecPurpose <$> rfc8941Token


renderSecPurpose :: SecPurpose -> M.Builder
renderSecPurpose (SecPurpose t) = R.rfc8941Token t
