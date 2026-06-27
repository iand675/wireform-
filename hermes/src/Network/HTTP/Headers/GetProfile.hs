{- |
Module      : Network.HTTP.Headers.GetProfile
Description : The obsolete @GetProfile@ HTTP header (Open Profiling Standard).

The @GetProfile@ header was defined by the Open Profiling Standard (OPS), a
1997 W3C Note describing the exchange of user profile information over HTTP. A
server emits @GetProfile@ to request profile elements (with credentials) from a
user agent. OPS was never standardized and was superseded by P3P; its value is
an opaque, host-specific encoding.

Because the grammar is historic and effectively opaque, this module preserves
the field value verbatim as a faithful raw newtype rather than fabricating a
dead grammar.

Spec: W3C Note \"Implementation of OPS over HTTP\" (NOTE-OPS-OverHTTP, 1997-06-02)
<https://www.w3.org/TR/NOTE-OPS-OverHTTP>

See also: "Network.HTTP.Headers.SetProfile", "Network.HTTP.Headers.ProfileObject", "Network.HTTP.Headers.P3P".
-}
module Network.HTTP.Headers.GetProfile (
  GetProfile (..),
  getProfileParser,
  renderGetProfile,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hGetProfile)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | @GetProfile@ header value, preserved verbatim (opaque OPS profile request).
newtype GetProfile = GetProfile {getProfileValue :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader GetProfile where
  type ParseFailure GetProfile = String
  type Cardinality GetProfile = 'ZeroOrOne
  type Direction GetProfile = 'Response


  parseFromHeaders _ headers = case runParser getProfileParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing GetProfile header: " <> show rest
    Fail -> Left "Failed to parse GetProfile header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderGetProfile


  headerName _ = hGetProfile


getProfileParser :: ParserT st String GetProfile
getProfileParser = GetProfile <$> takeRestShortText


renderGetProfile :: GetProfile -> M.Builder
renderGetProfile (GetProfile v) = shortText v
