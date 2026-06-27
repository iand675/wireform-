{- |
Module      : Network.HTTP.Headers.SetProfile
Description : The obsolete @SetProfile@ HTTP header (Open Profiling Standard).

The @SetProfile@ header was defined by the Open Profiling Standard (OPS), a
1997 W3C Note describing the exchange of user profile information over HTTP. A
server emits @SetProfile@ to provide profile write operations to be stored in a
user's profile. OPS was never standardized and was superseded by P3P; its value
is an opaque, host-specific encoding.

Because the grammar is historic and effectively opaque, this module preserves
the field value verbatim as a faithful raw newtype rather than fabricating a
dead grammar.

Spec: W3C Note \"Implementation of OPS over HTTP\" (NOTE-OPS-OverHTTP, 1997-06-02)
<https://www.w3.org/TR/NOTE-OPS-OverHTTP>

See also: "Network.HTTP.Headers.GetProfile", "Network.HTTP.Headers.ProfileObject", "Network.HTTP.Headers.P3P".
-}
module Network.HTTP.Headers.SetProfile (
  SetProfile (..),
  setProfileParser,
  renderSetProfile,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSetProfile)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | @SetProfile@ header value, preserved verbatim (opaque OPS profile write).
newtype SetProfile = SetProfile {setProfileValue :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader SetProfile where
  type ParseFailure SetProfile = String
  type Cardinality SetProfile = 'ZeroOrOne
  type Direction SetProfile = 'Response


  parseFromHeaders _ headers = case runParser setProfileParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing SetProfile header: " <> show rest
    Fail -> Left "Failed to parse SetProfile header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderSetProfile


  headerName _ = hSetProfile


setProfileParser :: ParserT st String SetProfile
setProfileParser = SetProfile <$> takeRestShortText


renderSetProfile :: SetProfile -> M.Builder
renderSetProfile (SetProfile v) = shortText v
