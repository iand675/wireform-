{- |
Module      : Network.HTTP.Headers.ProfileObject
Description : The obsolete @ProfileObject@ HTTP header (Open Profiling Standard).

The @ProfileObject@ header was defined by the Open Profiling Standard (OPS), a
1997 W3C Note describing the exchange of user profile information over HTTP. It
carries the actual profile data being exchanged between a server and a user
agent. OPS was never standardized and was superseded by P3P; its value is an
opaque, host-specific encoding (typically an encoded profile object).

Because the grammar is historic and effectively opaque, this module preserves
the field value verbatim as a faithful raw newtype rather than fabricating a
dead grammar.

Spec: W3C Note \"Implementation of OPS over HTTP\" (NOTE-OPS-OverHTTP, 1997-06-02)
<https://www.w3.org/TR/NOTE-OPS-OverHTTP>

See also: "Network.HTTP.Headers.GetProfile", "Network.HTTP.Headers.SetProfile", "Network.HTTP.Headers.P3P".
-}
module Network.HTTP.Headers.ProfileObject (
  ProfileObject (..),
  profileObjectParser,
  renderProfileObject,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hProfileObject)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | @ProfileObject@ header value, preserved verbatim (opaque OPS profile data).
newtype ProfileObject = ProfileObject {profileObjectValue :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader ProfileObject where
  type ParseFailure ProfileObject = String
  type Cardinality ProfileObject = 'ZeroOrOne
  type Direction ProfileObject = 'RequestAndResponse


  parseFromHeaders _ headers = case runParser profileObjectParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing ProfileObject header: " <> show rest
    Fail -> Left "Failed to parse ProfileObject header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderProfileObject


  headerName _ = hProfileObject


profileObjectParser :: ParserT st String ProfileObject
profileObjectParser = ProfileObject <$> takeRestShortText


renderProfileObject :: ProfileObject -> M.Builder
renderProfileObject (ProfileObject v) = shortText v
