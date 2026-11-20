{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- | @ProfileObject@ carries the profile data that an OPS agent returns in an HTTP
message. The note illustrates it in a request following GetProfile.
The OPS note describes the payload and leaves its wire encoding open.
The codec validates the HTTP field and preserves its octets.

Spec: <https://www.w3.org/TR/NOTE-OPS-OverHTTP>

See also: "Network.HTTP.Headers.GetProfile", "Network.HTTP.Headers.SetProfile", "Network.HTTP.Headers.P3P".
-}
module Network.HTTP.Headers.ProfileObject (
  ProfileObject (ProfileObject, profileObjectValue),
  profileObjectBytes,
  profileObjectFromBytes,
  profileObjectParser,
  renderProfileObject,
) where

import Data.ByteString (ByteString)
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.Parser (ParserT)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hProfileObject)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.OpaqueField


newtype ProfileObject = ProfileObjectBytes ByteString
  deriving stock (Eq)


{- | Legacy text view. Use 'profileObjectBytes' for exact wire data and
'profileObjectFromBytes' for checked authoring.
-}
pattern ProfileObject :: ST.ShortText -> ProfileObject
pattern ProfileObject {profileObjectValue} <- (fieldText -> profileObjectValue)
  where
    ProfileObject text = ProfileObjectBytes (ST.toByteString text)


{-# COMPLETE ProfileObject #-}


instance Show ProfileObject where
  showsPrec precedence (ProfileObject text) =
    showParen (precedence > 10) $ showString "ProfileObject {profileObjectValue = " . shows text . showChar '}'


fieldText :: ProfileObject -> ST.ShortText
fieldText = opaqueFieldText . profileObjectBytes


-- | The original field bytes, including any received optional whitespace.
profileObjectBytes :: ProfileObject -> ByteString
profileObjectBytes (ProfileObjectBytes wire) = wire


profileObjectFromBytes :: ByteString -> Either String ProfileObject
profileObjectFromBytes = checkOpaqueField "ProfileObject" ProfileObjectBytes


instance KnownHeader ProfileObject where
  type ParseFailure ProfileObject = String
  type Cardinality ProfileObject = 'ZeroOrOne
  type Direction ProfileObject = 'RequestAndResponse


  parseFromHeaders _ = parseOpaqueFieldLine "ProfileObject" ProfileObjectBytes


  renderToHeaders _ = profileObjectBytes


  headerName _ = hProfileObject


profileObjectParser :: ParserT st String ProfileObject
profileObjectParser = opaqueFieldParser "ProfileObject" ProfileObjectBytes


renderProfileObject :: ProfileObject -> M.Builder
renderProfileObject = M.byteString . profileObjectBytes
