{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- | @GetProfile@ is an OPS response field requesting profile information. It carries
the escaped profile-read object.
The OPS note describes the payload and leaves its wire encoding open.
The codec validates the HTTP field and preserves its octets.

Spec: <https://www.w3.org/TR/NOTE-OPS-OverHTTP>

See also: "Network.HTTP.Headers.SetProfile", "Network.HTTP.Headers.ProfileObject", "Network.HTTP.Headers.P3P".
-}
module Network.HTTP.Headers.GetProfile (
  GetProfile (GetProfile, getProfileValue),
  getProfileBytes,
  getProfileFromBytes,
  getProfileParser,
  renderGetProfile,
) where

import Data.ByteString (ByteString)
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.Parser (ParserT)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hGetProfile)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.OpaqueField


newtype GetProfile = GetProfileBytes ByteString
  deriving stock (Eq)


{- | Legacy text view. Use 'getProfileBytes' for exact wire data and
'getProfileFromBytes' for checked authoring.
-}
pattern GetProfile :: ST.ShortText -> GetProfile
pattern GetProfile {getProfileValue} <- (fieldText -> getProfileValue)
  where
    GetProfile text = GetProfileBytes (ST.toByteString text)


{-# COMPLETE GetProfile #-}


instance Show GetProfile where
  showsPrec precedence (GetProfile text) =
    showParen (precedence > 10) $ showString "GetProfile {getProfileValue = " . shows text . showChar '}'


fieldText :: GetProfile -> ST.ShortText
fieldText = opaqueFieldText . getProfileBytes


-- | The original field bytes, including any received optional whitespace.
getProfileBytes :: GetProfile -> ByteString
getProfileBytes (GetProfileBytes wire) = wire


getProfileFromBytes :: ByteString -> Either String GetProfile
getProfileFromBytes = checkOpaqueField "GetProfile" GetProfileBytes


instance KnownHeader GetProfile where
  type ParseFailure GetProfile = String
  type Cardinality GetProfile = 'ZeroOrOne
  type Direction GetProfile = 'Response


  parseFromHeaders _ = parseOpaqueFieldLine "GetProfile" GetProfileBytes


  renderToHeaders _ = getProfileBytes


  renderToHeadersChecked _ = renderOpaqueFieldChecked . getProfileBytes


  headerName _ = hGetProfile


getProfileParser :: ParserT st String GetProfile
getProfileParser = opaqueFieldParser "GetProfile" GetProfileBytes


renderGetProfile :: GetProfile -> M.Builder
renderGetProfile = M.byteString . getProfileBytes
