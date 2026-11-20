{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- | @SetProfile@ is an OPS response field requesting a write to the user profile. It
carries the escaped profile-update object.
The OPS note describes the payload and leaves its wire encoding open.
The codec validates the HTTP field and preserves its octets.

Spec: <https://www.w3.org/TR/NOTE-OPS-OverHTTP>

See also: "Network.HTTP.Headers.GetProfile", "Network.HTTP.Headers.ProfileObject", "Network.HTTP.Headers.P3P".
-}
module Network.HTTP.Headers.SetProfile (
  SetProfile (SetProfile, setProfileValue),
  setProfileBytes,
  setProfileFromBytes,
  setProfileParser,
  renderSetProfile,
) where

import Data.ByteString (ByteString)
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.Parser (ParserT)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSetProfile)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.OpaqueField


newtype SetProfile = SetProfileBytes ByteString
  deriving stock (Eq)


{- | Legacy text view. Use 'setProfileBytes' for exact wire data and
'setProfileFromBytes' for checked authoring.
-}
pattern SetProfile :: ST.ShortText -> SetProfile
pattern SetProfile {setProfileValue} <- (fieldText -> setProfileValue)
  where
    SetProfile text = SetProfileBytes (ST.toByteString text)


{-# COMPLETE SetProfile #-}


instance Show SetProfile where
  showsPrec precedence (SetProfile text) =
    showParen (precedence > 10) $ showString "SetProfile {setProfileValue = " . shows text . showChar '}'


fieldText :: SetProfile -> ST.ShortText
fieldText = opaqueFieldText . setProfileBytes


-- | The original field bytes, including any received optional whitespace.
setProfileBytes :: SetProfile -> ByteString
setProfileBytes (SetProfileBytes wire) = wire


setProfileFromBytes :: ByteString -> Either String SetProfile
setProfileFromBytes = checkOpaqueField "SetProfile" SetProfileBytes


instance KnownHeader SetProfile where
  type ParseFailure SetProfile = String
  type Cardinality SetProfile = 'ZeroOrOne
  type Direction SetProfile = 'Response


  parseFromHeaders _ = parseOpaqueFieldLine "SetProfile" SetProfileBytes


  renderToHeaders _ = setProfileBytes


  headerName _ = hSetProfile


setProfileParser :: ParserT st String SetProfile
setProfileParser = opaqueFieldParser "SetProfile" SetProfileBytes


renderSetProfile :: SetProfile -> M.Builder
renderSetProfile = M.byteString . setProfileBytes
