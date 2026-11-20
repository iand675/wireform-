{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
@DNT@ (\"Do Not Track\") — a /de-facto/ (formerly W3C-tracked, never
IANA-registered) request header by which a user agent signals the user's
tracking preference. It is now obsolete (the W3C specification was retired and
most browsers have removed it) but is still emitted by some clients.

== Grammar

@
DNT = ( "0" / "1" ) *DNT-extension
@

@1@ means \"do not track me\" and @0@ means \"tracking is acceptable\".
The codec retains the optional extension suffix and exposes the preference
as a Boolean. An absent field expresses no preference.

This is a /de-facto/ header; see the (retired) W3C Tracking Preference
Expression specification, <https://www.w3.org/TR/tracking-dnt/>.

See also: "Network.HTTP.Headers.SecGPC", "Network.HTTP.Headers.ClearSiteData", "Network.HTTP.Headers.PermissionsPolicy".
-}
module Network.HTTP.Headers.DNT (
  DNT (DNT, dntDoNotTrack),
  dntWithExtension,
  dntExtension,
  dNTParser,
  renderDNT,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Unsafe as BSU
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hDNT)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | A tracking preference with an optional extension suffix.
data DNT = DNTValue !Bool !ST.ShortText
  deriving stock (Eq, Show)


{- | Construct a bare preference. Matching the pattern exposes the
preference in a bare or extended value.
-}
pattern DNT :: Bool -> DNT
pattern DNT {dntDoNotTrack} <- DNTValue dntDoNotTrack _
  where
    DNT preference = DNTValue preference ""


{-# COMPLETE DNT #-}


-- | Construct a preference with a validated extension suffix.
dntWithExtension :: Bool -> ST.ShortText -> Maybe DNT
dntWithExtension preference suffix
  | ST.all isDntExtensionChar suffix = Just (DNTValue preference suffix)
  | otherwise = Nothing


-- | The suffix following the preference digit, empty for a bare value.
dntExtension :: DNT -> ST.ShortText
dntExtension (DNTValue _ suffix) = suffix


instance KnownHeader DNT where
  type ParseFailure DNT = String
  type Cardinality DNT = 'ZeroOrOne
  type Direction DNT = 'Request


  parseFromHeaders _ (wire :| [])
    | BS.length wire == 1
    , let digit = BSU.unsafeHead wire
    , digit == 0x30 || digit == 0x31 =
        if digit == 0x31 then Right (DNT True) else Right (DNT False)
    | BS.length wire > 1
    , let digit = BSU.unsafeHead wire
    , digit == 0x30 || digit == 0x31
    , let suffix = BSU.unsafeTail wire
    , BSC.all isDntExtensionChar suffix =
        Right $! DNTValue (digit == 0x31) (STU.fromByteStringUnsafe suffix)
  parseFromHeaders _ fields = parseSingleFieldLine "DNT" dNTParser fields


  renderToHeaders _ (DNTValue True "") = "1"
  renderToHeaders _ (DNTValue False "") = "0"
  renderToHeaders _ value = M.toStrictByteString (renderDNT value)


  headerName _ = hDNT


dNTParser :: ParserT st String DNT
dNTParser = do
  preference <- branch $(char '1') (pure True) (False <$ $(char '0'))
  DNTValue preference <$> shortASCIIFromParser_ (skipMany (skipSatisfyAscii isDntExtensionChar))


isDntExtensionChar :: Char -> Bool
isDntExtensionChar c = c >= '!' && c <= '~' && c /= '"' && c /= ',' && c /= '\\'
{-# INLINE isDntExtensionChar #-}


renderDNT :: DNT -> M.Builder
renderDNT (DNTValue preference suffix) = M.char7 (if preference then '1' else '0') <> shortText suffix
