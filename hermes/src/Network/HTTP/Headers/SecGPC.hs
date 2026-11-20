{-# LANGUAGE TemplateHaskell #-}

{- |
The @Sec-GPC@ HTTP request header carries the Global Privacy Control signal.
When present with the value @1@, the user is asserting a do-not-sell /
do-not-share preference; @1@ is the only valid value, and the header is omitted
when no such preference is being expressed.

On receipt, any field line carrying @1@ asserts the preference. Use
'hasSecGPC' for the receiver's effective signal.

<https://www.w3.org/TR/gpc/#sec-gpc-header-field W3C Global Privacy Control>

See also: "Network.HTTP.Headers.DNT", "Network.HTTP.Headers.SaveData", "Network.HTTP.Headers.SecPurpose", "Network.HTTP.Headers.AcceptCH".
-}
module Network.HTTP.Headers.SecGPC (
  SecGPC (..),
  hasSecGPC,
  secGPCParser,
  renderSecGPC,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty (..))
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSecGPC)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | Presence of @Sec-GPC: 1@, an asserted Global Privacy Control signal.
data SecGPC = SecGPC
  deriving stock (Eq, Show)


instance KnownHeader SecGPC where
  type ParseFailure SecGPC = String
  type Cardinality SecGPC = 'ZeroOrOne
  type Direction SecGPC = 'Request


  parseFromHeaders _ fields
    | asserted fields = Right SecGPC
    | otherwise = Left "No Sec-GPC field line carries the value 1"


  renderToHeaders _ SecGPC = "1"


  headerName _ = hSecGPC


{- | Whether the request asserts Global Privacy Control. Apply the W3C
receive rule to the field lines in their original form.
-}
hasSecGPC :: HeaderMap -> Bool
hasSecGPC = maybe False asserted . lookupRawHeader hSecGPC


asserted :: NonEmpty BS.ByteString -> Bool
asserted (first :| rest) = assertsPreference first || any assertsPreference rest
  where
    assertsPreference bytes
      | BS.length bytes == 1 = BSU.unsafeHead bytes == 0x31
      | otherwise = case BS.uncons (BS.dropWhile isWhitespace bytes) of
          Just (0x31, suffix) -> BS.all isWhitespace suffix
          _ -> False
    isWhitespace byte = byte == 0x20 || byte == 0x09


secGPCParser :: ParserT st String SecGPC
secGPCParser = $(char '1') $> SecGPC


renderSecGPC :: SecGPC -> M.Builder
renderSecGPC SecGPC = M.char7 '1'
