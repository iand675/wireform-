{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 7486 §6.1.1 @Hobareg@ — a response field used by HTTP Origin-Bound
Authentication (HOBA) to report the state of an account registration.

@
Hobareg = \"regok\" / \"reginwork\"
@

The tokens are case-insensitive. A present invalid or repeated field is
interpreted as @reginwork@. Registration succeeds with @regok@ on a 2xx
response. Use 'hobaregRegistrationSucceeded' to interpret the response
status together with both field sections.

Spec: <https://www.rfc-editor.org/rfc/rfc7486.html#section-6.1.1 RFC 7486 §6.1.1>

See also: "Network.HTTP.Headers.Authorization", "Network.HTTP.Headers.WWWAuthenticate", "Network.HTTP.Headers.AuthenticationInfo".
-}
module Network.HTTP.Headers.Hobareg (
  Hobareg (..),
  hobaregRegistrationSucceeded,
  hobaregParser,
  renderHobareg,
) where

import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import Data.List.NonEmpty (NonEmpty (..))
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import GHC.Exts (Int (..), indexWord8OffAddr#, indexWord8OffAddrAsWord32#)
import GHC.ForeignPtr (unsafeWithForeignPtr)
import GHC.Ptr (Ptr (..))
import GHC.Word (Word32 (..), Word8 (..))
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hHobareg)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Status (StatusCode, statusIsSuccessful)


-- | @Hobareg@ registration-status value (RFC 7486 §6.1).
data Hobareg
  = -- | @regok@ — the registration has completed on a 2xx response.
    HobaregRegister
  | -- | @reginwork@ — the registration is still in progress.
    HobaregInProgress
  deriving stock (Eq, Show)


instance KnownHeader Hobareg where
  type ParseFailure Hobareg = String
  type Cardinality Hobareg = 'ZeroOrOne
  type Direction Hobareg = 'Response


  parseFromHeaders _ (wire :| [])
    | isRegok wire = Right HobaregRegister
  parseFromHeaders _ _ = Right HobaregInProgress
  {-# INLINE parseFromHeaders #-}


  renderToHeaders _ = hobaregBytes
  {-# INLINE renderToHeaders #-}


  headerName _ = hHobareg


{- | Registration succeeds only on a 2xx response with one valid @regok@
across its header and trailer sections. A present value in each section
counts as a repeated field.
-}
hobaregRegistrationSucceeded :: StatusCode -> HeaderMap -> HeaderMap -> Bool
hobaregRegistrationSucceeded status headers trailers =
  statusIsSuccessful status
    && case (lookupRawHeader hHobareg headers, lookupRawHeader hHobareg trailers) of
      (Just (wire :| []), Nothing) -> isRegok wire
      (Nothing, Just (wire :| [])) -> isRegok wire
      _ -> False


-- Only a complete, case-insensitive regok can change the registration status.
-- Bound the word and byte loads after skipping any field whitespace.
isRegok :: ByteString -> Bool
isRegok (BSI.BS input size)
  | size < 5 = False
  | otherwise = BSI.accursedUnutterablePerformIO $ unsafeWithForeignPtr input $ \(Ptr ptr) ->
      let at (I# offset) = W8# (indexWord8OffAddr# ptr offset)
          skipOWS offset
            | offset < size
            , let byte = at offset
            , byte == 32 || byte == 9 =
                skipOWS (offset + 1)
            | otherwise = offset
          matches start@(I# offset) =
            W32# (indexWord8OffAddrAsWord32# ptr offset) .|. 0x20202020 == rego
              && at (start + 4) .|. 0x20 == 107
      in pure $!
           if size == 5
             then matches 0
             else
               let start = skipOWS 0
               in size - start >= 5 && matches start && skipOWS (start + 5) == size
  where
    rego = case targetByteOrder of
      BigEndian -> 0x7265676f
      LittleEndian -> 0x6f676572
{-# INLINE isRegok #-}


hobaregParser :: ParserT st String Hobareg
hobaregParser =
  $( switchAsciiCI
       [|
         case _ of
           "reginwork" -> pure HobaregInProgress
           "regok" -> pure HobaregRegister
         |]
   )


hobaregBytes :: Hobareg -> ByteString
hobaregBytes = \case
  HobaregRegister -> "regok"
  HobaregInProgress -> "reginwork"
{-# INLINE hobaregBytes #-}


renderHobareg :: Hobareg -> M.Builder
renderHobareg = M.byteString . hobaregBytes
