{-# LANGUAGE MagicHash #-}

{- | HTTP field framing for historically specified fields whose payload
encoding is application-defined. Validated values retain their wire octets.
-}
module Network.HTTP.Headers.OpaqueField (
  checkOpaqueField,
  validOpaqueField,
  renderOpaqueFieldChecked,
  parseOpaqueFieldLine,
  opaqueFieldParser,
  opaqueFieldText,
) where

import Data.Bits (complement, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Short as SBS
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text.Short (ShortText)
import Foreign.ForeignPtr (withForeignPtr)
import GHC.Exts (Int (I#), Word (W#), eqAddr#, indexWord8OffAddr#, indexWord8OffAddrAsWord64#, isTrue#, minusAddr#, plusAddr#, word8ToWord#, (<=#))
import GHC.ForeignPtr (ForeignPtr (ForeignPtr))
import GHC.Word (Word64 (W64#))
import Network.HTTP.Grammar.Parser (ParserT, err, isOpaqueByte, takeRest)
import Network.HTTP.Grammar.Rendering (opaqueShortText)


checkOpaqueField :: String -> (ByteString -> a) -> ByteString -> Either String a
checkOpaqueField name wrap wire
  | validOpaqueField wire = Right (wrap wire)
  | otherwise = Left ("Invalid HTTP field octet in " <> name)
{-# INLINE checkOpaqueField #-}


-- | Return the original bytes after validating HTTP field framing.
renderOpaqueFieldChecked :: ByteString -> Maybe ByteString
renderOpaqueFieldChecked wire
  | validOpaqueField wire = Just wire
  | otherwise = Nothing
{-# INLINE renderOpaqueFieldChecked #-}


validOpaqueField :: ByteString -> Bool
validOpaqueField (BI.BS fp@(ForeignPtr start _) (I# size)) =
  BI.accursedUnutterablePerformIO $ withForeignPtr fp $ \_ ->
    let end = plusAddr# start size
        scan cur
          | isTrue# (8# <=# minusAddr# end cur) =
              opaqueWord (W64# (indexWord8OffAddrAsWord64# cur 0#)) && scan (plusAddr# cur 8#)
          | isTrue# (eqAddr# cur end) = True
          | otherwise =
              isOpaqueByte (W# (word8ToWord# (indexWord8OffAddr# cur 0#))) && scan (plusAddr# cur 1#)
    in pure $! scan start
{-# INLINE validOpaqueField #-}


-- A set high bit marks an accepted byte. Masking into seven-bit lanes
-- keeps the additions and subtraction from carrying into adjacent lanes.
opaqueWord :: Word64 -> Bool
opaqueWord bytes =
  (bytes .|. printable .|. tabs) .&. complement deletes .&. high == high
  where
    high = 0x8080_8080_8080_8080
    ascii = 0x7f7f_7f7f_7f7f_7f7f
    low = bytes .&. ascii
    printable = low + 0x6060_6060_6060_6060
    tabs = complement ((low `xor` 0x0909_0909_0909_0909) + ascii)
    deletes = complement (0xfefe_fefe_fefe_fefe - low) .&. complement bytes
{-# INLINE opaqueWord #-}


parseOpaqueFieldLine :: String -> (ByteString -> a) -> NonEmpty ByteString -> Either String a
parseOpaqueFieldLine name wrap (value :| []) = checkOpaqueField name wrap value
parseOpaqueFieldLine name _ _ = Left ("Multiple " <> name <> " field lines")
{-# INLINE parseOpaqueFieldLine #-}


opaqueFieldParser :: String -> (ByteString -> a) -> ParserT st String a
opaqueFieldParser name wrap = takeRest >>= either err pure . checkOpaqueField name wrap
{-# INLINE opaqueFieldParser #-}


opaqueFieldText :: ByteString -> ShortText
opaqueFieldText = opaqueShortText . SBS.toShort
