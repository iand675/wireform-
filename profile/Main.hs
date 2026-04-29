{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
module Main where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.Int (Int32, Int64)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import System.Environment (getArgs)

import qualified Proto.Encode as H
import qualified Proto.Decode as H
import Proto.Wire (Tag(..), WireType(..))
import Proto.Wire.Decode
import GHC.Exts (Int#, Int(I#))

data HSmall = HSmall
  { hsId     :: {-# UNPACK #-} !Int64
  , hsName   :: !Text
  , hsActive :: !Bool
  }

instance H.MessageEncode HSmall where
  buildMessage (HSmall i n a) =
    (if i == 0 then mempty else H.encodeFieldVarint 1 (fromIntegral i)) <>
    (if n == "" then mempty else H.encodeFieldString 2 n) <>
    (if not a then mempty else H.encodeFieldBool 3 a)
  {-# INLINE buildMessage #-}

instance H.MessageDecode HSmall where
  messageDecoder = Decoder (\bs off -> loop 0 "" False bs off)
    where
      loop :: Int64 -> Text -> Bool -> BS.ByteString -> Int# -> (# (# HSmall, Int# #) | DecodeError #)
      loop !i !n !a !bs !off =
        withTag bs off
          (\off' -> (# (# HSmall i n a, off' #) | #))
          (\fn _wt off' -> case I# fn of
            1 -> case runDecoder# getVarint bs off' of
              (# (# v, off'' #) | #) -> loop (fromIntegral v) n a bs off''
              (# | e #) -> (# | e #)
            2 -> case runDecoder# getText bs off' of
              (# (# v, off'' #) | #) -> loop i v a bs off''
              (# | e #) -> (# | e #)
            3 -> case runDecoder# getVarint bs off' of
              (# (# v, off'' #) | #) -> loop i n (v /= 0) bs off''
              (# | e #) -> (# | e #)
            _ -> case runDecoder# (skipField (toEnum (I# _wt))) bs off' of
              (# (# _, off'' #) | #) -> loop i n a bs off''
              (# | e #) -> (# | e #)
          )
          (\e -> (# | e #))
  {-# INLINE messageDecoder #-}

data HWithRepeated = HWithRepeated
  { hwrValues :: !(V.Vector Int32)
  , hwrTags   :: !(V.Vector Text)
  , hwrItems  :: !(V.Vector HSmall)
  }

instance H.MessageEncode HWithRepeated where
  buildMessage m =
    (let vs = hwrValues m in if V.null vs then mempty
       else H.encodePackedWord64 1 (VU.convert (V.map fromIntegral vs))) <>
    V.foldl' (\acc s -> acc <> H.encodeFieldString 2 s) mempty (hwrTags m) <>
    V.foldl' (\acc item -> acc <> H.encodeFieldMessage 3 item) mempty (hwrItems m)
  {-# INLINE buildMessage #-}

instance H.MessageDecode HWithRepeated where
  messageDecoder = loop [] [] []
    where
      loop !vals !tags !items = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure (HWithRepeated (V.fromList (reverse vals)) (V.fromList (reverse tags)) (V.fromList (reverse items)))
          UJust (Tag fn wt) -> case fn of
            1 -> case wt of
              WireLengthDelimited -> do
                bs <- getLengthDelimited
                let !parsed = decodePacked bs
                loop (reversePrepend parsed vals) tags items
              _ -> getVarint >>= \v -> loop (fromIntegral v : vals) tags items
            2 -> H.decodeFieldString >>= \v -> loop vals (v : tags) items
            3 -> H.decodeFieldMessage >>= \v -> loop vals tags (v : items)
            _ -> skipField wt >> loop vals tags items
  {-# INLINE messageDecoder #-}

reversePrepend :: [a] -> [a] -> [a]
reversePrepend [] ys = ys
reversePrepend (x:xs) ys = reversePrepend xs (x : ys)

decodePacked :: BS.ByteString -> [Int32]
decodePacked bs = go [] 0
  where
    len = BS.length bs
    go !acc !off
      | off >= len = acc
      | otherwise = case runDecoder' getVarint bs off of
          DecodeOK v off' -> go (fromIntegral v : acc) off'
          DecodeFail _    -> acc

smallHS :: HSmall
smallHS = HSmall 42 "hello world" True

repeatedHS :: HWithRepeated
repeatedHS = HWithRepeated
  (V.fromList [1..50])
  (V.fromList (fmap (\i -> "tag_" <> T.pack (show i)) [1..20 :: Int]))
  (V.fromList [ HSmall (fromIntegral i) ("item" <> T.pack (show i)) (even i) | i <- [1..10 :: Int] ])

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["encode-small", ns] -> do
      ref <- newIORef (0 :: Int)
      forM_ [1..read ns :: Int] $ \_ -> do
        let !bs = H.encodeMessage smallHS
        modifyIORef' ref (+ BS.length bs)
      readIORef ref >>= \n -> putStrLn ("Total bytes: " <> show n)

    ["decode-small", ns] -> do
      let !bs = H.encodeMessage smallHS
      ref <- newIORef (0 :: Int)
      forM_ [1..read ns :: Int] $ \_ -> case H.decodeMessage bs of
        Right (m :: HSmall) -> modifyIORef' ref (+ fromIntegral (hsId m))
        Left _ -> pure ()
      readIORef ref >>= \n -> putStrLn ("Sum: " <> show n)

    ["encode-repeated", ns] -> do
      ref <- newIORef (0 :: Int)
      forM_ [1..read ns :: Int] $ \i -> do
        let msg = repeatedHS { hwrValues = V.fromList (fmap fromIntegral [i..i+49]) }
            !bs = H.encodeMessage msg
        modifyIORef' ref (+ BS.length bs)
      readIORef ref >>= \n -> putStrLn ("Total bytes: " <> show n)

    ["decode-repeated", ns] -> do
      let !bs = H.encodeMessage repeatedHS
      ref <- newIORef (0 :: Int)
      forM_ [1..read ns :: Int] $ \_ -> case H.decodeMessage bs of
        Right (m :: HWithRepeated) -> modifyIORef' ref (+ V.length (hwrValues m))
        Left _ -> pure ()
      readIORef ref >>= \n -> putStrLn ("Sum: " <> show n)

    _ -> putStrLn "Usage: profile <encode-small|decode-small|encode-repeated|decode-repeated> <iterations>"
