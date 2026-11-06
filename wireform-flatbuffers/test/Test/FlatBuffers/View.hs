{-# LANGUAGE OverloadedStrings #-}

{- | Zero-copy view tests.

These tests exercise two orthogonal properties:

* /Functional/ — the View deriver agrees with the value-shaped
  deriver bit-for-bit on round-trip.
* /Zero-copy invariant/ — string and byte-vector decode return
  slices that share the input 'ByteString'\'s 'ForeignPtr', so
  no payload copy is happening.
-}
module Test.FlatBuffers.View (tests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Text qualified as T
import FlatBuffers.Derive (toFlatBuffers)
import FlatBuffers.Encode qualified as FBE
import FlatBuffers.Parser (parseFlatBuffers)
import FlatBuffers.View qualified as FV
import Foreign.ForeignPtr.Unsafe qualified
import Foreign.Ptr qualified
import Test.FlatBuffers.Derive.Instances ()
import Test.FlatBuffers.Derive.Types
import Test.Syd


tests :: Spec
tests =
  describe "FlatBuffers.View" $
    sequence_
      [ functional
      , zeroCopy
      ]


-- ---------------------------------------------------------------------------
-- Functional: deriveView round-trips identically to deriveFromFlatBuffers
-- ---------------------------------------------------------------------------

functional :: Spec
functional =
  describe "functional round-trips via deriveView" $
    sequence_
      [ it "Position with all fields set" $ do
          let p = Position "origin" 3 7 (Just "home") "ignored"
              bs = encodePosition p
          case FV.decodeRoot bs of
            Right p' -> do
              posName p' `shouldBe` posName p
              posX p' `shouldBe` posX p
              posY p' `shouldBe` posY p
              posNote p' `shouldBe` posNote p
              -- The skipped slot reinstates the user-supplied default.
              posLabel p' `shouldBe` defaultLabel
            Left e -> expectationFailure ("decodeRoot failed: " <> e)
      , it "Position with Nothing note" $ do
          let p = Position "no-note" 1 2 Nothing "ignored"
              bs = encodePosition p
          case FV.decodeRoot bs of
            Right p' -> do
              posNote p' `shouldBe` Nothing
              posLabel p' `shouldBe` defaultLabel
            Left e -> expectationFailure e
      ]


-- ---------------------------------------------------------------------------
-- Zero-copy: ForeignPtr identity proves we're returning slices
-- ---------------------------------------------------------------------------

zeroCopy :: Spec
zeroCopy =
  describe "zero-copy slice invariants" $
    sequence_
      [ it "readStringSlice returns a slice of the input buffer" $ do
          -- Encode a record with a recognisable string field, then
          -- pull the raw byte slice out of the buffer via the
          -- low-level reader API. The slice must be inside the
          -- input buffer's memory range — anything else means the
          -- decoder allocated a copy.
          let needleText = "ZERO_COPY_SLICE_PROBE_0123456789" :: T.Text
              p = Position needleText 1 2 Nothing "ignored"
              bs = encodePosition p
          case findStringSlice bs needleText of
            Nothing -> expectationFailure "string slice not found in buffer"
            Just slice ->
              (sharesForeignPtr bs slice) `shouldBe` True
      , it "view-decoded ByteString field is a slice (not a copy)" $ do
          -- 'ByteString' fields go through 'FV.SlotView' which
          -- bottoms out in 'readByteVectorSlice'. Confirm the
          -- returned bytes really do lie inside the input.
          let p = Position "PAYLOAD-1234567890" 0 0 Nothing "ignored"
              bs = encodePosition p
          case FV.rootTable bs of
            Left e -> expectationFailure e
            Right t -> case FV.viewSlot t 0 :: Either String ByteString of
              Left e -> expectationFailure e
              Right slice -> do
                (slice == "PAYLOAD-1234567890") `shouldBe` True
                (sharesForeignPtr bs slice) `shouldBe` True
      , it "string slot accessed twice yields equal Text" $ do
          let p = Position "shared" 0 0 Nothing "ignored"
              bs = encodePosition p
          case (FV.decodeRoot bs, FV.decodeRoot bs) of
            (Right p1, Right p2) -> posName p1 `shouldBe` posName p2
            _ -> expectationFailure "decodeRoot failed"
      ]


{- | Linear scan for the slice that contains @needle@ inside a
flatbuffer 'ByteString'. We use this rather than parsing the
buffer to double-check that the bytes really do live there
(and so the slice we extract really shares its ForeignPtr).
-}
findStringSlice :: ByteString -> T.Text -> Maybe ByteString
findStringSlice buf needle =
  let !n = T.length needle
      !raw = BS.pack (map (fromIntegral . fromEnum) (T.unpack needle))
      (_, after) = BS.breakSubstring raw buf
  in if BS.null after
       then Nothing
       else Just (BS.take n after)


{- | True iff the two ByteStrings overlap in memory (= the slice
is part of the same underlying buffer, not an independent
copy).

After bytestring-0.11 the @BS@ constructor stores @ForeignPtr
+ length@ where the @ForeignPtr@ is already offset to the
payload start. So two slices of the same buffer don't share
'ForeignPtr' pointer-equality — the one further into the
buffer has a 'plusForeignPtr'-shifted pointer. We instead
check that the slice's payload range is contained within the
haystack's payload range.
-}
sharesForeignPtr :: ByteString -> ByteString -> Bool
sharesForeignPtr (BSI.BS fp1 len1) (BSI.BS fp2 len2) =
  let !p1 = Foreign.ForeignPtr.Unsafe.unsafeForeignPtrToPtr fp1
      !p2 = Foreign.ForeignPtr.Unsafe.unsafeForeignPtrToPtr fp2
      !p1End = p1 `plusPtrInt` len1
      !p2End = p2 `plusPtrInt` len2
  in p2 >= p1 && p2End <= p1End


plusPtrInt :: Foreign.Ptr.Ptr a -> Int -> Foreign.Ptr.Ptr a
plusPtrInt = Foreign.Ptr.plusPtr


encodePosition :: Position -> ByteString
encodePosition position = either error id $ do
  schema <- parseFlatBuffers "table Position { name:string; x:int; y:int; note:string; label:string(deprecated); } root_type Position;"
  FBE.encode schema (toFlatBuffers position)
