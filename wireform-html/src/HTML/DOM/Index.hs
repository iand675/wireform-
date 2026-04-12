{-# LANGUAGE BangPatterns #-}
-- | Pre-order element index for fast selector queries (internal to @HTML.DOM@).
module HTML.DOM.Index
  ( ElementIndex(..)
  , buildElementIndex
  ) where

import Control.Monad (when)
import Control.Monad.ST (runST)
import Data.Foldable (foldl')
import qualified Data.HashMap.Strict as HM
import Data.Primitive.PrimArray
  ( PrimArray
  , newPrimArray, writePrimArray, unsafeFreezePrimArray
  )
import Data.Primitive.SmallArray
  ( SmallArray
  , sizeofSmallArray, indexSmallArray
  , newSmallArray, writeSmallArray, unsafeFreezeSmallArray
  )
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Text (Text)
import qualified Data.Text as T

import qualified HTML.Selector as Sel
import HTML.Value (HTMLNode(..))

import Data.Int (Int32)

-- | Pre-order element index for O(1) structural pseudo-class evaluation
-- and fast selector dispatch.
data ElementIndex = ElementIndex
  { eiCount    :: {-# UNPACK #-} !Int
  , eiNodes    :: !(SmallArray HTMLNode)
  , eiParent   :: !(PrimArray Int32)    -- parent flat index (-1 for root)
  , eiRawChild :: !(PrimArray Int32)    -- index in parent's SmallArray HTMLNode
  , eiElemPos  :: !(PrimArray Int32)    -- 1-based position among element siblings
  , eiElemCnt  :: !(PrimArray Int32)    -- total element children of parent
  , eiPrevElem :: !(PrimArray Int32)    -- previous element sibling flat index (-1)
  , eiNextElem :: !(PrimArray Int32)    -- next element sibling flat index (-1)
  , eiSubEnd   :: !(PrimArray Int32)    -- exclusive subtree end (first index outside subtree)
  , eiByTag    :: !(HM.HashMap Text (PrimArray Int32))
  , eiByClass  :: !(HM.HashMap Text (PrimArray Int32))
  }

countAllElements :: HTMLNode -> Int
countAllElements (HTMLElement _ _ children) =
  let !n = sizeofSmallArray children
  in 1 + countElemKids children 0 n 0
countAllElements _ = 0

countElemKids :: SmallArray HTMLNode -> Int -> Int -> Int -> Int
countElemKids !children !i !n !acc
  | i >= n = acc
  | otherwise =
      let !child = indexSmallArray children i
      in countElemKids children (i + 1) n (acc + countAllElements child)

countElemChildrenOnly :: SmallArray HTMLNode -> Int -> Int -> Int -> Int
countElemChildrenOnly !children !i !n !acc
  | i >= n = acc
  | HTMLElement {} <- indexSmallArray children i =
      countElemChildrenOnly children (i + 1) n (acc + 1)
  | otherwise = countElemChildrenOnly children (i + 1) n acc

buildElementIndex :: HTMLNode -> ElementIndex
buildElementIndex root = runST $ do
  let !count = countAllElements root
  mNodes   <- newSmallArray count (error "eiNodes: uninitialized")
  mParent  <- newPrimArray count
  mRawIdx  <- newPrimArray count
  mElemPos <- newPrimArray count
  mElemCnt <- newPrimArray count
  mPrevEl  <- newPrimArray count
  mNextEl  <- newPrimArray count
  mSubEnd  <- newPrimArray count
  nextRef  <- newSTRef (0 :: Int)
  tagRef   <- newSTRef (HM.empty :: HM.HashMap Text [Int])
  clsRef   <- newSTRef (HM.empty :: HM.HashMap Text [Int])

  let visit node !parentI !rawI !ePos !eCnt = do
        myI <- readSTRef nextRef
        writeSTRef nextRef (myI + 1)
        writeSmallArray mNodes myI node
        writePrimArray mParent myI (fromIntegral parentI)
        writePrimArray mRawIdx myI (fromIntegral rawI)
        writePrimArray mElemPos myI (fromIntegral ePos)
        writePrimArray mElemCnt myI (fromIntegral eCnt)

        case node of
          HTMLElement tag attrs children -> do
            tagMap <- readSTRef tagRef
            writeSTRef tagRef $! HM.insertWith (\_ old -> myI : old) tag [myI] tagMap
            case Sel.findAttr "class" attrs of
              Just cv -> do
                let !ws = T.words cv
                clsMap <- readSTRef clsRef
                writeSTRef clsRef $! foldl' (\m w -> HM.insertWith (\_ old -> myI : old) w [myI] m) clsMap ws
              Nothing -> pure ()
            let !cn = sizeofSmallArray children
                !ec = countElemChildrenOnly children 0 cn 0
            goKids children myI 0 cn 1 ec (-1 :: Int)
            afterI <- readSTRef nextRef
            writePrimArray mSubEnd myI (fromIntegral afterI)
          _ -> writePrimArray mSubEnd myI (fromIntegral (myI + 1))

      goKids !children !parentI !i !n !ePos !eCnt !prevFlat
        | i >= n = when (prevFlat >= 0) $
            writePrimArray mNextEl prevFlat (-1)
        | otherwise =
            let !child = indexSmallArray children i
            in case child of
              HTMLElement {} -> do
                childFlat <- readSTRef nextRef
                writePrimArray mPrevEl childFlat (fromIntegral prevFlat)
                when (prevFlat >= 0) $
                  writePrimArray mNextEl prevFlat (fromIntegral childFlat)
                visit child parentI i ePos eCnt
                goKids children parentI (i + 1) n (ePos + 1) eCnt childFlat
              _ ->
                goKids children parentI (i + 1) n ePos eCnt prevFlat

  writePrimArray mPrevEl 0 (-1 :: Int32)
  writePrimArray mNextEl 0 (-1 :: Int32)
  visit root (-1) 0 (1 :: Int) (1 :: Int)

  nodes'   <- unsafeFreezeSmallArray mNodes
  parent'  <- unsafeFreezePrimArray mParent
  rawIdx'  <- unsafeFreezePrimArray mRawIdx
  elemPos' <- unsafeFreezePrimArray mElemPos
  elemCnt' <- unsafeFreezePrimArray mElemCnt
  prevEl'  <- unsafeFreezePrimArray mPrevEl
  nextEl'  <- unsafeFreezePrimArray mNextEl
  subEnd'  <- unsafeFreezePrimArray mSubEnd
  tagMap   <- readSTRef tagRef
  clsMap   <- readSTRef clsRef

  pure $! ElementIndex
    { eiCount    = count
    , eiNodes    = nodes'
    , eiParent   = parent'
    , eiRawChild = rawIdx'
    , eiElemPos  = elemPos'
    , eiElemCnt  = elemCnt'
    , eiPrevElem = prevEl'
    , eiNextElem = nextEl'
    , eiSubEnd   = subEnd'
    , eiByTag    = HM.map listToPrimArray tagMap
    , eiByClass  = HM.map listToPrimArray clsMap
    }

-- Convert a reversed list of Ints to a sorted PrimArray Int32.
listToPrimArray :: [Int] -> PrimArray Int32
listToPrimArray xs = runST $ do
  let !n = length xs
  ma <- newPrimArray n
  go ma (n - 1) xs
  unsafeFreezePrimArray ma
  where
    go _ _ [] = pure ()
    go ma !i (x : rest) = do
      writePrimArray ma i (fromIntegral x)
      go ma (i - 1) rest
