{-# LANGUAGE StrictData #-}
module HTML.Rewriter.StackFrame (
  StackFrame (..),
) where

import Data.Primitive.SmallArray (SmallArray)
import Data.Text (Text)
import HTML.Value (HTMLAttribute (..))

data StackFrame = StackFrame
  { sfTag :: !Text
  , sfAttrs :: !(SmallArray HTMLAttribute)
  , sfDepth :: {-# UNPACK #-} !Int
  , sfTextMatch :: !Bool
  }

