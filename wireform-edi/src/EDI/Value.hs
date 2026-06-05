-- | Dynamic EDI interchange representation.
--
-- EDI documents are delimiter-sensitive streams of ordered segments. The
-- 'Syntax' value records the delimiters used by a concrete interchange, and
-- the segment tree preserves positional elements without pretending EDI has
-- named fields on the wire.
module EDI.Value
  ( Syntax(..)
  , defaultSyntax
  , validateSyntax
  , Element(..)
  , Segment(..)
  , Interchange(..)
  , segment
  , interchange
  ) where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)

-- | Delimiters used by an EDI interchange.
data Syntax = Syntax
  { elementSeparator :: !Char
  , componentSeparator :: !Char
  , repetitionSeparator :: !(Maybe Char)
  , segmentTerminator :: !Char
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

-- | Common X12 delimiters: @*@ elements, @:@ components, @~@ segments.
defaultSyntax :: Syntax
defaultSyntax = Syntax
  { elementSeparator = '*'
  , componentSeparator = ':'
  , repetitionSeparator = Nothing
  , segmentTerminator = '~'
  }

-- | Check that delimiter roles are distinct.
validateSyntax :: Syntax -> Either String ()
validateSyntax syn
  | elementSeparator syn == componentSeparator syn =
      Left "EDI.Value: element and component separators must differ"
  | elementSeparator syn == segmentTerminator syn =
      Left "EDI.Value: element separator and segment terminator must differ"
  | componentSeparator syn == segmentTerminator syn =
      Left "EDI.Value: component separator and segment terminator must differ"
  | Just r <- repetitionSeparator syn
  , r == elementSeparator syn || r == componentSeparator syn || r == segmentTerminator syn =
      Left "EDI.Value: repetition separator must differ from other separators"
  | otherwise = Right ()

-- | A positional EDI element. Composite elements are split on the component
-- separator; simple elements retain their raw text.
data Element
  = Simple !Text
  | Composite !(Vector Text)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

-- | A segment tag plus its positional elements.
data Segment = Segment
  { segmentTag :: !Text
  , segmentElements :: !(Vector Element)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

-- | A full interchange with its delimiter syntax.
data Interchange = Interchange
  { interchangeSyntax :: !Syntax
  , interchangeSegments :: !(Vector Segment)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

-- | Convenience constructor for a segment from a list of elements.
segment :: Text -> [Element] -> Segment
segment tag elems = Segment tag (V.fromList elems)

-- | Convenience constructor for an interchange from a list of segments.
interchange :: Syntax -> [Segment] -> Interchange
interchange syn segments = Interchange syn (V.fromList segments)
