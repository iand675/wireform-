{- | The query-language AST, mirroring the normative grammar of spec §4.8.

A parsed 'Document' is the surface form; canonicalization (§5.1) produces a
restricted 'Document' (one anonymous query, no imports, no local fragment
definitions, spreads only to schema fragments) whose rendering is the
query's identity. The canonical renderer lives in "Lattice.Canonical".
-}
module Lattice.Query.AST (
  Document (..),
  QueryDef (..),
  VarDef (..),
  TypeRefQ (..),
  SelectionSet,
  Selection (..),
  Field (..),
  Argument (..),
  QValue (..),
  FragmentDefQ (..),

  -- * Traversal helpers
  selectionFields,
  documentSpreads,
) where

import Data.Scientific (Scientific)
import Data.Text (Text)
import Lattice.Types


{- | A query document: any number of imports and fragment definitions, and
(static rule 1) exactly one query definition.
-}
data Document = Document
  { docImports :: [Text]
  , docFragments :: [FragmentDefQ]
  , docQuery :: QueryDef
  }
  deriving stock (Eq, Show)


data QueryDef = QueryDef
  { qName :: Maybe Text
  -- ^ Documentation only; erased by canonicalization.
  , qVars :: [VarDef]
  , qSelection :: SelectionSet
  }
  deriving stock (Eq, Show)


data VarDef = VarDef
  { vdName :: VarName
  , vdType :: TypeRefQ
  , vdDefault :: Maybe QValue
  }
  deriving stock (Eq, Show)


-- | A type reference in a variable declaration: @Name@ or @Name?@.
data TypeRefQ = TypeRefQ
  { trName :: Text
  , trOptional :: Bool
  }
  deriving stock (Eq, Show)


-- | A local fragment definition, @fragment Name [(params)] on Type { ... }@.
data FragmentDefQ = FragmentDefQ
  { fdName :: FragmentName
  , fdParams :: [VarDef]
  , fdOn :: Text
  -- ^ Entity type or interface name.
  , fdSelection :: SelectionSet
  }
  deriving stock (Eq, Show)


type SelectionSet = [Selection]


data Selection
  = SField Field
  | -- | @... on Type { ... }@
    SInline TypeName SelectionSet
  | -- | @...Name@ or @...Name(args)@
    SSpread FragmentName [Argument]
  deriving stock (Eq, Show)


data Field = Field
  { fName :: FieldName
  , fArgs :: [Argument]
  , fDepth :: Maybe Int
  -- ^ @\@depth(n)@; static rule 4 forbids combining with a selection set.
  , fSelection :: Maybe SelectionSet
  -- ^ @Nothing@ for scalar fields; @Just@ for edges.
  }
  deriving stock (Eq, Show)


data Argument = Argument
  { argName :: ArgName
  , argValue :: QValue
  }
  deriving stock (Eq, Show)


{- | Values in argument and default position. Integers keep their exact
'Integer'; non-integral numbers carry the parsed 'Scientific' and render
canonically (shortest form). Enum values are bare names; @true@/@false@
lex as 'QBool' by the reserved-name rule. Explicit @null@ does not exist
(§4.8 rule 6): omission is the only spelling of absence.
-}
data QValue
  = QVar VarName
  | QInt Integer
  | QNum Scientific
  | QString Text
  | QBool Bool
  | QEnum Text
  | QList [QValue]
  deriving stock (Eq, Ord, Show)


-- | All fields at the top of a selection set (not descending into subselections).
selectionFields :: SelectionSet -> [Field]
selectionFields sels = [f | SField f <- sels]


-- | Every spread name mentioned anywhere in the document (fragments and query).
documentSpreads :: Document -> [FragmentName]
documentSpreads Document {..} =
  concatMap goSel (qSelection docQuery)
    <> concatMap (concatMap goSel . fdSelection) docFragments
  where
    goSel = \case
      SField f -> maybe [] (concatMap goSel) (fSelection f)
      SInline _ ss -> concatMap goSel ss
      SSpread n _ -> [n]
