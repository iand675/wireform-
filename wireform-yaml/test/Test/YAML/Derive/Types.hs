{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Annotated fixture types for the YAML deriver round-trip tests.
module Test.YAML.Derive.Types (
  Profile (..),
  OptionalRecord (..),
  DefaultRecord (..),
  Wrapped (..),
  CoercedRecord (..),
  CoercedBox (..),
  OptionalCoercedBox (..),
  OptionalCoerced (..),
  Box (..),
  WrappedF (..),
  Tree (..),
  Phantom (..),
  RecordChoice (..),
  GenericRecord (..),
  defaultWrapped,
  defaultRetries,
  Tag (..),
  Color (..),
  Shape (..),
  defaultPrivate,
) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Wireform.Derive.Backend
import Wireform.Derive.Modifier
import Wireform.Derive.NameStyle


data Box a = Box {boxed :: a} deriving (Eq, Show)


newtype WrappedF f a = WrappedF (f a) deriving (Eq, Show)


data Tree a = Leaf a | Branch (Tree a) (Maybe (Tree a)) deriving (Eq, Show)


data Phantom a = Phantom deriving (Eq, Show)


data Profile = Profile
  { profileName :: !Text
  , profileAge :: !Int
  , profileEmail :: !Text
  , profilePrivate :: !Text
  }
  deriving (Eq, Show)


defaultPrivate :: Text
defaultPrivate = "<redacted>"


{-# ANN profileName (rename "name") #-}


{-# ANN profileAge (renameStyle SnakeCase) #-}


{-# ANN profileEmail (renameStyle (StripPrefix "profile" `andThen` SnakeCase)) #-}


{-# ANN profilePrivate (forBackend backendYAML skip) #-}
{-# ANN profilePrivate (forBackend backendYAML (defaults 'defaultPrivate)) #-}


newtype Tag = Tag {unTag :: Int}
  deriving (Eq, Show)


data Color = Red | Green | DarkBlue
  deriving (Eq, Show)


{-# ANN Red (rename "red") #-}


{-# ANN Green (rename "green") #-}


{-# ANN DarkBlue (renameStyle KebabCase) #-}


data Shape
  = Origin
  | Circle !Double
  | Rect !Double !Double
  deriving (Eq, Show)


{-# ANN Origin (rename "origin") #-}


{-# ANN Circle (rename "circle") #-}


{-# ANN Rect (renameStyle SnakeCase) #-}


data OptionalRecord = OptionalRecord {optionalCount :: Maybe Int}
  deriving (Eq, Show)


{-# ANN optionalCount optional #-}


data DefaultRecord = DefaultRecord {retries :: Int} deriving (Eq, Show)


defaultRetries :: Int
defaultRetries = 3


{-# ANN retries (defaults 'defaultRetries) #-}


newtype Wrapped = Wrapped Int deriving (Eq, Show)


data CoercedRecord = CoercedRecord {wrapped :: Wrapped} deriving (Eq, Show)


{-# ANN wrapped (coerced ''Int) #-}


data OptionalCoerced = OptionalCoerced {optionalWrapped :: Maybe Wrapped} deriving (Eq, Show)


defaultWrapped :: Maybe Wrapped
defaultWrapped = Just (Wrapped 5)


{-# ANN optionalWrapped optional #-}
{-# ANN optionalWrapped (coerced ''Int) #-}
{-# ANN optionalWrapped (defaults 'defaultWrapped) #-}


data RecordChoice
  = NoRecord
  | NamedRecord
      { recordName :: Text
      , recordRetries :: Int
      , recordPrivate :: Text
      }
  deriving (Eq, Show)


{-# ANN recordName (rename "name") #-}


{-# ANN recordRetries (defaults 'defaultRetries) #-}


{-# ANN recordPrivate (forBackend backendYAML skip) #-}
{-# ANN recordPrivate (forBackend backendYAML (defaults 'defaultPrivate)) #-}


data GenericRecord = GenericRecord {genericCount :: Int} deriving (Eq, Show, Generic)


data CoercedBox a = CoercedBox {coercedBox :: a} deriving (Eq, Show)


{-# ANN coercedBox (coerced ''Int) #-}


data OptionalCoercedBox a = OptionalCoercedBox {optionalCoercedBox :: Maybe a} deriving (Eq, Show)


{-# ANN optionalCoercedBox optional #-}
{-# ANN optionalCoercedBox (coerced ''Int) #-}
