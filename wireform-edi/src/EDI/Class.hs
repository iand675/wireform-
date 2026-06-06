{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Typeclass-based EDI serialization.
module EDI.Class
  ( ToEDI(..)
  , FromEDI(..)
  , ToEDIField(..)
  , FromEDIField(..)
  , encodeEDI
  , encodeEDIWithSyntax
  , decodeEDI
  , decodeEDIWithSyntax
  , fieldText
  , singletonInterchange
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy.Builder.Int as TBI
import qualified Data.Text.Read as TR
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8, Word16, Word32, Word64)
import Numeric.Natural (Natural)

import qualified EDI.Decode as Decode
import qualified EDI.Encode as Encode
import EDI.Value

class ToEDI a where
  toEDI :: a -> Interchange

class FromEDI a where
  fromEDI :: Interchange -> Either String a

-- | Scalar field conversion for positional segment elements.
class ToEDIField a where
  toEDIElement :: a -> Element

class FromEDIField a where
  fromEDIElement :: Element -> Either String a

encodeEDI :: ToEDI a => a -> Text
encodeEDI = Encode.encode . toEDI

encodeEDIWithSyntax :: ToEDI a => Syntax -> a -> Text
encodeEDIWithSyntax syn =
  Encode.encode . withSyntax syn . toEDI

decodeEDI :: FromEDI a => Text -> Either String a
decodeEDI input = Decode.decode input >>= fromEDI

decodeEDIWithSyntax :: FromEDI a => Syntax -> Text -> Either String a
decodeEDIWithSyntax syn input = Decode.decodeWithSyntax syn input >>= fromEDI

singletonInterchange :: Segment -> Interchange
singletonInterchange seg = Interchange defaultSyntax (V.singleton seg)

withSyntax :: Syntax -> Interchange -> Interchange
withSyntax syn doc = doc { interchangeSyntax = syn }

instance ToEDI Interchange where
  toEDI = id

instance FromEDI Interchange where
  fromEDI = Right

instance ToEDI Segment where
  toEDI = singletonInterchange

instance FromEDI Segment where
  fromEDI doc =
    case V.uncons (interchangeSegments doc) of
      Nothing -> Left "FromEDI Segment: expected at least one segment"
      Just (seg, _) -> Right seg

instance ToEDI (Vector Segment) where
  toEDI = Interchange defaultSyntax

instance FromEDI (Vector Segment) where
  fromEDI = Right . interchangeSegments

instance ToEDIField Element where
  toEDIElement = id

instance FromEDIField Element where
  fromEDIElement = Right

instance ToEDIField Text where
  toEDIElement = Simple

instance FromEDIField Text where
  fromEDIElement = fieldText

instance ToEDIField TL.Text where
  toEDIElement = Simple . TL.toStrict

instance FromEDIField TL.Text where
  fromEDIElement elemValue = TL.fromStrict <$> fieldText elemValue

instance ToEDIField ByteString where
  toEDIElement = Simple . TE.decodeUtf8With TEE.lenientDecode

instance FromEDIField ByteString where
  fromEDIElement elemValue = TE.encodeUtf8 <$> fieldText elemValue

instance ToEDIField BSL.ByteString where
  toEDIElement = Simple . TE.decodeUtf8With TEE.lenientDecode . BSL.toStrict

instance FromEDIField BSL.ByteString where
  fromEDIElement elemValue = BSL.fromStrict . TE.encodeUtf8 <$> fieldText elemValue

instance ToEDIField Bool where
  toEDIElement True = Simple "true"
  toEDIElement False = Simple "false"

instance FromEDIField Bool where
  fromEDIElement elemValue = do
    t <- T.toLower <$> fieldText elemValue
    case t of
      "true" -> Right True
      "false" -> Right False
      "1" -> Right True
      "0" -> Right False
      "y" -> Right True
      "n" -> Right False
      _ -> Left "FromEDIField Bool: expected true/false, 1/0, or Y/N"

instance ToEDIField Int where
  toEDIElement = integerElement . toInteger

instance FromEDIField Int where
  fromEDIElement = parseBoundedIntegral "Int"

instance ToEDIField Int8 where
  toEDIElement = integerElement . toInteger

instance FromEDIField Int8 where
  fromEDIElement = parseBoundedIntegral "Int8"

instance ToEDIField Int16 where
  toEDIElement = integerElement . toInteger

instance FromEDIField Int16 where
  fromEDIElement = parseBoundedIntegral "Int16"

instance ToEDIField Int32 where
  toEDIElement = integerElement . toInteger

instance FromEDIField Int32 where
  fromEDIElement = parseBoundedIntegral "Int32"

instance ToEDIField Int64 where
  toEDIElement = integerElement . toInteger

instance FromEDIField Int64 where
  fromEDIElement = parseBoundedIntegral "Int64"

instance ToEDIField Integer where
  toEDIElement = integerElement

instance FromEDIField Integer where
  fromEDIElement elemValue = do
    t <- fieldText elemValue
    parseInteger "Integer" t

instance ToEDIField Natural where
  toEDIElement = integerElement . toInteger

instance FromEDIField Natural where
  fromEDIElement elemValue = do
    n <- fromEDIElement elemValue :: Either String Integer
    if n < 0
      then Left "FromEDIField Natural: expected non-negative integer"
      else Right (fromInteger n)

instance ToEDIField Word where
  toEDIElement = integerElement . toInteger

instance FromEDIField Word where
  fromEDIElement = parseBoundedIntegral "Word"

instance ToEDIField Word8 where
  toEDIElement = integerElement . toInteger

instance FromEDIField Word8 where
  fromEDIElement = parseBoundedIntegral "Word8"

instance ToEDIField Word16 where
  toEDIElement = integerElement . toInteger

instance FromEDIField Word16 where
  fromEDIElement = parseBoundedIntegral "Word16"

instance ToEDIField Word32 where
  toEDIElement = integerElement . toInteger

instance FromEDIField Word32 where
  fromEDIElement = parseBoundedIntegral "Word32"

instance ToEDIField Word64 where
  toEDIElement = integerElement . toInteger

instance FromEDIField Word64 where
  fromEDIElement = parseBoundedIntegral "Word64"

instance ToEDIField a => ToEDIField (Maybe a) where
  toEDIElement Nothing = Simple ""
  toEDIElement (Just value) = toEDIElement value

instance FromEDIField a => FromEDIField (Maybe a) where
  fromEDIElement (Simple "") = Right Nothing
  fromEDIElement elemValue = Just <$> fromEDIElement elemValue

instance ToEDIField (Vector Text) where
  toEDIElement = Composite

instance FromEDIField (Vector Text) where
  fromEDIElement (Composite parts) = Right parts
  fromEDIElement (Simple t) = Right (V.singleton t)

fieldText :: Element -> Either String Text
fieldText (Simple t) = Right t
fieldText (Composite _) = Left "EDI.Class: expected simple element"

integerElement :: Integer -> Element
integerElement =
  Simple . TL.toStrict . toLazyText . TBI.decimal

parseBoundedIntegral :: forall a. (Bounded a, Integral a) => String -> Element -> Either String a
parseBoundedIntegral label elemValue = do
  t <- fieldText elemValue
  n <- parseInteger label t
  let lo = toInteger (minBound :: a)
      hi = toInteger (maxBound :: a)
  if n < lo || n > hi
    then Left ("FromEDIField " <> label <> ": integer out of bounds")
    else Right (fromInteger n)

parseInteger :: String -> Text -> Either String Integer
parseInteger label t =
  case TR.signed TR.decimal t of
    Right (n, rest) | T.null rest -> Right n
    _ -> Left ("FromEDIField " <> label <> ": invalid integer")
