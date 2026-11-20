{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- | @Method-Check@ is the method token sent by the original W3C cross-site
access draft. HTTP method comparison is case-sensitive.

Spec: <https://www.w3.org/TR/2007/WD-access-control-20071126/>
-}
module Network.HTTP.Headers.MethodCheck (
  MethodCheck (MethodCheck, methodCheckValue),
  methodCheckMethod,
  methodCheckFromMethod,
  methodCheckParser,
  renderMethodCheck,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (shortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hMethodCheck)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Methods (Method, MethodError, methodToBytes, methodTokenParser, mkMethod)


data MethodCheck
  = RawMethodCheck !ST.ShortText
  | ParsedMethodCheck !Method


-- | The original spelling of a single HTTP method token.
pattern MethodCheck :: ST.ShortText -> MethodCheck
pattern MethodCheck {methodCheckValue} <- (legacyText -> methodCheckValue)
  where
    MethodCheck raw = RawMethodCheck raw


{-# COMPLETE MethodCheck #-}


instance Eq MethodCheck where
  left == right = wireBytes left == wireBytes right


instance Show MethodCheck where
  showsPrec precedence (MethodCheck value) =
    showParen (precedence > 10) $ showString "MethodCheck {methodCheckValue = " . shows value . showChar '}'


legacyText :: MethodCheck -> ST.ShortText
legacyText (RawMethodCheck text) = text
legacyText (ParsedMethodCheck method) = STU.fromByteStringUnsafe (methodToBytes method)


wireBytes :: MethodCheck -> ByteString
wireBytes (RawMethodCheck text) = ST.toByteString text
wireBytes (ParsedMethodCheck method) = methodToBytes method


methodCheckMethod :: MethodCheck -> Either MethodError Method
methodCheckMethod (RawMethodCheck text) = mkMethod (ST.toByteString text)
methodCheckMethod (ParsedMethodCheck method) = Right method


methodCheckFromMethod :: Method -> Either String MethodCheck
methodCheckFromMethod = checkedMethod . methodToBytes


checkedMethod :: ByteString -> Either String MethodCheck
checkedMethod = either (Left . show) (Right . ParsedMethodCheck) . mkMethod
{-# INLINE checkedMethod #-}


instance KnownHeader MethodCheck where
  type ParseFailure MethodCheck = String
  type Cardinality MethodCheck = 'ZeroOrOne
  type Direction MethodCheck = 'Request


  parseFromHeaders _ (wire :| [])
    | not (BS.null wire)
    , fieldSpace (BSU.unsafeHead wire) || fieldSpace (BSU.unsafeLast wire) =
        checkedMethod (BS.dropWhileEnd fieldSpace (BS.dropWhile fieldSpace wire))
    | otherwise = checkedMethod wire
    where
      fieldSpace byte = byte == 32 || byte == 9
  parseFromHeaders _ _ = Left "Multiple Method-Check field lines"


  renderToHeaders _ = wireBytes


  headerName _ = hMethodCheck


methodCheckParser :: ParserT st String MethodCheck
methodCheckParser = ParsedMethodCheck <$> methodTokenParser


renderMethodCheck :: MethodCheck -> M.Builder
renderMethodCheck (RawMethodCheck value) = shortText value
renderMethodCheck (ParsedMethodCheck method) = M.byteString (methodToBytes method)
