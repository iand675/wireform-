{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{- |
@Content-Security-Policy@ carries an ordered list of independent policies.
Use 'cspPolicies' to access the whole field and 'effectiveDirectives' to
project the first occurrence of each directive within one policy.

<https://www.w3.org/TR/CSP3/#csp-header Content Security Policy Level 3>

See also: "Network.HTTP.Headers.ContentSecurityPolicyReportOnly",
"Network.HTTP.Headers.ReportingEndpoints", "Network.HTTP.Headers.PermissionsPolicy".
-}
module Network.HTTP.Headers.ContentSecurityPolicy (
  ContentSecurityPolicy (ContentSecurityPolicy, cspDirectives, ContentSecurityPolicyList, cspPolicies),
  CSPPolicy (..),
  Directive (..),
  effectiveDirectives,
  contentSecurityPolicyParser,
  renderContentSecurityPolicy,
  directiveParser,
  renderDirective,
  directiveSetParser,
  renderDirectiveSet,
  policyListParser,
  parsePolicyFieldLines,
  renderPolicyList,
  encodePolicyListChecked,
) where

import Data.Bits ((.&.), (.|.), complement, xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short.Internal as SBS
import Data.Char (isAsciiUpper, toLower)
import qualified Data.CharSet as CharSet
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Short as ST
import Data.Word (Word8, Word64)
import Foreign.Ptr (plusPtr)
import GHC.Exts (Int (I#), indexWord8ArrayAsWord64#)
import GHC.Word (Word64 (W64#))
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hContentSecurityPolicy)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R
import Wireform.Encode.Direct (dWord8, directEncode)


-- | A directive's wire spelling and its whitespace-separated tokens.
data Directive = Directive
  { directiveName :: !ST.ShortText
  , directiveValues :: ![ST.ShortText]
  }
  deriving stock (Eq, Show)


-- | One policy, retaining all serialized directives in wire order.
newtype CSPPolicy = CSPPolicy {policyDirectives :: [Directive]}
  deriving stock (Eq, Show)


newtype ContentSecurityPolicy = ContentSecurityPolicyList {cspPolicies :: NonEmpty CSPPolicy}
  deriving stock (Eq, Show)


{- | Construct one policy or read the first policy's directives. Use
'cspPolicies' to inspect a field that may contain multiple policies.
-}
pattern ContentSecurityPolicy :: [Directive] -> ContentSecurityPolicy
pattern ContentSecurityPolicy {cspDirectives} <- (policyDirectives . NE.head . cspPolicies -> cspDirectives)
  where
    ContentSecurityPolicy directives = ContentSecurityPolicyList (CSPPolicy directives :| [])


{-# COMPLETE ContentSecurityPolicy #-}


{- | CSP3 normalizes directive names and uses the first value of each name.
Apply this within a single policy so directives from different policies
remain independent.
-}
effectiveDirectives :: CSPPolicy -> [Directive]
effectiveDirectives = go Set.empty . policyDirectives
  where
    go _ [] = []
    go seen (directive : rest)
      | Set.member name seen = go seen rest
      | otherwise = directive {directiveName = name} : go (Set.insert name seen) rest
      where
        spelling = directiveName directive
        name
          | ST.any isAsciiUpper spelling = ST.fromText (T.map lowerAscii (ST.toText spelling))
          | otherwise = spelling
        lowerAscii c
          | isAsciiUpper c = toLower c
          | otherwise = c


instance KnownHeader ContentSecurityPolicy where
  type ParseFailure ContentSecurityPolicy = String
  type Cardinality ContentSecurityPolicy = 'ZeroOrOne
  type Direction ContentSecurityPolicy = 'Response


  parseFromHeaders _ = fmap ContentSecurityPolicyList . parsePolicyFieldLines "Content-Security-Policy"


  renderToHeaders _ = M.toStrictByteString . renderContentSecurityPolicy


  renderToHeadersChecked _ = encodePolicyListChecked . cspPolicies


  headerName _ = hContentSecurityPolicy


contentSecurityPolicyParser :: ParserT st String ContentSecurityPolicy
contentSecurityPolicyParser = ContentSecurityPolicyList <$> policyListParser


parsePolicyFieldLines :: String -> NonEmpty ByteString -> Either String (NonEmpty CSPPolicy)
parsePolicyFieldLines name = fmap explicitEmptyPolicy . parseListFieldLines name nonemptyPoliciesParser


policyListParser :: ParserT st String (NonEmpty CSPPolicy)
policyListParser = explicitEmptyPolicy <$> nonemptyPoliciesParser


explicitEmptyPolicy :: [CSPPolicy] -> NonEmpty CSPPolicy
explicitEmptyPolicy [] = CSPPolicy [] :| []
explicitEmptyPolicy (first : rest) = first :| rest


-- HTTP field values admit SP and HTAB. Commas always delimit policies and
-- never occur inside a directive, so physical lines can be parsed separately.
nonemptyPoliciesParser :: ParserT st String [CSPPolicy]
nonemptyPoliciesParser = do
  directives <- directiveSetWith isOWS
  rest <- branch $(char ',') nonemptyPoliciesParser (pure [])
  pure $ case directives of
    [] -> rest
    _ -> CSPPolicy directives : rest


-- | Read one serialized CSP directive using Infra ASCII whitespace.
directiveParser :: ParserT st String Directive
directiveParser = directiveWith isCSPWhitespace


-- | Read a single serialized policy, including policies from HTML sources.
directiveSetParser :: ParserT st String [Directive]
directiveSetParser = directiveSetWith isCSPWhitespace


isCSPWhitespace :: Char -> Bool
isCSPWhitespace c = isOWS c || c == '\n' || c == '\r' || c == '\f'


directiveSetWith :: (Char -> Bool) -> ParserT st String [Directive]
directiveSetWith isSpace = space *> go
  where
    space = skipMany (skipSatisfyAscii isSpace)
    separator = $(char ';') *> space
    go =
      branch separator go $
        withOption
          (directiveWith isSpace)
          (\directive -> (directive :) <$> branch separator go (pure []))
          (pure [])
{-# INLINE directiveSetWith #-}


directiveWith :: (Char -> Bool) -> ParserT st String Directive
directiveWith isSpace = do
  name <- shortASCIIFromParser_ (asciiSetBytes directiveNameChars)
  values <- branch (skipSome space) (many (value <* skipMany space)) (pure [])
  pure (Directive name values)
  where
    space = skipSatisfyAscii isSpace
    value = shortASCIIFromParser_ (asciiSetBytes directiveValueChars)
{-# INLINE directiveWith #-}


directiveNameChars :: AsciiSet
directiveNameChars = asciiSet (CharSet.range 'A' 'Z' <> CharSet.range 'a' 'z' <> CharSet.range '0' '9' <> "-")
{-# NOINLINE directiveNameChars #-}


directiveValueChars :: AsciiSet
directiveValueChars = asciiSet (CharSet.range '\x21' '\x2b' <> CharSet.range '\x2d' '\x3a' <> CharSet.range '\x3c' '\x7e')
{-# NOINLINE directiveValueChars #-}


renderDirective :: Directive -> M.Builder
renderDirective (Directive name values) = R.shortText name <> foldMap (\value -> " " <> R.shortText value) values


renderDirectiveSet :: [Directive] -> M.Builder
renderDirectiveSet = R.renderSeparated "; " renderDirective


renderPolicyList :: NonEmpty CSPPolicy -> M.Builder
renderPolicyList = R.renderSeparated ", " (renderDirectiveSet . policyDirectives) . filter (not . null . policyDirectives) . NE.toList


-- | Check names and tokens before emitting the full list of policies.
encodePolicyListChecked :: NonEmpty CSPPolicy -> Maybe ByteString
encodePolicyListChecked policies = measurePolicies 0 entries
  where
    entries = NE.toList policies
    add = BSI.checkedAdd "CSP policy list"
    measurePolicies !size [] = Just $ directEncode size $ \ptr start -> writePolicies ptr start entries
    measurePolicies !size (CSPPolicy [] : rest) = measurePolicies size rest
    measurePolicies !size (CSPPolicy directives : rest) =
      let end = measureDirectives size directives
      in if end < 0 then Nothing else measurePolicies (add end (if size == 0 then 0 else 2)) rest
    measureDirectives !size [] = size
    measureDirectives !size (Directive name values : rest)
      | validCompactToken nameWord nameByte name
      , let end = measureValues (add size (width name)) values
      , end >= 0 = case rest of
          [] -> end
          _ -> measureDirectives (add end 2) rest
      | otherwise = -1
    measureValues !size [] = size
    measureValues !size (value : rest)
      | validCompactToken valueWord valueByte value = measureValues (add (add size 1) (width value)) rest
      | otherwise = -1
    writePolicies _ !offset [] = pure offset
    writePolicies ptr !offset (CSPPolicy [] : rest) = writePolicies ptr offset rest
    writePolicies ptr !offset (CSPPolicy directives : rest) = do
      start <- if offset == 0 then pure offset else dWord8 ptr offset 44 >>= \comma -> dWord8 ptr comma 32
      end <- writeDirectives ptr start directives
      writePolicies ptr end rest
    writeDirectives _ !offset [] = pure offset
    writeDirectives ptr !offset (Directive name values : rest) = do
      nameEnd <- copyText ptr offset name
      end <- writeValues ptr nameEnd values
      case rest of
        [] -> pure end
        _ -> do
          semicolon <- dWord8 ptr end 59
          start <- dWord8 ptr semicolon 32
          writeDirectives ptr start rest
    writeValues _ !offset [] = pure offset
    writeValues ptr !offset (value : rest) = do
      start <- dWord8 ptr offset 32
      end <- copyText ptr start value
      writeValues ptr end rest
    copyText ptr offset text =
      let bytes = ST.toShortByteString text
          size = SBS.length bytes
      in SBS.copyToPtr bytes 0 (ptr `plusPtr` offset) size *> pure (offset + size)
    width = SBS.length . ST.toShortByteString


-- Scan whole ASCII lanes when possible. The last load may overlap the
-- preceding one and stays within the compact value.
validCompactToken :: (Word64 -> Bool) -> (Word8 -> Bool) -> ST.ShortText -> Bool
validCompactToken wordOK byteOK value =
  case ST.toShortByteString value of
    bytes@(SBS.SBS array) ->
      let size = SBS.length bytes
          load (I# offset) = W64# (indexWord8ArrayAsWord64# array offset)
          scan !offset
            | offset >= size - 8 = wordOK (load (size - 8))
            | otherwise = wordOK (load offset) && scan (offset + 8)
      in size > 0 && if size < 8 then scanCompactBytes byteOK bytes 0 else scan 0
{-# INLINE validCompactToken #-}


nameByte :: Word8 -> Bool
nameByte byte = isLatinLetter c || isDigit c || byte == 45
  where
    c = BSI.w2c byte
{-# INLINE nameByte #-}


valueByte :: Word8 -> Bool
valueByte byte = byte >= 33 && byte <= 126 && byte /= 44 && byte /= 59
{-# INLINE valueByte #-}


nameWord :: Word64 -> Bool
nameWord bytes = coversAsciiLanes bytes (asciiAlnumMask low .|. complement ((low `xor` 0x2d2d2d2d2d2d2d2d) + ascii))
  where
    ascii = 0x7f7f7f7f7f7f7f7f
    low = bytes .&. ascii
{-# INLINE nameWord #-}


valueWord :: Word64 -> Bool
valueWord bytes =
  coversAsciiLanes bytes
    (((low .|. asciiHighBits) - 0x2121212121212121) .&. (0xfefefefefefefefe - low)
      .&. ((low `xor` 0x2c2c2c2c2c2c2c2c) + ascii) .&. ((low `xor` 0x3b3b3b3b3b3b3b3b) + ascii))
  where
    ascii = 0x7f7f7f7f7f7f7f7f
    low = bytes .&. ascii
{-# INLINE valueWord #-}


renderContentSecurityPolicy :: ContentSecurityPolicy -> M.Builder
renderContentSecurityPolicy = renderPolicyList . cspPolicies
