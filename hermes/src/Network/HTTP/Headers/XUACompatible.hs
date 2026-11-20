{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
@X-UA-Compatible@ is a de-facto response (and @\<meta http-equiv\>@) header,
originally introduced by Microsoft Internet Explorer, used to select the
rendering / document mode of a user agent, e.g. @IE=edge@, @IE=EmulateIE7@, or
@chrome=1@. Its value is a comma-separated list of @engine=mode@ directives,
each side being an RFC 9110 token, surfaced here as a non-empty list of
'UACompatDirective' pairs.

This header is __not__ IANA-registered.

See Microsoft's <https://learn.microsoft.com/en-us/openspecs/ie_standards/ms-iedoco/380e2488-f5eb-4457-a07a-0cb1b6e4b4b5 document mode table>
and <https://learn.microsoft.com/en-us/previous-versions/aa769237(v=vs.85) documentation of comma-delimited modes>.

See also: "Network.HTTP.Headers.UserAgent", "Network.HTTP.Headers.AcceptCH", "Network.HTTP.Headers.SaveData", "Network.HTTP.Headers.Server".
-}
module Network.HTTP.Headers.XUACompatible (
  XUACompatible (..),
  UACompatDirective (..),
  IEDocumentMode (..),
  ieDocumentMode,
  xUACompatibleParser,
  renderXUACompatible,
) where

import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBSI
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Data.Word (Word8)
import Foreign.Ptr (plusPtr)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXUACompatible)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (renderSeparated, shortText)
import Wireform.Encode.Direct (dWord8, directEncode)


-- | A single @engine=mode@ directive, e.g. @IE=edge@.
data UACompatDirective = UACompatDirective
  { uaCompatEngine :: !ST.ShortText
  , uaCompatMode :: !ST.ShortText
  }
  deriving stock (Eq, Show)


-- | An @X-UA-Compatible@ value: a non-empty list of @engine=mode@ directives.
newtype XUACompatible = XUACompatible {uaCompatDirectives :: NonEmpty UACompatDirective}
  deriving stock (Eq, Show)


-- | A declared Internet Explorer mode. The browser version and document
-- doctype determine the mode ultimately used for rendering.
data IEDocumentMode
  = IEEdge
  | IEVersion {-# UNPACK #-} !Word
  | IEEmulate {-# UNPACK #-} !Word
  deriving stock (Eq, Show)


-- | Interpret an IE directive without changing its retained spelling.
ieDocumentMode :: UACompatDirective -> Maybe IEDocumentMode
ieDocumentMode (UACompatDirective engine mode)
  | not (matchesIE 32 engineBytes) = Nothing
  | matchesEdge 32 modeBytes = Just IEEdge
  | hasASCIIPrefix "emulateie" modeBytes = readModeVersion IEEmulate 9 modeBytes
  | otherwise = readModeVersion IEVersion 0 modeBytes
  where
    engineBytes = ST.toShortByteString engine
    modeBytes = ST.toShortByteString mode
{-# INLINE ieDocumentMode #-}


-- The prefix contains only lowercase ASCII letters. Setting bit five
-- matches each of those letters or its uppercase form.
hasASCIIPrefix :: SBS.ShortByteString -> SBS.ShortByteString -> Bool
hasASCIIPrefix prefix bytes = size <= SBS.length bytes && go 0
  where
    size = SBS.length prefix
    go i =
      i == size
        || (SBSI.unsafeIndex bytes i .|. 32 == SBSI.unsafeIndex prefix i && go (i + 1))
{-# INLINE hasASCIIPrefix #-}


readModeVersion :: (Word -> IEDocumentMode) -> Int -> SBS.ShortByteString -> Maybe IEDocumentMode
readModeVersion make start bytes
  | start >= size = Nothing
  | otherwise = go start 0
  where
    size = SBS.length bytes
    (maxQuotient, maxDigit) = (maxBound :: Word) `quotRem` 10
    go i !n
      | i == size = Just (make n)
      | digit > 9 || n > maxQuotient || (n == maxQuotient && digit > maxDigit) = Nothing
      | otherwise = go (i + 1) (n * 10 + digit)
      where
        digit = fromIntegral (SBSI.unsafeIndex bytes i) - 48
{-# INLINE readModeVersion #-}


instance KnownHeader XUACompatible where
  type ParseFailure XUACompatible = String
  type Cardinality XUACompatible = 'ZeroOrOne
  type Direction XUACompatible = 'Response


  parseFromHeaders _ ("IE=edge" :| []) = Right edgeCompatibility
  parseFromHeaders _ (wire :| []) = parseFieldLine "X-UA-Compatible" xUACompatibleParser wire
  parseFromHeaders _ fields =
    XUACompatible <$> (parseListFieldLines "X-UA-Compatible" (httpList directiveParser) fields >>= requireNonEmptyList "X-UA-Compatible")
  {-# INLINE parseFromHeaders #-}


  renderToHeaders _ (XUACompatible (UACompatDirective engine mode :| []))
    | matchesIE 0 (ST.toShortByteString engine)
    , matchesEdge 0 (ST.toShortByteString mode) = "IE=edge"
  renderToHeaders _ (XUACompatible directives) = encodeDirectives directives
  {-# INLINE renderToHeaders #-}


  headerName _ = hXUACompatible


xUACompatibleParser :: ParserT st String XUACompatible
xUACompatibleParser = XUACompatible <$> httpList1 "X-UA-Compatible" directiveParser


directiveParser :: ParserT st String UACompatDirective
directiveParser = UACompatDirective <$> rfc9110Token <*> ($(char '=') *> rfc9110Token)


edgeCompatibility :: XUACompatible
edgeCompatibility = XUACompatible (UACompatDirective "IE" "edge" :| [])


-- Use a mask of zero for the wire spelling and 32 for ASCII case folding.
matchesIE :: Word8 -> SBS.ShortByteString -> Bool
matchesIE mask engine =
  SBS.length engine == 2
    && SBSI.unsafeIndex engine 0 .|. mask == 73 .|. mask
    && SBSI.unsafeIndex engine 1 .|. mask == 69 .|. mask
{-# INLINE matchesIE #-}


matchesEdge :: Word8 -> SBS.ShortByteString -> Bool
matchesEdge mask mode =
  SBS.length mode == 4
    && SBSI.unsafeIndex mode 0 .|. mask == 101
    && SBSI.unsafeIndex mode 1 .|. mask == 100
    && SBSI.unsafeIndex mode 2 .|. mask == 103
    && SBSI.unsafeIndex mode 3 .|. mask == 101
{-# INLINE matchesEdge #-}


encodeDirectives :: NonEmpty UACompatDirective -> ByteString
encodeDirectives (first :| rest) = directEncode size $ \ptr start ->
  writeDirective ptr start first >>= writeRest ptr rest
  where
    add = BSI.checkedAdd "X-UA-Compatible"
    size = foldl' (\total entry -> addDirective (add total 2) entry) (addDirective 0 first) rest
    addDirective total (UACompatDirective engine mode) =
      add (add (add total (textSize engine)) 1) (textSize mode)
    textSize = SBS.length . ST.toShortByteString

    writeDirective ptr offset (UACompatDirective engine mode) = do
      engineEnd <- writeText ptr offset engine
      modeStart <- dWord8 ptr engineEnd 61
      writeText ptr modeStart mode
    writeText ptr offset text = do
      let bytes = ST.toShortByteString text
          len = SBS.length bytes
      SBSI.copyToPtr bytes 0 (ptr `plusPtr` offset) len
      pure $! offset + len
    writeRest _ [] !offset = pure offset
    writeRest ptr (entry : more) !offset = do
      space <- dWord8 ptr offset 44
      start <- dWord8 ptr space 32
      end <- writeDirective ptr start entry
      writeRest ptr more end


renderXUACompatible :: XUACompatible -> M.Builder
renderXUACompatible (XUACompatible ds) = renderSeparated ", " renderDirective (NE.toList ds)
  where
    renderDirective (UACompatDirective e m) = shortText e <> M.char7 '=' <> shortText m
