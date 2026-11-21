{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Shared wire rules for HSTS, HPKP and Expect-CT directives.
module Network.HTTP.Headers.TransportPolicy (
  assignment,
  policyAge,
  directiveEnd,
  valueless,
  extension,
  reportURI,
  quoted,
  pinPolicy,
  renderPinPolicy,
  encodePinPolicyChecked,
) where

import qualified Control.Applicative as A
import Control.Monad (void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder.Prim as Prim
import qualified Data.ByteString.Builder.Prim.Internal as Prim
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short.Internal as SBS
import qualified Data.CaseInsensitive as CI
import qualified Data.Set as Set
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as ST
import Foreign.Ptr (minusPtr, plusPtr)
import Network.HTTP.ContentNegotiation (sameNegotiationToken)
import Network.HTTP.Grammar.URIReference (absoluteURI, outboundAbsoluteURIWithFragmentBytes, uriWithFragment, validURIText)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (copyText, shortText)
import Wireform.Encode.Direct (dBytes, dWord8)
import Wireform.Parser.Driver (parseByteString)


assignment :: ParserT st e ()
assignment = ows *> $(char '=') *> ows
{-# INLINE assignment #-}


policyAge :: ParserT st String Word
policyAge = assignment *> (age <|> directQuote <|> decodedQuote)
  where
    age = saturatingDeltaSeconds maxBound
    directQuote = $(char '"') *> age <* $(char '"')
    decodedQuote = parseValue age <$> httpQuotedBytes >>= either (const failed) pure
{-# INLINE policyAge #-}


directiveEnd :: ParserT st e ()
directiveEnd = notFollowedBy tokenBytes
{-# INLINE directiveEnd #-}


valueless :: ParserT st e ()
valueless = ows *> notFollowedBy $(char '=')
{-# INLINE valueless #-}


extension :: ParserT st String (CI.CI BS.ByteString)
extension = CI.mk <$> rfc9110TokenBS <* extensionValue
{-# INLINE extension #-}


extensionValue :: ParserT st String ()
extensionValue = branch assignment (tokenBytes <|> void httpQuotedBytes) (pure ())
{-# INLINE extensionValue #-}


reportURI :: ParserT st String ShortText
reportURI = assignment *> $(char '"') *> absoluteURI <* $(char '"')
{-# INLINE reportURI #-}


pinReportURI :: ParserT st String ShortText
pinReportURI = assignment *> (direct <|> decoded)
  where
    direct = $(char '"') *> uriWithFragment <* $(char '"')
    decoded = httpQuotedBytes >>= either (const failed) pure . parseValue uriWithFragment
{-# INLINE pinReportURI #-}


parseValue :: ParserT st String a -> BS.ByteString -> Either (ParseError String) a
parseValue parser = parseByteString (parser <* eof)
{-# INLINE parseValue #-}


quoted :: ShortText -> M.Builder
quoted t = M.char7 '"' <> shortText t <> M.char7 '"'


data PinDirective pin
  = PinSHA256 !pin
  | MaxAge !Word
  | IncludeSubDomains
  | ReportURI !ShortText
  | PinExtension
  | Extension !(CI.CI BS.ByteString)


{- | HPKP and HPKP-RO share directive syntax. The supplied constructor preserves
the separate public types for their pins. The default age is used only when the
field has no max-age directive.
-}
pinPolicy
  :: (ShortText -> ShortText -> pin)
  -> ([pin] -> Word -> Bool -> Maybe ShortText -> policy)
  -> Maybe Word
  -> ParserT st String policy
pinPolicy makePin makePolicy defaultAge = ows *> parseDirectives [] Nothing False Nothing Set.empty
  where
    directive =
      $( switchAsciiCI
           [|
             case _ of
               "pin-sha256" -> known (PinSHA256 . makePin "sha256" <$> (assignment *> sha256Pin))
               "max-age" -> known (MaxAge <$> policyAge)
               "includesubdomains" -> known (IncludeSubDomains <$ valueless)
               "report-uri" -> known (ReportURI <$> pinReportURI)
             |]
       )
        <|> do
          name <- rfc9110TokenBS
          let key = CI.mk name
          if BS.length name > 4 && "pin-" `BS.isPrefixOf` CI.foldedCase key
            then PinExtension <$ (assignment *> unsupportedPin)
            else Extension key <$ extensionValue
    known action = directiveEnd *> (action <|> err "Invalid Public-Key-Pins directive")

    parseDirectives !pins !age !subdomains !uri !seen = directive >>= \case
      PinSHA256 p -> next (p : pins) age subdomains uri seen
      MaxAge n -> case age of
        Just _ -> err "Duplicate max-age directive"
        Nothing -> next pins (Just n) subdomains uri seen
      IncludeSubDomains
        | subdomains -> err "Duplicate includeSubDomains directive"
        | otherwise -> next pins age True uri seen
      ReportURI u -> case uri of
        Just _ -> err "Duplicate report-uri directive"
        Nothing -> next pins age subdomains (Just u) seen
      PinExtension -> next pins age subdomains uri seen
      Extension name
        | Set.member name seen -> err "Duplicate Public-Key-Pins extension"
        | otherwise -> next pins age subdomains uri (Set.insert name seen)

    next pins age subdomains uri seen =
      ows *> branch ($(char ';') *> ows) (parseDirectives pins age subdomains uri seen) (finish pins age subdomains uri)

    finish pins age subdomains uri = case age A.<|> defaultAge of
      Just n -> pure (makePolicy (reverse pins) n subdomains uri)
      Nothing -> err "Public-Key-Pins requires max-age"
{-# INLINE pinPolicy #-}


-- Unsupported hashes have no known digest size, but their wire value
-- still has to be a nonempty standard Base64 fingerprint.
unsupportedPin :: ParserT st String ()
unsupportedPin = do
  bytes <- httpQuotedBytes
  let len = BS.length bytes
  if len < 4 || len `rem` 4 /= 0
    then invalid
    else do
      let padding
            | BS.last bytes /= 61 = 0
            | BS.index bytes (len - 2) == 61 = 2
            | otherwise = 1
      case decodeFixedBase64 (3 * (len `quot` 4) - padding) bytes of
        Just _ -> pure ()
        Nothing -> invalid
  where
    invalid = err "Invalid Public-Key-Pins fingerprint"


sha256Pin :: ParserT st String ShortText
sha256Pin =
  ($(char '"') *> sha256Text <* $(char '"'))
    <|> (httpQuotedBytes >>= either (const failed) pure . parseValue sha256Text)
  where
    sha256Text = shortASCIIFromParser_ (embedError (fixedBase64Bytes 32) (const failed))
{-# INLINE sha256Pin #-}


renderPinPolicy
  :: (pin -> ShortText)
  -> (pin -> ShortText)
  -> [pin]
  -> Maybe Word
  -> Bool
  -> Maybe ShortText
  -> M.Builder
renderPinPolicy algorithm fingerprint pins age subdomains uri =
  M.intersperse "; " (map renderPin pins ++ agePart ++ subdomainsPart ++ reportPart)
  where
    renderPin pin = "pin-" <> shortText (algorithm pin) <> "=" <> quoted (fingerprint pin)
    agePart = maybe [] (\n -> ["max-age=" <> M.wordDec n]) age
    subdomainsPart = if subdomains then ["includeSubDomains"] else []
    reportPart = maybe [] (\u -> ["report-uri=" <> quoted u]) uri


-- | Check every fingerprint and the reporting URI before emitting a policy.
encodePinPolicyChecked
  :: (pin -> ShortText)
  -> (pin -> ShortText)
  -> [pin]
  -> Maybe Word
  -> Bool
  -> Maybe ShortText
  -> Maybe BS.ByteString
encodePinPolicyChecked algorithm fingerprint pins age subdomains uri
  | maybe True (validURIText outboundAbsoluteURIWithFragmentBytes) uri = sizePins optionsSize pins
  | otherwise = Nothing
  where
    add = BSI.checkedAdd "Public-Key-Pins"
    width = SBS.length . ST.toShortByteString
    optionsSize = add (maybe 0 (const 30) age + if subdomains then 19 else 0) (maybe 0 (add 15 . width) uri)
    sizePins !size [] = Just $ BSI.unsafeCreateUptoN size $ \ptr -> do
      afterPins <- writePins ptr 0 pins
      afterAge <- case age of
        Nothing -> pure afterPins
        Just seconds -> do
          start <- separator ptr afterPins >>= \end -> dBytes ptr end "max-age="
          end <- Prim.runB Prim.wordDec seconds (ptr `plusPtr` start)
          pure (end `minusPtr` ptr)
      afterDomains <-
        if subdomains
          then separator ptr afterAge >>= \end -> dBytes ptr end "includeSubDomains"
          else pure afterAge
      case uri of
        Nothing -> pure afterDomains
        Just endpoint -> do
          start <- separator ptr afterDomains >>= \end -> dBytes ptr end "report-uri=\""
          end <- copyText ptr start endpoint
          dWord8 ptr end 34
    sizePins !size (pin : rest)
      | isHttpTokenText name, validFingerprint (sameNegotiationToken "sha256" name) digest =
          sizePins (add (add (add size 9) (width name)) (width digest)) rest
      | otherwise = Nothing
      where
        name = algorithm pin
        digest = fingerprint pin
    writePins _ !offset [] = pure offset
    writePins ptr !offset (pin : rest) = do
      start <- separator ptr offset >>= \end -> dBytes ptr end "pin-"
      nameEnd <- copyText ptr start (algorithm pin)
      opened <- dBytes ptr nameEnd "=\""
      digestEnd <- copyText ptr opened (fingerprint pin)
      closed <- dWord8 ptr digestEnd 34
      writePins ptr closed rest
    separator ptr offset
      | offset == 0 = pure 0
      | otherwise = dBytes ptr offset "; "
{-# INLINE encodePinPolicyChecked #-}


validFingerprint :: Bool -> ShortText -> Bool
validFingerprint sha256 text =
  size >= 4 && size `rem` 4 == 0
    && (not sha256 || (size == 44 && padding == 1))
    && canonicalPadding
    && validBase64Prefix bytes end
  where
    bytes = ST.toShortByteString text
    size = SBS.length bytes
    at = SBS.unsafeIndex bytes
    padding
      | at (size - 1) /= 61 = 0
      | at (size - 2) == 61 = 2
      | otherwise = 1
    end = size - padding
    canonicalPadding = case padding of
      2 -> at (end - 1) `BS.elem` "AQgw"
      1 -> at (end - 1) `BS.elem` "AEIMQUYcgkosw048"
      _ -> True
{-# INLINE validFingerprint #-}
