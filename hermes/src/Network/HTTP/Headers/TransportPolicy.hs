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
) where

import qualified Control.Applicative as A
import Control.Monad (void)
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import qualified Data.Set as Set
import Data.Text.Short (ShortText)
import Network.HTTP.Grammar.URIReference (absoluteURI, uriWithFragment)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)
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
