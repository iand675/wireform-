{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
@Forwarded@ — request header by which proxies disclose the client-facing
information that would otherwise be lost or altered when a request passes
through intermediaries: the originating client (@for@), the interface a
proxy received the request on (@by@), the host the client targeted
(@host@), and the protocol used (@proto@). It is the IANA-registered,
standardized replacement for the de-facto @X-Forwarded-*@ family.

== Grammar (RFC 7239 §4)

@
Forwarded         = 1#forwarded-element
forwarded-element = [ forwarded-pair ] *( ";" [ forwarded-pair ] )
forwarded-pair    = token "=" value
value             = token / quoted-string
@

Each element describes one hop. Parameter names are case-insensitive and
unique within a hop. The decoder validates registered parameter values and
preserves extension parameters. Names retain their spelling and values retain
their token or quoted-string form.

Spec: <https://www.rfc-editor.org/rfc/rfc7239#section-4>

See also: "Network.HTTP.Headers.XForwardedFor",
"Network.HTTP.Headers.XForwardedHost",
"Network.HTTP.Headers.XForwardedProto",
"Network.HTTP.Headers.ProxyStatus", "Network.HTTP.Headers.Via".
-}
module Network.HTTP.Headers.Forwarded (
  Forwarded (..),
  ForwardedElement (..),
  ForwardedValue (..),
  forwardedParser,
  renderForwarded,
) where

import Control.Monad (guard, void)
import Data.Bits (setBit, testBit)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBSI
import Data.Either (isRight)
import qualified Data.CaseInsensitive as CI
import Data.Char (isAsciiLower, isAsciiUpper)
import qualified Data.Set as Set
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Foreign.Ptr (plusPtr)
import Network.HTTP.ContentNegotiation (sameNegotiationToken)
import Network.HTTP.Grammar.Authority (isUriSchemeText, uriSchemeBytes)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hForwarded)
import Network.HTTP.Headers.Host (hostParser)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R
import Network.IPAddress (ipv4Parser, skipIPv6Parser)
import Wireform.Encode.Direct (dWord8)
import Wireform.Parser.Driver (parseByteString)


-- ---------------------------------------------------------------------------
-- Data
-- ---------------------------------------------------------------------------

{- | A @forwarded-pair@ value: either a bare @token@ or a
@quoted-string@ (stored unescaped).  The distinction is retained so
the quoted form can be rendered again. In quoted values, Latin-1 code
points represent the corresponding HTTP wire octets.
-}
data ForwardedValue
  = ForwardedToken !ShortText
  | ForwardedQuoted !ShortText
  deriving stock (Eq, Show)


{- | A single @forwarded-element@: an ordered list of @token=value@
pairs (typically @for@\/@by@\/@host@\/@proto@), preserving the order.
-}
newtype ForwardedElement = ForwardedElement
  { forwardedPairs :: [(ShortText, ForwardedValue)]
  }
  deriving stock (Eq, Show)


{- | The @Forwarded@ header value: an ordered list of
forwarded-elements, starting with the first proxy that updated the field.
-}
newtype Forwarded = Forwarded
  { forwardedElements :: [ForwardedElement]
  }
  deriving stock (Eq, Show)


instance KnownHeader Forwarded where
  type ParseFailure Forwarded = String
  type Cardinality Forwarded = 'ZeroOrMore
  type Direction Forwarded = 'Request


  parseFromHeaders _ = fmap Forwarded . parseListFieldLines "Forwarded" (httpList forwardedElementParser)


  renderToHeaders _ fwd = [M.toStrictByteString (renderForwarded fwd)]


  renderToHeadersChecked _ (Forwarded hops)
    | all validHop hops = Just [encodeForwarded hops]
    | otherwise = Nothing


  headerName _ = hForwarded


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

-- | Parse a @Forwarded@ value: a comma-separated list of elements.
forwardedParser :: ParserT st String Forwarded
forwardedParser = Forwarded <$> httpList forwardedElementParser


data ParameterName = For | By | Host | Proto | Extension
  deriving stock (Enum)


-- A hop may have empty pair positions. A bare semicolon distinguishes an
-- explicit empty hop from an empty slot in the outer comma list.
forwardedElementParser :: forall st. ParserT st String ForwardedElement
forwardedElementParser =
  ForwardedElement <$> (populated 0 Set.empty <|> ($(char ';') *> entries 0 Set.empty))
  where
    rest known extensions = branch $(char ';') (entries known extensions) (pure [])
    entries known extensions = skipMany $(char ';') *> (populated known extensions <|> pure [])

    populated :: Word -> Set.Set (CI.CI BS.ByteString) -> ParserT st String [(ShortText, ForwardedValue)]
    populated !known !extensions = withByteString parameterName $ \name raw -> do
      $(char '=')
      value <- parameterValue name
      following <- case name of
        Extension ->
          let key = CI.mk raw
          in if Set.member key extensions
               then duplicate known extensions
               else rest known (Set.insert key extensions)
        _
          | testBit known (fromEnum name) -> duplicate known extensions
          | otherwise -> rest (setBit known (fromEnum name)) extensions
      let !text = parameterText name raw
      pure ((text, value) : following)

    duplicate known extensions =
      rest known extensions *> err "Duplicate parameter in Forwarded element"


parameterName :: ParserT st String ParameterName
parameterName =
  $( switchAsciiCI
       [|
         case _ of
           "for" -> For <$ lookahead $(char '=')
           "by" -> By <$ lookahead $(char '=')
           "host" -> Host <$ lookahead $(char '=')
           "proto" -> Proto <$ lookahead $(char '=')
         |]
   )
    <|> (Extension <$ tokenBytes)
{-# INLINE parameterName #-}


-- Share the canonical lowercase spellings. Preserve a compact copy of
-- any other spelling, including mixed-case registered names.
parameterText :: ParameterName -> BS.ByteString -> ShortText
parameterText name raw = case name of
  For | raw == "for" -> "for"
  By | raw == "by" -> "by"
  Host | raw == "host" -> "host"
  Proto | raw == "proto" -> "proto"
  _ -> STU.fromByteStringUnsafe raw
{-# INLINE parameterText #-}


parameterValue :: ParameterName -> ParserT st String ForwardedValue
parameterValue = \case
  For -> knownValue For
  By -> knownValue By
  Host -> validatedValue bareHost (knownValueSyntax Host True)
  Proto -> knownValue Proto
  Extension -> (ForwardedToken <$> rfc9110Token) <|> (ForwardedQuoted . ST.fromText <$> httpQuotedString)
  where
    -- A host reg-name may itself contain delimiter characters. Limit the
    -- bare form to its HTTP token before validating the authority.
    bareHost = rfc9110TokenBS >>= validate (void hostParser)
    knownValue name = validatedValue (knownValueSyntax name False) (knownValueSyntax name True)
{-# INLINE parameterValue #-}


knownValueSyntax :: ParameterName -> Bool -> ParserT st String ()
knownValueSyntax name quoted = case name of
  For -> node
  By -> node
  Host -> void hostParser
  Proto -> uriSchemeBytes
  Extension -> pure ()
  where
    node = nodeName *> (if quoted then optionalPort else pure ())
{-# INLINE knownValueSyntax #-}


validatedValue :: ParserT st String () -> ParserT st String () -> ParserT st String ForwardedValue
validatedValue bare quoted =
  (ForwardedToken <$> shortASCIIFromParser_ (bare <* notFollowedBy tokenBytes))
    <|> do
      raw <- httpQuotedBytes
      validate quoted raw
      pure (ForwardedQuoted (STU.fromByteStringUnsafe raw))
{-# INLINE validatedValue #-}


validate :: ParserT st String () -> BS.ByteString -> ParserT st String ()
validate parser = either (const failed) pure . parseByteString (parser <* eof)
{-# INLINE validate #-}


nodeName :: ParserT st String ()
nodeName =
  $(switchAsciiCI [|case _ of "unknown" -> pure ()|])
    <|> obfuscated
    <|> ($(char '[') *> skipIPv6Parser <* $(char ']'))
    <|> void ipv4Parser
{-# INLINE nodeName #-}


optionalPort :: ParserT st String ()
optionalPort = branch $(char ':') (obfuscated <|> numeric) (pure ())
  where
    numeric = withSpan (skipSome (skipSatisfyAscii isDigit)) $ \() (Span (Pos start) (Pos end)) -> guard (end - start <= 5)
{-# INLINE optionalPort #-}


obfuscated :: ParserT st String ()
obfuscated =
  $(char '_') *> skipSome (skipSatisfyAscii allowed)
  where
    allowed c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '.' || c == '_' || c == '-'
{-# INLINE obfuscated #-}


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

validHop :: ForwardedElement -> Bool
validHop (ForwardedElement pairs) = check 0 Set.empty pairs
  where
    check :: Word -> Set.Set (CI.CI BS.ByteString) -> [(ShortText, ForwardedValue)] -> Bool
    check !_ !_ [] = True
    check !known !extensions ((name, value) : rest) =
      isHttpTokenText name && validForwardedValue kind value && case kind of
        Extension -> not (Set.member key extensions) && check known (Set.insert key extensions) rest
        _ -> not (testBit known bit) && check (setBit known bit) extensions rest
      where
        kind
          | sameNegotiationToken name "for" = For
          | sameNegotiationToken name "by" = By
          | sameNegotiationToken name "host" = Host
          | sameNegotiationToken name "proto" = Proto
          | otherwise = Extension
        bit = fromEnum kind
        key = CI.mk (ST.toByteString name)


validForwardedValue :: ParameterName -> ForwardedValue -> Bool
validForwardedValue kind = \case
  ForwardedToken text -> isHttpTokenText text && validSyntax False text
  ForwardedQuoted text -> SBS.all (isOpaqueByte . fromIntegral) (ST.toShortByteString text) && validSyntax True text
  where
    validSyntax quoted text = case kind of
      Extension -> True
      Proto -> isUriSchemeText text
      _ -> isRight (parseByteString (knownValueSyntax kind quoted <* eof) (ST.toByteString text))


encodeForwarded :: [ForwardedElement] -> BS.ByteString
encodeForwarded [] = BS.empty
encodeForwarded hops@(first : rest) = BSI.unsafeCreateUptoN (foldl' hopBound 0 hops) $ \ptr -> do
  afterFirst <- writeHop ptr 0 first
  writeHops ptr afterFirst rest
  where
    add = BSI.checkedAdd "Forwarded"
    width = SBS.length . ST.toShortByteString
    hopBound size (ForwardedElement pairs) = foldl' pairBound (add size 3) pairs
    pairBound size (name, value) = add (add size (add 2 (width name))) (valueBound value)
    valueBound (ForwardedToken token) = width token
    valueBound (ForwardedQuoted quoted) = let size = width quoted in add 2 (add size size)

    writeHops _ !offset [] = pure offset
    writeHops ptr !offset (hop : more) = do
      afterComma <- dWord8 ptr offset 44
      afterSpace <- dWord8 ptr afterComma 32
      afterHop <- writeHop ptr afterSpace hop
      writeHops ptr afterHop more

    writeHop ptr offset (ForwardedElement []) = dWord8 ptr offset 59
    writeHop ptr offset (ForwardedElement (entry : entries)) =
      writePair ptr offset entry >>= \end -> writePairs ptr end entries

    writePairs _ !offset [] = pure offset
    writePairs ptr !offset (entry : entries) = do
      afterSeparator <- dWord8 ptr offset 59
      afterEntry <- writePair ptr afterSeparator entry
      writePairs ptr afterEntry entries

    writePair ptr offset (name, value) = do
      afterName <- copyText ptr offset name
      start <- dWord8 ptr afterName 61
      case value of
        ForwardedToken token -> copyText ptr start token
        ForwardedQuoted text -> R.foldHttpQuotedShortBytes (dWord8 ptr) start (ST.toShortByteString text)

    copyText ptr offset text = do
      let bytes = ST.toShortByteString text
          size = SBS.length bytes
      SBSI.copyToPtr bytes 0 (ptr `plusPtr` offset) size
      pure $! offset + size


renderForwarded :: Forwarded -> M.Builder
renderForwarded (Forwarded els) =
  R.renderSeparated ", " renderElement els


renderElement :: ForwardedElement -> M.Builder
renderElement (ForwardedElement []) = M.char7 ';'
renderElement (ForwardedElement pairs) =
  R.renderSeparated (M.char7 ';') renderPair pairs


renderPair :: (ShortText, ForwardedValue) -> M.Builder
renderPair (name, val) =
  R.shortText name <> M.char7 '=' <> renderValue val


renderValue :: ForwardedValue -> M.Builder
renderValue = \case
  ForwardedToken t -> R.shortText t
  ForwardedQuoted t -> R.renderHttpQuotedShortText t
