{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

{- |
@Surrogate-Control@ carries processing and cache directives for edge
surrogates. A directive may include an ordered list of device tokens
that identify its recipients in @Surrogate-Capability@.

The codec checks the defined directive arguments, including the optional
freshness extension in @max-age@ and the ordered capabilities in @content@.
Extensions retain their unquoted argument, with opaque quoted bytes mapped
one-for-one to Latin-1 code points. The typed projection also recognizes
the stale-serving delay directives used by Fastly.

Spec: <https://www.w3.org/TR/edge-arch/>

See also: "Network.HTTP.Headers.SurrogateCapability", "Network.HTTP.Headers.CDNCacheControl", "Network.HTTP.Headers.CacheControl".
-}
module Network.HTTP.Headers.SurrogateControl (
  SurrogateControl (..),
  SurrogateControlDirective (SurrogateControlDirective, TargetedSurrogateControlDirective, surrogateControlName, surrogateControlValue),
  SurrogateControlKind (..),
  SurrogateFreshness (..),
  surrogateControlTargets,
  surrogateControlKind,
  surrogateControlFromKind,
  surrogateControlParser,
  renderSurrogateControl,
) where

import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBSI
import Data.Char (toLower)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (renderHttpQuotedString, shortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSurrogateControl)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.SurrogateCapability (SurrogateFeature, parseSurrogateFeatureSet, renderSurrogateFeatureSet, surrogateCapabilitySetParser)


newtype SurrogateControl = SurrogateControl
  { surrogateControlDirectives :: NonEmpty SurrogateControlDirective
  }
  deriving stock (Eq, Show)


data SurrogateControlDirective
  = SurrogateControlDirective
      { surrogateControlName :: !ST.ShortText
      , surrogateControlValue :: !(Maybe ST.ShortText)
      }
  | TargetedSurrogateControlDirective
      { surrogateControlName :: !ST.ShortText
      , surrogateControlValue :: !(Maybe ST.ShortText)
      , targetedSurrogateDevices :: !(NonEmpty ST.ShortText)
      }
  deriving stock (Eq, Show)


{- | A directive's recognized argument. Freshness and staleness delays
are measured in seconds and saturate at the largest representable 'Word'.
-}
data SurrogateControlKind
  = SurrogateNoStore
  | SurrogateNoStoreRemote
  | SurrogateMaxAge {-# UNPACK #-} !SurrogateFreshness
  | SurrogateContent !(NonEmpty SurrogateFeature)
  | SurrogateStaleWhileRevalidate {-# UNPACK #-} !Word
  | SurrogateStaleIfError {-# UNPACK #-} !Word
  | SurrogateExtension !ST.ShortText !(Maybe ST.ShortText)
  deriving stock (Eq, Show)


data SurrogateFreshness = SurrogateFreshness
  { surrogateFreshSeconds :: {-# UNPACK #-} !Word
  , surrogateFreshnessExtension :: {-# UNPACK #-} !Word
  }
  deriving stock (Eq, Show)


-- | Target device tokens in wire order. An empty list denotes a global directive.
surrogateControlTargets :: SurrogateControlDirective -> [ST.ShortText]
surrogateControlTargets SurrogateControlDirective {} = []
surrogateControlTargets (TargetedSurrogateControlDirective _ _ targets) = NE.toList targets


{- | Interpret the argument. An invalid argument constructed by hand returns
'Left'. Unknown directives retain their argument and original name.
-}
surrogateControlKind :: SurrogateControlDirective -> Either String SurrogateControlKind
surrogateControlKind directive =
  case foldedName (ST.toByteString (surrogateControlName directive)) of
    "no-store" -> flag SurrogateNoStore
    "no-store-remote" -> flag SurrogateNoStoreRemote
    "max-age" -> argument (parseFieldLine "Surrogate-Control max-age" maxAge . ST.toByteString)
    "content" -> argument (fmap SurrogateContent . parseSurrogateFeatureSet)
    "stale-while-revalidate" -> stale SurrogateStaleWhileRevalidate
    "stale-if-error" -> stale SurrogateStaleIfError
    _ -> Right (SurrogateExtension (surrogateControlName directive) value)
  where
    value = surrogateControlValue directive
    flag kind = case value of
      Nothing -> Right kind
      Just _ -> Left "Surrogate-Control flag contains an argument"
    argument readValue = maybe (Left "Missing Surrogate-Control argument") readValue value
    stale wrap = argument (fmap wrap . parseFieldLine "Surrogate-Control stale delay" delay . ST.toByteString)


-- | Build a directive from its typed argument and ordered device targets.
surrogateControlFromKind :: SurrogateControlKind -> [ST.ShortText] -> SurrogateControlDirective
surrogateControlFromKind kind targets = case kind of
  SurrogateNoStore -> make "no-store" Nothing
  SurrogateNoStoreRemote -> make "no-store-remote" Nothing
  SurrogateMaxAge (SurrogateFreshness fresh additional) ->
    valued "max-age" (M.wordDec fresh <> if additional == 0 then mempty else M.char7 '+' <> M.wordDec additional)
  SurrogateContent features -> valued "content" (renderSurrogateFeatureSet features)
  SurrogateStaleWhileRevalidate seconds -> valued "stale-while-revalidate" (M.wordDec seconds)
  SurrogateStaleIfError seconds -> valued "stale-if-error" (M.wordDec seconds)
  SurrogateExtension name value -> make name value
  where
    make name value = case targets of
      [] -> SurrogateControlDirective name value
      first : rest -> TargetedSurrogateControlDirective name value (first :| rest)
    valued name = make name . Just . STU.fromByteStringUnsafe . M.toStrictByteString


instance KnownHeader SurrogateControl where
  type ParseFailure SurrogateControl = String
  type Cardinality SurrogateControl = 'ZeroOrOne
  type Direction SurrogateControl = 'Response


  parseFromHeaders _ fields =
    SurrogateControl <$> (parseListFieldLines "Surrogate-Control" directiveList fields >>= requireNonEmptyList "Surrogate-Control")


  renderToHeaders _ = M.toStrictByteString . renderSurrogateControl


  headerName _ = hSurrogateControl


surrogateControlParser :: ParserT st String SurrogateControl
surrogateControlParser =
  directiveList >>= either err (pure . SurrogateControl) . requireNonEmptyList "Surrogate-Control"


directiveList :: ParserT st String [SurrogateControlDirective]
directiveList = httpList directiveParser


directiveParser :: ParserT st String SurrogateControlDirective
directiveParser = do
  nameBytes <- rfc9110TokenBS
  let name = STU.fromByteStringUnsafe nameBytes
  ows
  value <- case foldedName nameBytes of
    "no-store" -> pure Nothing
    "no-store-remote" -> pure Nothing
    "max-age" -> Just <$> (equals *> shortASCIIFromParser_ maxAge)
    "content" -> Just <$> (equals *> surrogateCapabilitySetParser)
    "stale-while-revalidate" -> numeric
    "stale-if-error" -> numeric
    _ -> withOption equals (const (Just <$> (rfc9110Token <|> (ST.fromText <$> httpQuotedString)))) (pure Nothing)
  targets <- many (ows *> $(char ';') *> ows *> rfc9110Token)
  pure $ case NE.nonEmpty targets of
    Nothing -> SurrogateControlDirective name value
    Just devices -> TargetedSurrogateControlDirective name value devices
  where
    equals = $(char '=') *> ows
    numeric = Just <$> (equals *> shortASCIIFromParser_ delay)


foldedName :: ByteString -> ByteString
foldedName name
  | BSC.any (\c -> c >= 'A' && c <= 'Z') name = BSC.map toLower name
  | otherwise = name
{-# INLINE foldedName #-}


delay :: ParserT st String Word
delay = saturatingDeltaSeconds maxBound
{-# INLINE delay #-}


maxAge :: ParserT st String SurrogateControlKind
maxAge = SurrogateMaxAge <$> (SurrogateFreshness <$> delay <*> withOption $(char '+') (const delay) (pure 0))
{-# INLINE maxAge #-}


renderSurrogateControl :: SurrogateControl -> M.Builder
renderSurrogateControl (SurrogateControl (first :| rest)) =
  renderDirective first <> foldMap ((", " <>) . renderDirective) rest
{-# INLINE renderSurrogateControl #-}


renderDirective :: SurrogateControlDirective -> M.Builder
renderDirective directive =
  shortText name
    <> maybe mempty (\v -> M.char7 '=' <> renderValue v) (surrogateControlValue directive)
    <> foldMap (\target -> M.char7 ';' <> shortText target) (surrogateControlTargets directive)
  where
    name = surrogateControlName directive
    renderValue v
      | not (SBS.null bytes) && SBS.all (isTokenChar . toEnum . fromIntegral) bytes && not contentName = M.shortByteString bytes
      | SBS.all plainQuotedByte bytes = M.char7 '"' <> M.shortByteString bytes <> M.char7 '"'
      | otherwise = renderHttpQuotedString (ST.toText v)
      where
        bytes = ST.toShortByteString v
    contentName =
      let bytes = ST.toShortByteString name
      in SBS.length bytes == 7 && SBSI.unsafeIndex bytes 0 .|. 32 == 99 && foldedName (ST.toByteString name) == "content"
    plainQuotedByte byte = byte == 9 || (byte >= 32 && byte < 127 && byte /= 34 && byte /= 92)
{-# INLINE renderDirective #-}
