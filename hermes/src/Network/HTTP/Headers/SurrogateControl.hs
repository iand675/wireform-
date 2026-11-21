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

import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short as SBS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isNothing)
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Network.HTTP.ContentNegotiation (sameNegotiationToken)
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (renderHttpQuotedString, shortText)
import qualified Network.HTTP.Grammar.Rendering as R
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSurrogateControl)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.SurrogateCapability (SurrogateFeature, isSurrogateFeatureSet, parseSurrogateFeatureSet, renderSurrogateFeatureSet, surrogateCapabilitySetParser)
import Wireform.Encode.Direct (dWord8)


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
  case directiveName (surrogateControlName directive) of
    NoStoreName -> flag SurrogateNoStore
    NoStoreRemoteName -> flag SurrogateNoStoreRemote
    MaxAgeName -> argument (parseFieldLine "Surrogate-Control max-age" maxAge . ST.toByteString)
    ContentName -> argument (fmap SurrogateContent . parseSurrogateFeatureSet)
    StaleRevalidateName -> stale SurrogateStaleWhileRevalidate
    StaleErrorName -> stale SurrogateStaleIfError
    ExtensionName -> Right (SurrogateExtension (surrogateControlName directive) value)
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


  renderToHeadersChecked _ (SurrogateControl entries) =
    R.encodeCommaSeparatedChecked bound write (NE.toList entries)
    where
      width = SBS.length . ST.toShortByteString
      add = BSI.checkedAdd "Surrogate-Control"
      bound directive
        | isHttpTokenText name
        , validArgument (directiveName name) value =
            targetBound (add (width name) (maybe 0 (\v -> let n = width v in add 3 (add n n)) value))
              (surrogateControlTargets directive)
        | otherwise = -1
        where
          name = surrogateControlName directive
          value = surrogateControlValue directive
      targetBound size [] = size
      targetBound size (target : rest)
        | isHttpTokenText target = targetBound (add (add size 1) (width target)) rest
        | otherwise = -1
      validArgument NoStoreName value = isNothing value
      validArgument NoStoreRemoteName value = isNothing value
      validArgument MaxAgeName value = maybe False (\v -> isDecimalText v || isDecimalPairText 43 v) value
      validArgument ContentName value = maybe False isSurrogateFeatureSet value
      validArgument StaleRevalidateName value = maybe False isDecimalText value
      validArgument StaleErrorName value = maybe False isDecimalText value
      validArgument ExtensionName value = maybe True isHttpQuotedText value
      write ptr start directive = do
        let name = surrogateControlName directive
        nameEnd <- R.copyText ptr start name
        end <- case surrogateControlValue directive of
          Nothing -> pure nameEnd
          Just value -> do
            equals <- dWord8 ptr nameEnd 61
            if sameNegotiationToken name "content"
              then do
                opened <- dWord8 ptr equals 34
                closed <- R.copyText ptr opened value
                dWord8 ptr closed 34
              else R.writeParameterValue ptr equals value
        writeTargets ptr end (surrogateControlTargets directive)
      writeTargets _ offset [] = pure offset
      writeTargets ptr offset (target : rest) = do
        separator <- dWord8 ptr offset 59
        end <- R.copyText ptr separator target
        writeTargets ptr end rest


  headerName _ = hSurrogateControl


surrogateControlParser :: ParserT st String SurrogateControl
surrogateControlParser =
  directiveList >>= either err (pure . SurrogateControl) . requireNonEmptyList "Surrogate-Control"


directiveList :: ParserT st String [SurrogateControlDirective]
directiveList = httpList directiveParser


directiveParser :: ParserT st String SurrogateControlDirective
directiveParser = do
  name <- rfc9110Token
  ows
  value <- case directiveName name of
    NoStoreName -> pure Nothing
    NoStoreRemoteName -> pure Nothing
    MaxAgeName -> Just <$> (equals *> shortASCIIFromParser_ maxAge)
    ContentName -> Just <$> (equals *> surrogateCapabilitySetParser)
    StaleRevalidateName -> numeric
    StaleErrorName -> numeric
    ExtensionName -> withOption equals (const (Just <$> (rfc9110Token <|> (ST.fromText <$> httpQuotedString)))) (pure Nothing)
  targets <- many (ows *> $(char ';') *> ows *> rfc9110Token)
  pure $ case NE.nonEmpty targets of
    Nothing -> SurrogateControlDirective name value
    Just devices -> TargetedSurrogateControlDirective name value devices
  where
    equals = $(char '=') *> ows
    numeric = Just <$> (equals *> shortASCIIFromParser_ delay)


data DirectiveName
  = NoStoreName
  | NoStoreRemoteName
  | MaxAgeName
  | ContentName
  | StaleRevalidateName
  | StaleErrorName
  | ExtensionName


directiveName :: ST.ShortText -> DirectiveName
directiveName name = case SBS.length (ST.toShortByteString name) of
  7
    | sameNegotiationToken name "max-age" -> MaxAgeName
    | sameNegotiationToken name "content" -> ContentName
  8 | sameNegotiationToken name "no-store" -> NoStoreName
  14 | sameNegotiationToken name "stale-if-error" -> StaleErrorName
  15 | sameNegotiationToken name "no-store-remote" -> NoStoreRemoteName
  22 | sameNegotiationToken name "stale-while-revalidate" -> StaleRevalidateName
  _ -> ExtensionName
{-# INLINE directiveName #-}


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
      | isHttpTokenText v && not (sameNegotiationToken name "content") = M.shortByteString bytes
      | SBS.all plainQuotedByte bytes = M.char7 '"' <> M.shortByteString bytes <> M.char7 '"'
      | otherwise = renderHttpQuotedString (ST.toText v)
      where
        bytes = ST.toShortByteString v
    plainQuotedByte byte = byte == 9 || (byte >= 32 && byte < 127 && byte /= 34 && byte /= 92)
{-# INLINE renderDirective #-}
