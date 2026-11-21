{- |
Module      : Network.HTTP.Headers.CDNCacheControl
Description : The @CDN-Cache-Control@ response header

The @CDN-Cache-Control@ header field is a targeted cache-control header,
allowing an origin to direct content delivery network (CDN) caches without
affecting other caches (such as browser caches) on the delivery path. It uses
the same directive vocabulary as 'Cache-Control'. RFC 9213 defines its
wire format as a Structured Fields dictionary.

See <https://datatracker.ietf.org/doc/html/rfc9213> for the official
specification (RFC 9213: Targeted HTTP Cache Control).

See also: "Network.HTTP.Headers.CacheControl", "Network.HTTP.Headers.SurrogateControl", "Network.HTTP.Headers.CacheStatus", "Network.HTTP.Headers.CDNLoop".
-}
module Network.HTTP.Headers.CDNCacheControl (
  CDNCacheControl (CDNCacheControl, CDNCacheControlEmpty, cdnCacheControlDirectives),
  cdnCacheControlDirectiveList,
  cdnCacheControlDictionary,
  cdnCacheControlParser,
  renderCDNCacheControl,
  renderCDNCacheControlChecked,
) where

import qualified Data.List.NonEmpty as NE
import Data.Text.Short (ShortText)
import qualified Network.HTTP.Grammar.Rendering as R
import Network.HTTP.Headers
import Network.HTTP.Headers.CacheControl (CacheControlDirective (..), cacheDirectiveFieldNames)
import Network.HTTP.Headers.HeaderFieldName (hCDNCacheControl)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


{- | A targeted cache policy. Use 'cdnCacheControlDirectiveList' to read
the understood directives in every case. The record selector applies to
values using the directive constructor. Parsed extension entries and
parameters are available through 'cdnCacheControlDictionary'.
-}
data CDNCacheControl
  = CDNCacheControl {cdnCacheControlDirectives :: !(NE.NonEmpty CacheControlDirective)}
  | CDNCacheControlEmpty
  | CDNCacheControlStructured ![StructuredFieldDictionaryEntry] ![CacheControlDirective]
  deriving stock (Eq, Show)


instance KnownHeader CDNCacheControl where
  type ParseFailure CDNCacheControl = String
  type Cardinality CDNCacheControl = 'ZeroOrMore
  type Direction CDNCacheControl = 'Response


  parseFromHeaders _ values =
    parseStructuredFieldDictionary "CDN-Cache-Control" values >>= fromDictionary
  renderToHeaders _ CDNCacheControlEmpty = []
  renderToHeaders _ value = either (const []) (pure . M.toStrictByteString) (renderCDNCacheControlChecked value)


  omitHeaderValue CDNCacheControlEmpty = True
  omitHeaderValue _ = False
  {-# INLINE omitHeaderValue #-}


  headerName _ = hCDNCacheControl


cdnCacheControlParser :: ParserT st e CDNCacheControl
cdnCacheControlParser = embedError parser (const failed)
  where
    parser = structuredFieldDictionary >>= either err pure . fromDictionary


-- | Project understood cache directives. Member parameters are ignored.
cdnCacheControlDirectiveList :: CDNCacheControl -> [CacheControlDirective]
cdnCacheControlDirectiveList CDNCacheControlEmpty = []
cdnCacheControlDirectiveList (CDNCacheControl ds) = NE.toList ds
cdnCacheControlDirectiveList (CDNCacheControlStructured _ directives) = directives


-- | The dictionary, in key order, with any extension entries and parameters.
cdnCacheControlDictionary :: CDNCacheControl -> Either String [StructuredFieldDictionaryEntry]
cdnCacheControlDictionary CDNCacheControlEmpty = Right []
cdnCacheControlDictionary (CDNCacheControlStructured entries _) = Right entries
cdnCacheControlDictionary (CDNCacheControl ds) =
  traverse directiveEntry (orderedFieldEntries directiveKey (candidates (NE.toList ds)))
  where
    candidates [] = []
    candidates (directive : rest) = case fromDirective directive of
      candidate@DirectiveCandidate {} -> candidate : candidates rest


renderCDNCacheControl :: CDNCacheControl -> M.Builder
renderCDNCacheControl = either (const mempty) id . renderCDNCacheControlChecked


-- | Validate the directive constructor before writing a dictionary.
renderCDNCacheControlChecked :: CDNCacheControl -> Either String M.Builder
renderCDNCacheControlChecked = fmap R.renderStructuredFieldDictionary . cdnCacheControlDictionary


fromDictionary :: [StructuredFieldDictionaryEntry] -> Either String CDNCacheControl
fromDictionary [] = Right CDNCacheControlEmpty
fromDictionary entries = collect entries [] False
  where
    collect [] reversed retainDictionary =
      let directives = reverse reversed
      in Right $! case NE.nonEmpty directives of
           Just known | not retainDictionary -> CDNCacheControl known
           _ -> CDNCacheControlStructured entries directives
    collect (next : rest) reversed retainDictionary = case toDirective next of
      Left err -> Left err
      Right Nothing -> collect rest reversed True
      Right (Just directive) -> collect rest (directive : reversed) (retainDictionary || not (unparameterized next))
    unparameterized (StructuredFieldDictionaryEntry _ (StructuredItem (StructuredFieldItem _ []))) = True
    unparameterized _ = False


toDirective :: StructuredFieldDictionaryEntry -> Either String (Maybe CacheControlDirective)
toDirective (StructuredFieldDictionaryEntry key member) =
  case key of
    "max-age" -> delay MaxAge
    "s-maxage" -> delay SMaxAge
    "stale-while-revalidate" -> delay StaleWhileRevalidate
    "stale-if-error" -> delay StaleIfError
    "private" -> qualifiedNames Private
    "no-cache" -> qualifiedNames NoCache
    "public" -> flag Public
    "no-store" -> flag NoStore
    "must-revalidate" -> flag MustRevalidate
    "must-understand" -> flag MustUnderstand
    "proxy-revalidate" -> flag ProxyRevalidate
    "no-transform" -> flag NoTransform
    "immutable" -> flag Immutable
    _ -> Right Nothing
  where
    invalid = Left ("Invalid CDN-Cache-Control value for " <> show key)
    flag directive = case member of
      StructuredItem (StructuredFieldItem (Boolean True) _) -> Right (Just directive)
      _ -> invalid
    delay make = case member of
      StructuredItem (StructuredFieldItem (Integer n) _) | n >= 0 -> Right (Just (make (fromIntegral n)))
      _ -> invalid
    qualifiedNames make = case member of
      StructuredItem (StructuredFieldItem (Boolean True) _) -> Right (Just (make Nothing))
      StructuredItem (StructuredFieldItem (String (RFC8941String value)) _) -> do
        let directive = make (Just value)
        case cacheDirectiveFieldNames directive of
          Just _ -> Right (Just directive)
          Nothing -> invalid
      _ -> invalid


-- Delay validation of each value until dictionary replacement has selected it.
data DirectiveCandidate = DirectiveCandidate
  { directiveKey :: !ShortText
  , directiveEntry :: Either String StructuredFieldDictionaryEntry
  }


fromDirective :: CacheControlDirective -> DirectiveCandidate
fromDirective directive = case directive of
  Public -> flag "public"
  Private val -> qualifiedNames "private" val
  NoCache val -> qualifiedNames "no-cache" val
  NoStore -> flag "no-store"
  MaxAge n -> delay "max-age" n
  MaxStale _ -> requestOnly "max-stale"
  MinFresh _ -> requestOnly "min-fresh"
  SMaxAge n -> delay "s-maxage" n
  MustRevalidate -> flag "must-revalidate"
  MustUnderstand -> flag "must-understand"
  ProxyRevalidate -> flag "proxy-revalidate"
  NoTransform -> flag "no-transform"
  OnlyIfCached -> requestOnly "only-if-cached"
  Immutable -> flag "immutable"
  StaleWhileRevalidate n -> delay "stale-while-revalidate" n
  StaleIfError n -> delay "stale-if-error" n
  Unknown key val -> DirectiveCandidate key validateExtension
    where
      validateExtension
        | isStructuredFieldKey key = do
            member <- entry key <$> extension val
            member <$ toDirective member
        | otherwise = Left ("Invalid CDN-Cache-Control dictionary key " <> show key)
  where
    requestOnly key = DirectiveCandidate key (Left "CDN-Cache-Control requires a response cache directive")
    flag key = DirectiveCandidate key (Right (entry key (Boolean True)))
    delay key n = DirectiveCandidate key (delayEntry key n)
    delayEntry key n
      | n <= 999_999_999_999_999 = Right (entry key (Integer (fromIntegral n)))
      | otherwise = Left ("CDN-Cache-Control integer out of range for " <> show key)
    qualifiedNames key Nothing = flag key
    qualifiedNames key (Just value) = DirectiveCandidate key $ case cacheDirectiveFieldNames directive of
      Just _ -> entry key . String <$> stringValue value
      Nothing -> Left ("Invalid CDN-Cache-Control field names for " <> show key)
    extension Nothing = Right (Boolean True)
    extension (Just value) = case mkRFC8941Token value of
      Just token -> Right (Token token)
      Nothing -> String <$> stringValue value


entry :: ShortText -> ItemValue -> StructuredFieldDictionaryEntry
entry key value = StructuredFieldDictionaryEntry key (StructuredItem (StructuredFieldItem value []))


stringValue :: ShortText -> Either String RFC8941String
stringValue text
  | isStructuredFieldString string = Right string
  | otherwise = Left "CDN-Cache-Control requires an ASCII Structured Field string"
  where
    string = RFC8941String text
