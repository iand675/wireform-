{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}
module HTML.TagId
  ( TagId(..)
  , tagIdFromText
  , tagIdFromBS
  , internTagBS
  , internTagAddr
  , internTagAddrU
  , fastTagIdAddr
  , internAttrNameBS
  , internAttrNameRange
  , tagIdToText
  , tagIdIsSpecial
  , tagIdIsFormatting
  , tagIdIsHeading
  , tagIdIsImpliedEndTag
  , tagIdIsDefaultScopeTerminator
  , tagIdIsButtonScopeTerminator
  , tagIdIsListItemScopeTerminator
  , tagIdIsDefinitionScopeTerminator
  , tagIdIsTableScopeTerminator
  , tagIdIsForeignBreakout
  , tagIdIsVoid
  , tagIdIsRawText
  ) where

import Data.Word (Word8)
import GHC.Exts (Addr#, indexWord8OffAddr#, Int(..), Int#)
import GHC.Word (Word8(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.HashMap.Strict as HM

data TagId
  = TagA | TagAbbr | TagAddress | TagApplet | TagArea | TagArticle | TagAside
  | TagB | TagBase | TagBasefont | TagBgsound | TagBig | TagBlockquote | TagBody
  | TagBr | TagButton
  | TagCaption | TagCenter | TagCode | TagCol | TagColgroup
  | TagDd | TagDetails | TagDialog | TagDir | TagDiv | TagDl | TagDt
  | TagEm | TagEmbed
  | TagFieldset | TagFigcaption | TagFigure | TagFont | TagFooter | TagForm
  | TagFrame | TagFrameset
  | TagH1 | TagH2 | TagH3 | TagH4 | TagH5 | TagH6
  | TagHead | TagHeader | TagHgroup | TagHr | TagHtml
  | TagI | TagIframe | TagImage | TagImg | TagInput
  | TagKeygen
  | TagLi | TagLink | TagListing
  | TagMain | TagMarquee | TagMenu | TagMenuitem | TagMeta
  | TagNav | TagNobr | TagNoembed | TagNoframes | TagNoscript
  | TagObject | TagOl | TagOptgroup | TagOption
  | TagP | TagParam | TagPlaintext | TagPre
  | TagRb | TagRp | TagRt | TagRtc | TagRuby
  | TagS | TagScript | TagSearch | TagSection | TagSelect | TagSmall
  | TagSource | TagSpan | TagStrike | TagStrong | TagStyle | TagSub
  | TagSummary | TagSup
  | TagSvg | TagMath
  | TagTable | TagTbody | TagTd | TagTemplate | TagTextarea | TagTfoot
  | TagTh | TagThead | TagTitle | TagTr | TagTrack | TagTt
  | TagU | TagUl
  | TagVar | TagWbr
  | TagXmp
  | TagUnknown
  deriving (Eq, Ord, Enum, Bounded, Show)

{-# NOINLINE tagMap #-}
tagMap :: HM.HashMap Text TagId
tagMap = HM.fromList
  [ ("a", TagA), ("abbr", TagAbbr), ("address", TagAddress), ("applet", TagApplet)
  , ("area", TagArea), ("article", TagArticle), ("aside", TagAside)
  , ("b", TagB), ("base", TagBase), ("basefont", TagBasefont), ("bgsound", TagBgsound)
  , ("big", TagBig), ("blockquote", TagBlockquote), ("body", TagBody)
  , ("br", TagBr), ("button", TagButton)
  , ("caption", TagCaption), ("center", TagCenter), ("code", TagCode)
  , ("col", TagCol), ("colgroup", TagColgroup)
  , ("dd", TagDd), ("details", TagDetails), ("dialog", TagDialog)
  , ("dir", TagDir), ("div", TagDiv), ("dl", TagDl), ("dt", TagDt)
  , ("em", TagEm), ("embed", TagEmbed)
  , ("fieldset", TagFieldset), ("figcaption", TagFigcaption), ("figure", TagFigure)
  , ("font", TagFont), ("footer", TagFooter), ("form", TagForm)
  , ("frame", TagFrame), ("frameset", TagFrameset)
  , ("h1", TagH1), ("h2", TagH2), ("h3", TagH3), ("h4", TagH4)
  , ("h5", TagH5), ("h6", TagH6)
  , ("head", TagHead), ("header", TagHeader), ("hgroup", TagHgroup)
  , ("hr", TagHr), ("html", TagHtml)
  , ("i", TagI), ("iframe", TagIframe), ("image", TagImage), ("img", TagImg)
  , ("input", TagInput)
  , ("keygen", TagKeygen)
  , ("li", TagLi), ("link", TagLink), ("listing", TagListing)
  , ("main", TagMain), ("marquee", TagMarquee), ("menu", TagMenu)
  , ("menuitem", TagMenuitem), ("meta", TagMeta)
  , ("nav", TagNav), ("nobr", TagNobr), ("noembed", TagNoembed)
  , ("noframes", TagNoframes), ("noscript", TagNoscript)
  , ("object", TagObject), ("ol", TagOl), ("optgroup", TagOptgroup)
  , ("option", TagOption)
  , ("p", TagP), ("param", TagParam), ("plaintext", TagPlaintext), ("pre", TagPre)
  , ("rb", TagRb), ("rp", TagRp), ("rt", TagRt), ("rtc", TagRtc), ("ruby", TagRuby)
  , ("s", TagS), ("script", TagScript), ("search", TagSearch)
  , ("section", TagSection), ("select", TagSelect), ("small", TagSmall)
  , ("source", TagSource), ("span", TagSpan), ("strike", TagStrike)
  , ("strong", TagStrong), ("style", TagStyle), ("sub", TagSub)
  , ("summary", TagSummary), ("sup", TagSup)
  , ("svg", TagSvg), ("math", TagMath)
  , ("table", TagTable), ("tbody", TagTbody), ("td", TagTd)
  , ("template", TagTemplate), ("textarea", TagTextarea), ("tfoot", TagTfoot)
  , ("th", TagTh), ("thead", TagThead), ("title", TagTitle), ("tr", TagTr)
  , ("track", TagTrack), ("tt", TagTt)
  , ("u", TagU), ("ul", TagUl)
  , ("var", TagVar), ("wbr", TagWbr)
  , ("xmp", TagXmp)
  ]

{-# INLINE tagIdFromText #-}
tagIdFromText :: Text -> TagId
tagIdFromText !t = case HM.lookup t tagMap of
  Just tid -> tid
  Nothing  -> TagUnknown

{-# INLINE tagIdFromBS #-}
tagIdFromBS :: ByteString -> TagId
tagIdFromBS = tagIdFromText . TE.decodeUtf8Lenient

{-# NOINLINE tagMapBS #-}
tagMapBS :: HM.HashMap ByteString (Text, TagId)
tagMapBS = HM.fromList
  [(TE.encodeUtf8 t, (t, tid)) | (t, tid) <- HM.toList tagMap]

{-# INLINE internTagBS #-}
internTagBS :: ByteString -> (Text, TagId)
internTagBS !rawName =
  let !len = BS.length rawName
  in case fastTagLookup rawName len of
    Just pair -> pair
    Nothing -> slowInternTagBS rawName

{-# INLINE internTagAddr #-}
internTagAddr :: Addr# -> Int -> Int -> ByteString -> (Text, TagId)
internTagAddr addr# !off !tagLen !origBS =
  case fastTagLookupAddr addr# off tagLen of
    Just pair -> pair
    Nothing -> slowInternTagBS (BSU.unsafeTake tagLen (BSU.unsafeDrop off origBS))

{-# INLINE internTagAddrU #-}
internTagAddrU :: Addr# -> Int -> Int -> ByteString -> (# Text, TagId #)
internTagAddrU addr# !off !tagLen !origBS =
  case fastTagLookupAddr addr# off tagLen of
    Just (!t, !tid) -> (# t, tid #)
    Nothing -> case slowInternTagBS (BSU.unsafeTake tagLen (BSU.unsafeDrop off origBS)) of
      (!t, !tid) -> (# t, tid #)

{-# INLINE fastTagIdAddr #-}
fastTagIdAddr :: Addr# -> Int -> Int -> ByteString -> TagId
fastTagIdAddr addr# !off !tagLen !origBS =
  case fastTagLookupAddr addr# off tagLen of
    Just (_, !tid) -> tid
    Nothing -> case slowInternTagBS (BSU.unsafeTake tagLen (BSU.unsafeDrop off origBS)) of
      (_, !tid) -> tid

slowInternTagBS :: ByteString -> (Text, TagId)
slowInternTagBS !rawName =
  case HM.lookup rawName tagMapBS of
    Just !pair -> pair
    Nothing
      | BS.any (\b -> b >= 0x41 && b <= 0x5A) rawName ->
          let !lowered = BS.map asciiToLower rawName
          in case HM.lookup lowered tagMapBS of
               Just !pair -> pair
               Nothing -> (TE.decodeUtf8Lenient lowered, TagUnknown)
      | otherwise -> (TE.decodeUtf8Lenient rawName, TagUnknown)
  where
    asciiToLower :: Word8 -> Word8
    asciiToLower b
      | b >= 0x41, b <= 0x5A = b + 32
      | otherwise = b
{-# NOINLINE slowInternTagBS #-}

{-# INLINE fastTagLookup #-}
fastTagLookup :: ByteString -> Int -> Maybe (Text, TagId)
fastTagLookup !bs !len = case len of
  1 -> case BSU.unsafeIndex bs 0 of
    0x70 {- p -} -> Just ("p", TagP)
    0x61 {- a -} -> Just ("a", TagA)
    0x62 {- b -} -> Just ("b", TagB)
    0x69 {- i -} -> Just ("i", TagI)
    0x73 {- s -} -> Just ("s", TagS)
    0x75 {- u -} -> Just ("u", TagU)
    _ -> Nothing
  2 -> case BSU.unsafeIndex bs 0 of
    0x62 {- br -} | BSU.unsafeIndex bs 1 == 0x72 -> Just ("br", TagBr)
    0x64 {- dl/dd/dt -} -> case BSU.unsafeIndex bs 1 of
      0x6C -> Just ("dl", TagDl); 0x64 -> Just ("dd", TagDd); 0x74 -> Just ("dt", TagDt)
      _ -> Nothing
    0x65 {- em -} | BSU.unsafeIndex bs 1 == 0x6D -> Just ("em", TagEm)
    0x68 {- h1-h6 -} -> case BSU.unsafeIndex bs 1 of
      0x31 -> Just ("h1", TagH1); 0x32 -> Just ("h2", TagH2); 0x33 -> Just ("h3", TagH3)
      0x34 -> Just ("h4", TagH4); 0x35 -> Just ("h5", TagH5); 0x36 -> Just ("h6", TagH6)
      0x72 -> Just ("hr", TagHr)
      _ -> Nothing
    0x6C {- li -} | BSU.unsafeIndex bs 1 == 0x69 -> Just ("li", TagLi)
    0x6F {- ol -} | BSU.unsafeIndex bs 1 == 0x6C -> Just ("ol", TagOl)
    0x72 {- rb/rp/rt -} -> case BSU.unsafeIndex bs 1 of
      0x62 -> Just ("rb", TagRb); 0x70 -> Just ("rp", TagRp); 0x74 -> Just ("rt", TagRt)
      _ -> Nothing
    0x74 {- td/th/tr/tt -} -> case BSU.unsafeIndex bs 1 of
      0x64 -> Just ("td", TagTd); 0x68 -> Just ("th", TagTh)
      0x72 -> Just ("tr", TagTr); 0x74 -> Just ("tt", TagTt)
      _ -> Nothing
    0x75 {- ul -} | BSU.unsafeIndex bs 1 == 0x6C -> Just ("ul", TagUl)
    _ -> Nothing
  3 -> case BSU.unsafeIndex bs 0 of
    0x62 {- big -} | BSU.unsafeIndex bs 1 == 0x69 && BSU.unsafeIndex bs 2 == 0x67 -> Just ("big", TagBig)
    0x63 {- col -} | BSU.unsafeIndex bs 1 == 0x6F && BSU.unsafeIndex bs 2 == 0x6C -> Just ("col", TagCol)
    0x64 {- div/dir -} -> case BSU.unsafeIndex bs 1 of
      0x69 -> case BSU.unsafeIndex bs 2 of
        0x76 -> Just ("div", TagDiv); 0x72 -> Just ("dir", TagDir); _ -> Nothing
      _ -> Nothing
    0x69 {- img -} | BSU.unsafeIndex bs 1 == 0x6D && BSU.unsafeIndex bs 2 == 0x67 -> Just ("img", TagImg)
    0x6E {- nav -} | BSU.unsafeIndex bs 1 == 0x61 && BSU.unsafeIndex bs 2 == 0x76 -> Just ("nav", TagNav)
    0x70 {- pre -} | BSU.unsafeIndex bs 1 == 0x72 && BSU.unsafeIndex bs 2 == 0x65 -> Just ("pre", TagPre)
    0x72 {- rtc -} | BSU.unsafeIndex bs 1 == 0x74 && BSU.unsafeIndex bs 2 == 0x63 -> Just ("rtc", TagRtc)
    0x73 {- sub/sup/svg -} -> case BSU.unsafeIndex bs 1 of
      0x75 -> case BSU.unsafeIndex bs 2 of
        0x62 -> Just ("sub", TagSub); 0x70 -> Just ("sup", TagSup)
        _ -> Nothing
      0x76 | BSU.unsafeIndex bs 2 == 0x67 -> Just ("svg", TagSvg)
      _ -> Nothing
    0x76 {- var -} | BSU.unsafeIndex bs 1 == 0x61 && BSU.unsafeIndex bs 2 == 0x72 -> Just ("var", TagVar)
    0x77 {- wbr -} | BSU.unsafeIndex bs 1 == 0x62 && BSU.unsafeIndex bs 2 == 0x72 -> Just ("wbr", TagWbr)
    0x78 {- xmp -} | BSU.unsafeIndex bs 1 == 0x6D && BSU.unsafeIndex bs 2 == 0x70 -> Just ("xmp", TagXmp)
    _ -> Nothing
  4 -> case BSU.unsafeIndex bs 0 of
    0x61 {- area -} | bs4eq bs 0x61 0x72 0x65 0x61 -> Just ("area", TagArea)
    0x62 {- base/body -} -> case BSU.unsafeIndex bs 1 of
      0x61 | bs4eq bs 0x62 0x61 0x73 0x65 -> Just ("base", TagBase)
      0x6F | bs4eq bs 0x62 0x6F 0x64 0x79 -> Just ("body", TagBody)
      _ -> Nothing
    0x63 {- code -} | bs4eq bs 0x63 0x6F 0x64 0x65 -> Just ("code", TagCode)
    0x66 {- font/form -} -> case BSU.unsafeIndex bs 1 of
      0x6F -> case BSU.unsafeIndex bs 2 of
        0x6E | BSU.unsafeIndex bs 3 == 0x74 -> Just ("font", TagFont)
        0x72 | BSU.unsafeIndex bs 3 == 0x6D -> Just ("form", TagForm)
        _ -> Nothing
      _ -> Nothing
    0x68 {- head/html -} -> case BSU.unsafeIndex bs 1 of
      0x65 | bs4eq bs 0x68 0x65 0x61 0x64 -> Just ("head", TagHead)
      0x74 | bs4eq bs 0x68 0x74 0x6D 0x6C -> Just ("html", TagHtml)
      _ -> Nothing
    0x6C {- link -} | bs4eq bs 0x6C 0x69 0x6E 0x6B -> Just ("link", TagLink)
    0x6D {- main/math/menu/meta -} -> case BSU.unsafeIndex bs 1 of
      0x61 -> case BSU.unsafeIndex bs 2 of
        0x69 | BSU.unsafeIndex bs 3 == 0x6E -> Just ("main", TagMain)
        0x74 | BSU.unsafeIndex bs 3 == 0x68 -> Just ("math", TagMath)
        _ -> Nothing
      0x65 -> case BSU.unsafeIndex bs 2 of
        0x6E | BSU.unsafeIndex bs 3 == 0x75 -> Just ("menu", TagMenu)
        0x74 | BSU.unsafeIndex bs 3 == 0x61 -> Just ("meta", TagMeta)
        _ -> Nothing
      _ -> Nothing
    0x6E {- nobr -} | bs4eq bs 0x6E 0x6F 0x62 0x72 -> Just ("nobr", TagNobr)
    0x72 {- ruby -} | bs4eq bs 0x72 0x75 0x62 0x79 -> Just ("ruby", TagRuby)
    0x73 {- span -} | bs4eq bs 0x73 0x70 0x61 0x6E -> Just ("span", TagSpan)
    _ -> Nothing
  5 -> case BSU.unsafeIndex bs 0 of
    0x65 {- embed -} | bsEq bs "embed" -> Just ("embed", TagEmbed)
    0x69 {- image/input -} -> case BSU.unsafeIndex bs 1 of
      0x6D | bsEq bs "image" -> Just ("image", TagImage)
      0x6E | bsEq bs "input" -> Just ("input", TagInput)
      _ -> Nothing
    0x6C {- label -} | bsEq bs "label" -> Nothing
    0x73 {- small/style -} -> case BSU.unsafeIndex bs 1 of
      0x6D | bsEq bs "small" -> Just ("small", TagSmall)
      0x74 | bsEq bs "style" -> Just ("style", TagStyle)
      _ -> Nothing
    0x74 {- table/tbody/tfoot/thead/title/track -} -> case BSU.unsafeIndex bs 1 of
      0x61 | bsEq bs "table" -> Just ("table", TagTable)
      0x62 | bsEq bs "tbody" -> Just ("tbody", TagTbody)
      0x66 | bsEq bs "tfoot" -> Just ("tfoot", TagTfoot)
      0x68 | bsEq bs "thead" -> Just ("thead", TagThead)
      0x69 | bsEq bs "title" -> Just ("title", TagTitle)
      0x72 | bsEq bs "track" -> Just ("track", TagTrack)
      _ -> Nothing
    _ -> Nothing
  6 -> case BSU.unsafeIndex bs 0 of
    0x62 {- button -} | bsEq bs "button" -> Just ("button", TagButton)
    0x63 {- center -} | bsEq bs "center" -> Just ("center", TagCenter)
    0x64 {- dialog -} | bsEq bs "dialog" -> Just ("dialog", TagDialog)
    0x66 {- figure/footer -} -> case BSU.unsafeIndex bs 1 of
      0x69 | bsEq bs "figure" -> Just ("figure", TagFigure)
      0x6F | bsEq bs "footer" -> Just ("footer", TagFooter)
      _ -> Nothing
    0x68 {- header/hgroup -} -> case BSU.unsafeIndex bs 1 of
      0x65 | bsEq bs "header" -> Just ("header", TagHeader)
      0x67 | bsEq bs "hgroup" -> Just ("hgroup", TagHgroup)
      _ -> Nothing
    0x69 {- iframe -} | bsEq bs "iframe" -> Just ("iframe", TagIframe)
    0x6F {- object/option -} -> case BSU.unsafeIndex bs 1 of
      0x62 | bsEq bs "object" -> Just ("object", TagObject)
      0x70 | bsEq bs "option" -> Just ("option", TagOption)
      _ -> Nothing
    0x73 {- script/search/select/source/strike/strong -} -> case BSU.unsafeIndex bs 1 of
      0x63 | bsEq bs "script" -> Just ("script", TagScript)
      0x65 -> case BSU.unsafeIndex bs 2 of
        0x61 | bsEq bs "search" -> Just ("search", TagSearch)
        0x6C | bsEq bs "select" -> Just ("select", TagSelect)
        _ -> Nothing
      0x6F | bsEq bs "source" -> Just ("source", TagSource)
      0x74 -> case BSU.unsafeIndex bs 2 of
        0x72 | bsEq bs "strike" -> Just ("strike", TagStrike)
        _ -> Nothing
      _ -> Nothing
    _ -> Nothing
  _ -> Nothing
  where
    {-# INLINE bs4eq #-}
    bs4eq b a0 a1 a2 a3 =
      BSU.unsafeIndex b 0 == a0 && BSU.unsafeIndex b 1 == a1 &&
      BSU.unsafeIndex b 2 == a2 && BSU.unsafeIndex b 3 == a3
    {-# INLINE bsEq #-}
    bsEq b expected = b == expected

{-# INLINE fastTagLookupAddr #-}
fastTagLookupAddr :: Addr# -> Int -> Int -> Maybe (Text, TagId)
fastTagLookupAddr addr# !off !tagLen =
  let {-# INLINE b #-}
      b :: Int -> Word8
      b i = W8# (indexWord8OffAddr# addr# i#) where !(I# i#) = off + i
      {-# INLINE b4eq #-}
      b4eq a0 a1 a2 a3 = b 0 == a0 && b 1 == a1 && b 2 == a2 && b 3 == a3
      {-# INLINE bsEqAddr #-}
      bsEqAddr expected = tagLen == BS.length expected && goEq 0
        where
          goEq !i
            | i >= tagLen = True
            | otherwise = b i == BSU.unsafeIndex expected i && goEq (i + 1)
  in case tagLen of
    1 -> case b 0 of
      0x70 {- p -} -> Just ("p", TagP)
      0x61 {- a -} -> Just ("a", TagA)
      0x62 {- b -} -> Just ("b", TagB)
      0x69 {- i -} -> Just ("i", TagI)
      0x73 {- s -} -> Just ("s", TagS)
      0x75 {- u -} -> Just ("u", TagU)
      _ -> Nothing
    2 -> case b 0 of
      0x62 {- br -} | b 1 == 0x72 -> Just ("br", TagBr)
      0x64 {- dl/dd/dt -} -> case b 1 of
        0x6C -> Just ("dl", TagDl); 0x64 -> Just ("dd", TagDd); 0x74 -> Just ("dt", TagDt)
        _ -> Nothing
      0x65 {- em -} | b 1 == 0x6D -> Just ("em", TagEm)
      0x68 {- h1-h6 -} -> case b 1 of
        0x31 -> Just ("h1", TagH1); 0x32 -> Just ("h2", TagH2); 0x33 -> Just ("h3", TagH3)
        0x34 -> Just ("h4", TagH4); 0x35 -> Just ("h5", TagH5); 0x36 -> Just ("h6", TagH6)
        0x72 -> Just ("hr", TagHr)
        _ -> Nothing
      0x6C {- li -} | b 1 == 0x69 -> Just ("li", TagLi)
      0x6F {- ol -} | b 1 == 0x6C -> Just ("ol", TagOl)
      0x72 {- rb/rp/rt -} -> case b 1 of
        0x62 -> Just ("rb", TagRb); 0x70 -> Just ("rp", TagRp); 0x74 -> Just ("rt", TagRt)
        _ -> Nothing
      0x74 {- td/th/tr/tt -} -> case b 1 of
        0x64 -> Just ("td", TagTd); 0x68 -> Just ("th", TagTh)
        0x72 -> Just ("tr", TagTr); 0x74 -> Just ("tt", TagTt)
        _ -> Nothing
      0x75 {- ul -} | b 1 == 0x6C -> Just ("ul", TagUl)
      _ -> Nothing
    3 -> case b 0 of
      0x62 {- big -} | b 1 == 0x69 && b 2 == 0x67 -> Just ("big", TagBig)
      0x63 {- col -} | b 1 == 0x6F && b 2 == 0x6C -> Just ("col", TagCol)
      0x64 {- div/dir -} -> case b 1 of
        0x69 -> case b 2 of
          0x76 -> Just ("div", TagDiv); 0x72 -> Just ("dir", TagDir); _ -> Nothing
        _ -> Nothing
      0x69 {- img -} | b 1 == 0x6D && b 2 == 0x67 -> Just ("img", TagImg)
      0x6E {- nav -} | b 1 == 0x61 && b 2 == 0x76 -> Just ("nav", TagNav)
      0x70 {- pre -} | b 1 == 0x72 && b 2 == 0x65 -> Just ("pre", TagPre)
      0x72 {- rtc -} | b 1 == 0x74 && b 2 == 0x63 -> Just ("rtc", TagRtc)
      0x73 {- sub/sup/svg -} -> case b 1 of
        0x75 -> case b 2 of
          0x62 -> Just ("sub", TagSub); 0x70 -> Just ("sup", TagSup)
          _ -> Nothing
        0x76 | b 2 == 0x67 -> Just ("svg", TagSvg)
        _ -> Nothing
      0x76 {- var -} | b 1 == 0x61 && b 2 == 0x72 -> Just ("var", TagVar)
      0x77 {- wbr -} | b 1 == 0x62 && b 2 == 0x72 -> Just ("wbr", TagWbr)
      0x78 {- xmp -} | b 1 == 0x6D && b 2 == 0x70 -> Just ("xmp", TagXmp)
      _ -> Nothing
    4 -> case b 0 of
      0x61 {- area -} | b4eq 0x61 0x72 0x65 0x61 -> Just ("area", TagArea)
      0x62 {- base/body -} -> case b 1 of
        0x61 | b4eq 0x62 0x61 0x73 0x65 -> Just ("base", TagBase)
        0x6F | b4eq 0x62 0x6F 0x64 0x79 -> Just ("body", TagBody)
        _ -> Nothing
      0x63 {- code -} | b4eq 0x63 0x6F 0x64 0x65 -> Just ("code", TagCode)
      0x66 {- font/form -} -> case b 1 of
        0x6F -> case b 2 of
          0x6E | b 3 == 0x74 -> Just ("font", TagFont)
          0x72 | b 3 == 0x6D -> Just ("form", TagForm)
          _ -> Nothing
        _ -> Nothing
      0x68 {- head/html -} -> case b 1 of
        0x65 | b4eq 0x68 0x65 0x61 0x64 -> Just ("head", TagHead)
        0x74 | b4eq 0x68 0x74 0x6D 0x6C -> Just ("html", TagHtml)
        _ -> Nothing
      0x6C {- link -} | b4eq 0x6C 0x69 0x6E 0x6B -> Just ("link", TagLink)
      0x6D {- main/math/menu/meta -} -> case b 1 of
        0x61 -> case b 2 of
          0x69 | b 3 == 0x6E -> Just ("main", TagMain)
          0x74 | b 3 == 0x68 -> Just ("math", TagMath)
          _ -> Nothing
        0x65 -> case b 2 of
          0x6E | b 3 == 0x75 -> Just ("menu", TagMenu)
          0x74 | b 3 == 0x61 -> Just ("meta", TagMeta)
          _ -> Nothing
        _ -> Nothing
      0x6E {- nobr -} | b4eq 0x6E 0x6F 0x62 0x72 -> Just ("nobr", TagNobr)
      0x72 {- ruby -} | b4eq 0x72 0x75 0x62 0x79 -> Just ("ruby", TagRuby)
      0x73 {- span -} | b4eq 0x73 0x70 0x61 0x6E -> Just ("span", TagSpan)
      _ -> Nothing
    5 -> case b 0 of
      0x65 {- embed -} | bsEqAddr "embed" -> Just ("embed", TagEmbed)
      0x69 {- image/input -} -> case b 1 of
        0x6D | bsEqAddr "image" -> Just ("image", TagImage)
        0x6E | bsEqAddr "input" -> Just ("input", TagInput)
        _ -> Nothing
      0x6C {- label -} | bsEqAddr "label" -> Nothing
      0x73 {- small/style -} -> case b 1 of
        0x6D | bsEqAddr "small" -> Just ("small", TagSmall)
        0x74 | bsEqAddr "style" -> Just ("style", TagStyle)
        _ -> Nothing
      0x74 {- table/tbody/tfoot/thead/title/track -} -> case b 1 of
        0x61 | bsEqAddr "table" -> Just ("table", TagTable)
        0x62 | bsEqAddr "tbody" -> Just ("tbody", TagTbody)
        0x66 | bsEqAddr "tfoot" -> Just ("tfoot", TagTfoot)
        0x68 | bsEqAddr "thead" -> Just ("thead", TagThead)
        0x69 | bsEqAddr "title" -> Just ("title", TagTitle)
        0x72 | bsEqAddr "track" -> Just ("track", TagTrack)
        _ -> Nothing
      _ -> Nothing
    6 -> case b 0 of
      0x62 {- button -} | bsEqAddr "button" -> Just ("button", TagButton)
      0x63 {- center -} | bsEqAddr "center" -> Just ("center", TagCenter)
      0x64 {- dialog -} | bsEqAddr "dialog" -> Just ("dialog", TagDialog)
      0x66 {- figure/footer -} -> case b 1 of
        0x69 | bsEqAddr "figure" -> Just ("figure", TagFigure)
        0x6F | bsEqAddr "footer" -> Just ("footer", TagFooter)
        _ -> Nothing
      0x68 {- header/hgroup -} -> case b 1 of
        0x65 | bsEqAddr "header" -> Just ("header", TagHeader)
        0x67 | bsEqAddr "hgroup" -> Just ("hgroup", TagHgroup)
        _ -> Nothing
      0x69 {- iframe -} | bsEqAddr "iframe" -> Just ("iframe", TagIframe)
      0x6F {- object/option -} -> case b 1 of
        0x62 | bsEqAddr "object" -> Just ("object", TagObject)
        0x70 | bsEqAddr "option" -> Just ("option", TagOption)
        _ -> Nothing
      0x73 {- script/search/select/source/strike/strong -} -> case b 1 of
        0x63 | bsEqAddr "script" -> Just ("script", TagScript)
        0x65 -> case b 2 of
          0x61 | bsEqAddr "search" -> Just ("search", TagSearch)
          0x6C | bsEqAddr "select" -> Just ("select", TagSelect)
          _ -> Nothing
        0x6F | bsEqAddr "source" -> Just ("source", TagSource)
        0x74 -> case b 2 of
          0x72 | bsEqAddr "strike" -> Just ("strike", TagStrike)
          _ -> Nothing
        _ -> Nothing
      _ -> Nothing
    _ -> Nothing

{-# NOINLINE commonAttrMapBS #-}
commonAttrMapBS :: HM.HashMap ByteString Text
commonAttrMapBS = HM.fromList
  [ (n, TE.decodeUtf8Lenient n)
  | n <- [ "class","id","style","src","href","type","name","value","alt","title"
         , "width","height","rel","charset","content","lang","dir","action","method"
         , "for","tabindex","role","target","placeholder","disabled","checked"
         , "selected","readonly","required","hidden","data","aria-label","aria-hidden"
         , "autocomplete","autofocus","colspan","rowspan","scope","encoding"
         , "http-equiv","property","media","sizes","crossorigin","integrity"
         , "defer","async","nomodule","color","face","size","xmlns","xmlns:xlink"
         , "xlink:href","xlink:type","xml:lang","xml:space","definitionurl"
         ]
  ]

{-# INLINE internAttrNameBS #-}
internAttrNameBS :: ByteString -> Text
internAttrNameBS !rawName =
  case HM.lookup rawName commonAttrMapBS of
    Just !interned -> interned
    Nothing
      | BS.any (\b -> b >= 0x41 && b <= 0x5A) rawName ->
          let !lowered = BS.map asciiToLower rawName
          in case HM.lookup lowered commonAttrMapBS of
               Just !interned -> interned
               Nothing -> TE.decodeUtf8Lenient lowered
      | otherwise -> TE.decodeUtf8Lenient rawName
  where
    asciiToLower :: Word8 -> Word8
    asciiToLower b
      | b >= 0x41, b <= 0x5A = b + 32
      | otherwise = b

{-# NOINLINE internAttrNameRange #-}
internAttrNameRange :: ByteString -> Int -> Int -> Text
internAttrNameRange !bs !off !end =
  let !n = end - off
      b :: Int -> Word8
      b i = BSU.unsafeIndex bs (off + i)
      bl :: Int -> Word8
      bl i = let !w = BSU.unsafeIndex bs (off + i)
             in if w >= 0x41 && w <= 0x5A then w + 32 else w
  in case n of
    2 | bl 0 == 0x69, bl 1 == 0x64 -> "id"
    3 | bl 0 == 0x73, bl 1 == 0x72, bl 2 == 0x63 -> "src"
      | bl 0 == 0x64, bl 1 == 0x69, bl 2 == 0x72 -> "dir"
      | bl 0 == 0x66, bl 1 == 0x6f, bl 2 == 0x72 -> "for"
      | bl 0 == 0x61, bl 1 == 0x6c, bl 2 == 0x74 -> "alt"
      | bl 0 == 0x72, bl 1 == 0x65, bl 2 == 0x6c -> "rel"
    4 | bl 0 == 0x68, bl 1 == 0x72, bl 2 == 0x65, bl 3 == 0x66 -> "href"
      | bl 0 == 0x74, bl 1 == 0x79, bl 2 == 0x70, bl 3 == 0x65 -> "type"
      | bl 0 == 0x6e, bl 1 == 0x61, bl 2 == 0x6d, bl 3 == 0x65 -> "name"
      | bl 0 == 0x6c, bl 1 == 0x61, bl 2 == 0x6e, bl 3 == 0x67 -> "lang"
      | bl 0 == 0x72, bl 1 == 0x6f, bl 2 == 0x6c, bl 3 == 0x65 -> "role"
      | bl 0 == 0x73, bl 1 == 0x69, bl 2 == 0x7a, bl 3 == 0x65 -> "size"
      | bl 0 == 0x66, bl 1 == 0x61, bl 2 == 0x63, bl 3 == 0x65 -> "face"
      | bl 0 == 0x64, bl 1 == 0x61, bl 2 == 0x74, bl 3 == 0x61 -> "data"
    5 | bl 0 == 0x63, bl 1 == 0x6c, bl 2 == 0x61, bl 3 == 0x73, bl 4 == 0x73 -> "class"
      | bl 0 == 0x73, bl 1 == 0x74, bl 2 == 0x79, bl 3 == 0x6c, bl 4 == 0x65 -> "style"
      | bl 0 == 0x76, bl 1 == 0x61, bl 2 == 0x6c, bl 3 == 0x75, bl 4 == 0x65 -> "value"
      | bl 0 == 0x74, bl 1 == 0x69, bl 2 == 0x74, bl 3 == 0x6c, bl 4 == 0x65 -> "title"
      | bl 0 == 0x77, bl 1 == 0x69, bl 2 == 0x64, bl 3 == 0x74, bl 4 == 0x68 -> "width"
      | bl 0 == 0x6d, bl 1 == 0x65, bl 2 == 0x64, bl 3 == 0x69, bl 4 == 0x61 -> "media"
      | bl 0 == 0x73, bl 1 == 0x69, bl 2 == 0x7a, bl 3 == 0x65, bl 4 == 0x73 -> "sizes"
      | bl 0 == 0x64, bl 1 == 0x65, bl 2 == 0x66, bl 3 == 0x65, bl 4 == 0x72 -> "defer"
      | bl 0 == 0x61, bl 1 == 0x73, bl 2 == 0x79, bl 3 == 0x6e, bl 4 == 0x63 -> "async"
      | bl 0 == 0x63, bl 1 == 0x6f, bl 2 == 0x6c, bl 3 == 0x6f, bl 4 == 0x72 -> "color"
      | bl 0 == 0x73, bl 1 == 0x63, bl 2 == 0x6f, bl 3 == 0x70, bl 4 == 0x65 -> "scope"
    6 | bl 0 == 0x68, bl 1 == 0x65, bl 2 == 0x69, bl 3 == 0x67, bl 4 == 0x68, bl 5 == 0x74 -> "height"
      | bl 0 == 0x68, bl 1 == 0x69, bl 2 == 0x64, bl 3 == 0x64, bl 4 == 0x65, bl 5 == 0x6e -> "hidden"
      | bl 0 == 0x74, bl 1 == 0x61, bl 2 == 0x72, bl 3 == 0x67, bl 4 == 0x65, bl 5 == 0x74 -> "target"
      | bl 0 == 0x61, bl 1 == 0x63, bl 2 == 0x74, bl 3 == 0x69, bl 4 == 0x6f, bl 5 == 0x6e -> "action"
      | bl 0 == 0x6d, bl 1 == 0x65, bl 2 == 0x74, bl 3 == 0x68, bl 4 == 0x6f, bl 5 == 0x64 -> "method"
    7 | bl 0 == 0x63, bl 1 == 0x68, bl 2 == 0x61, bl 3 == 0x72
      , bl 4 == 0x73, bl 5 == 0x65, bl 6 == 0x74 -> "charset"
      | bl 0 == 0x63, bl 1 == 0x6f, bl 2 == 0x6e, bl 3 == 0x74
      , bl 4 == 0x65, bl 5 == 0x6e, bl 6 == 0x74 -> "content"
      | bl 0 == 0x63, bl 1 == 0x6f, bl 2 == 0x6c, bl 3 == 0x73
      , bl 4 == 0x70, bl 5 == 0x61, bl 6 == 0x6e -> "colspan"
      | bl 0 == 0x72, bl 1 == 0x6f, bl 2 == 0x77, bl 3 == 0x73
      , bl 4 == 0x70, bl 5 == 0x61, bl 6 == 0x6e -> "rowspan"
      | bl 0 == 0x63, bl 1 == 0x68, bl 2 == 0x65, bl 3 == 0x63
      , bl 4 == 0x6b, bl 5 == 0x65, bl 6 == 0x64 -> "checked"
    8 | bl 0 == 0x64, bl 1 == 0x69, bl 2 == 0x73, bl 3 == 0x61
      , bl 4 == 0x62, bl 5 == 0x6c, bl 6 == 0x65, bl 7 == 0x64 -> "disabled"
      | bl 0 == 0x73, bl 1 == 0x65, bl 2 == 0x6c, bl 3 == 0x65
      , bl 4 == 0x63, bl 5 == 0x74, bl 6 == 0x65, bl 7 == 0x64 -> "selected"
      | bl 0 == 0x72, bl 1 == 0x65, bl 2 == 0x61, bl 3 == 0x64
      , bl 4 == 0x6f, bl 5 == 0x6e, bl 6 == 0x6c, bl 7 == 0x79 -> "readonly"
      | bl 0 == 0x72, bl 1 == 0x65, bl 2 == 0x71, bl 3 == 0x75
      , bl 4 == 0x69, bl 5 == 0x72, bl 6 == 0x65, bl 7 == 0x64 -> "required"
      | bl 0 == 0x70, bl 1 == 0x72, bl 2 == 0x6f, bl 3 == 0x70
      , bl 4 == 0x65, bl 5 == 0x72, bl 6 == 0x74, bl 7 == 0x79 -> "property"
      | bl 0 == 0x74, bl 1 == 0x61, bl 2 == 0x62, bl 3 == 0x69
      , bl 4 == 0x6e, bl 5 == 0x64, bl 6 == 0x65, bl 7 == 0x78 -> "tabindex"
      | bl 0 == 0x65, bl 1 == 0x6e, bl 2 == 0x63, bl 3 == 0x6f
      , bl 4 == 0x64, bl 5 == 0x69, bl 6 == 0x6e, bl 7 == 0x67 -> "encoding"
    _ | n <= 0 -> ""
    _ -> internAttrNameBS (BSU.unsafeTake n (BSU.unsafeDrop off bs))

tagIdToText :: TagId -> Text
tagIdToText TagA = "a"
tagIdToText TagAbbr = "abbr"
tagIdToText TagAddress = "address"
tagIdToText TagApplet = "applet"
tagIdToText TagArea = "area"
tagIdToText TagArticle = "article"
tagIdToText TagAside = "aside"
tagIdToText TagB = "b"
tagIdToText TagBase = "base"
tagIdToText TagBasefont = "basefont"
tagIdToText TagBgsound = "bgsound"
tagIdToText TagBig = "big"
tagIdToText TagBlockquote = "blockquote"
tagIdToText TagBody = "body"
tagIdToText TagBr = "br"
tagIdToText TagButton = "button"
tagIdToText TagCaption = "caption"
tagIdToText TagCenter = "center"
tagIdToText TagCode = "code"
tagIdToText TagCol = "col"
tagIdToText TagColgroup = "colgroup"
tagIdToText TagDd = "dd"
tagIdToText TagDetails = "details"
tagIdToText TagDialog = "dialog"
tagIdToText TagDir = "dir"
tagIdToText TagDiv = "div"
tagIdToText TagDl = "dl"
tagIdToText TagDt = "dt"
tagIdToText TagEm = "em"
tagIdToText TagEmbed = "embed"
tagIdToText TagFieldset = "fieldset"
tagIdToText TagFigcaption = "figcaption"
tagIdToText TagFigure = "figure"
tagIdToText TagFont = "font"
tagIdToText TagFooter = "footer"
tagIdToText TagForm = "form"
tagIdToText TagFrame = "frame"
tagIdToText TagFrameset = "frameset"
tagIdToText TagH1 = "h1"
tagIdToText TagH2 = "h2"
tagIdToText TagH3 = "h3"
tagIdToText TagH4 = "h4"
tagIdToText TagH5 = "h5"
tagIdToText TagH6 = "h6"
tagIdToText TagHead = "head"
tagIdToText TagHeader = "header"
tagIdToText TagHgroup = "hgroup"
tagIdToText TagHr = "hr"
tagIdToText TagHtml = "html"
tagIdToText TagI = "i"
tagIdToText TagIframe = "iframe"
tagIdToText TagImage = "image"
tagIdToText TagImg = "img"
tagIdToText TagInput = "input"
tagIdToText TagKeygen = "keygen"
tagIdToText TagLi = "li"
tagIdToText TagLink = "link"
tagIdToText TagListing = "listing"
tagIdToText TagMain = "main"
tagIdToText TagMarquee = "marquee"
tagIdToText TagMenu = "menu"
tagIdToText TagMenuitem = "menuitem"
tagIdToText TagMeta = "meta"
tagIdToText TagNav = "nav"
tagIdToText TagNobr = "nobr"
tagIdToText TagNoembed = "noembed"
tagIdToText TagNoframes = "noframes"
tagIdToText TagNoscript = "noscript"
tagIdToText TagObject = "object"
tagIdToText TagOl = "ol"
tagIdToText TagOptgroup = "optgroup"
tagIdToText TagOption = "option"
tagIdToText TagP = "p"
tagIdToText TagParam = "param"
tagIdToText TagPlaintext = "plaintext"
tagIdToText TagPre = "pre"
tagIdToText TagRb = "rb"
tagIdToText TagRp = "rp"
tagIdToText TagRt = "rt"
tagIdToText TagRtc = "rtc"
tagIdToText TagRuby = "ruby"
tagIdToText TagS = "s"
tagIdToText TagScript = "script"
tagIdToText TagSearch = "search"
tagIdToText TagSection = "section"
tagIdToText TagSelect = "select"
tagIdToText TagSmall = "small"
tagIdToText TagSource = "source"
tagIdToText TagSpan = "span"
tagIdToText TagStrike = "strike"
tagIdToText TagStrong = "strong"
tagIdToText TagStyle = "style"
tagIdToText TagSub = "sub"
tagIdToText TagSummary = "summary"
tagIdToText TagSup = "sup"
tagIdToText TagSvg = "svg"
tagIdToText TagMath = "math"
tagIdToText TagTable = "table"
tagIdToText TagTbody = "tbody"
tagIdToText TagTd = "td"
tagIdToText TagTemplate = "template"
tagIdToText TagTextarea = "textarea"
tagIdToText TagTfoot = "tfoot"
tagIdToText TagTh = "th"
tagIdToText TagThead = "thead"
tagIdToText TagTitle = "title"
tagIdToText TagTr = "tr"
tagIdToText TagTrack = "track"
tagIdToText TagTt = "tt"
tagIdToText TagU = "u"
tagIdToText TagUl = "ul"
tagIdToText TagVar = "var"
tagIdToText TagWbr = "wbr"
tagIdToText TagXmp = "xmp"
tagIdToText TagUnknown = ""

{-# INLINE tagIdIsSpecial #-}
tagIdIsSpecial :: TagId -> Bool
tagIdIsSpecial !tid = case tid of
  TagAddress -> True; TagApplet -> True; TagArea -> True; TagArticle -> True
  TagAside -> True; TagBase -> True; TagBasefont -> True; TagBgsound -> True
  TagBlockquote -> True; TagBody -> True; TagBr -> True; TagButton -> True
  TagCaption -> True; TagCenter -> True; TagCol -> True; TagColgroup -> True
  TagDd -> True; TagDetails -> True; TagDialog -> True; TagDir -> True
  TagDiv -> True; TagDl -> True; TagDt -> True; TagEmbed -> True
  TagFieldset -> True; TagFigcaption -> True; TagFigure -> True
  TagFooter -> True; TagForm -> True; TagFrame -> True; TagFrameset -> True
  TagH1 -> True; TagH2 -> True; TagH3 -> True; TagH4 -> True
  TagH5 -> True; TagH6 -> True; TagHead -> True; TagHeader -> True
  TagHgroup -> True; TagHr -> True; TagHtml -> True
  TagIframe -> True; TagImg -> True; TagInput -> True; TagKeygen -> True
  TagLi -> True; TagLink -> True; TagListing -> True; TagMain -> True
  TagMarquee -> True; TagMenu -> True; TagMenuitem -> True; TagMeta -> True
  TagNav -> True; TagNoembed -> True; TagNoframes -> True; TagNoscript -> True
  TagObject -> True; TagOl -> True; TagP -> True; TagParam -> True
  TagPlaintext -> True; TagPre -> True; TagScript -> True; TagSearch -> True
  TagSection -> True; TagSelect -> True; TagSource -> True; TagStyle -> True
  TagSummary -> True; TagTable -> True; TagTbody -> True; TagTd -> True
  TagTemplate -> True; TagTextarea -> True; TagTfoot -> True; TagTh -> True
  TagThead -> True; TagTitle -> True; TagTr -> True; TagTrack -> True
  TagUl -> True; TagWbr -> True
  _ -> False

{-# INLINE tagIdIsFormatting #-}
tagIdIsFormatting :: TagId -> Bool
tagIdIsFormatting !tid = case tid of
  TagA -> True; TagB -> True; TagBig -> True; TagCode -> True
  TagEm -> True; TagFont -> True; TagI -> True; TagNobr -> True
  TagS -> True; TagSmall -> True; TagStrike -> True; TagStrong -> True
  TagTt -> True; TagU -> True
  _ -> False

{-# INLINE tagIdIsHeading #-}
tagIdIsHeading :: TagId -> Bool
tagIdIsHeading !tid = case tid of
  TagH1 -> True; TagH2 -> True; TagH3 -> True
  TagH4 -> True; TagH5 -> True; TagH6 -> True
  _ -> False

{-# INLINE tagIdIsImpliedEndTag #-}
tagIdIsImpliedEndTag :: TagId -> Bool
tagIdIsImpliedEndTag !tid = case tid of
  TagDd -> True; TagDt -> True; TagLi -> True; TagOption -> True
  TagOptgroup -> True; TagP -> True; TagRb -> True; TagRp -> True
  TagRt -> True; TagRtc -> True
  _ -> False

{-# INLINE tagIdIsDefaultScopeTerminator #-}
tagIdIsDefaultScopeTerminator :: TagId -> Bool
tagIdIsDefaultScopeTerminator !tid = case tid of
  TagApplet -> True; TagCaption -> True; TagHtml -> True; TagTable -> True
  TagTd -> True; TagTh -> True; TagMarquee -> True; TagObject -> True
  TagTemplate -> True
  _ -> False

{-# INLINE tagIdIsButtonScopeTerminator #-}
tagIdIsButtonScopeTerminator :: TagId -> Bool
tagIdIsButtonScopeTerminator !tid =
  tid == TagButton || tagIdIsDefaultScopeTerminator tid

{-# INLINE tagIdIsListItemScopeTerminator #-}
tagIdIsListItemScopeTerminator :: TagId -> Bool
tagIdIsListItemScopeTerminator !tid = case tid of
  TagOl -> True; TagUl -> True
  _ -> tagIdIsDefaultScopeTerminator tid

{-# INLINE tagIdIsDefinitionScopeTerminator #-}
tagIdIsDefinitionScopeTerminator :: TagId -> Bool
tagIdIsDefinitionScopeTerminator !tid =
  tid == TagDl || tagIdIsDefaultScopeTerminator tid

{-# INLINE tagIdIsTableScopeTerminator #-}
tagIdIsTableScopeTerminator :: TagId -> Bool
tagIdIsTableScopeTerminator !tid = case tid of
  TagHtml -> True; TagTable -> True; TagTemplate -> True
  _ -> False

{-# INLINE tagIdIsForeignBreakout #-}
tagIdIsForeignBreakout :: TagId -> Bool
tagIdIsForeignBreakout !tid = case tid of
  TagB -> True; TagBig -> True; TagBlockquote -> True; TagBody -> True
  TagBr -> True; TagCenter -> True; TagCode -> True; TagDd -> True
  TagDiv -> True; TagDl -> True; TagDt -> True; TagEm -> True
  TagEmbed -> True; TagH1 -> True; TagH2 -> True; TagH3 -> True
  TagH4 -> True; TagH5 -> True; TagH6 -> True; TagHead -> True
  TagHr -> True; TagI -> True; TagImg -> True; TagLi -> True
  TagListing -> True; TagMenu -> True; TagMeta -> True; TagNobr -> True
  TagOl -> True; TagP -> True; TagPre -> True; TagRuby -> True
  TagS -> True; TagSmall -> True; TagSpan -> True; TagStrong -> True
  TagStrike -> True; TagSub -> True; TagSup -> True; TagTable -> True
  TagTt -> True; TagU -> True; TagUl -> True; TagVar -> True
  _ -> False

{-# INLINE tagIdIsVoid #-}
tagIdIsVoid :: TagId -> Bool
tagIdIsVoid !tid = case tid of
  TagArea -> True; TagBase -> True; TagBr -> True; TagCol -> True
  TagEmbed -> True; TagHr -> True; TagImg -> True; TagInput -> True
  TagLink -> True; TagMeta -> True; TagSource -> True; TagTrack -> True
  TagWbr -> True
  _ -> False

{-# INLINE tagIdIsRawText #-}
tagIdIsRawText :: TagId -> Bool
tagIdIsRawText !tid = tid == TagScript || tid == TagStyle
