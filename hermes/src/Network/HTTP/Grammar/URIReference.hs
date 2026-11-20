{-# LANGUAGE TemplateHaskell #-}

-- | Wire syntax for URI references, IRI references and serialized browser URLs.
module Network.HTTP.Grammar.URIReference (
  uriReference,
  uriReferenceBytes,
  iriReference,
  fragmentlessReference,
  absoluteURI,
  absoluteURIBytes,
  uriWithFragment,
  absolutePathReference,
  uriPathSuffixBytes,
  nonEmptyPathSegment,
  codedURL,
  referrerReference,
  browserURLBytes,
) where

import Data.Bits ((.&.))
import Data.Char (ord)
import Data.CharSet (CharSet)
import qualified Data.CharSet as CS
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.Authority (pctEncoded, uriHostBytes, uriSchemeBytes)
import Network.HTTP.Grammar.Parser
import Network.IPAddress (skipIPLiteralParser, skipIPv6Parser)


-- | RFC 3986 URI-reference, preserving the original spelling.
uriReference :: ParserT st String ShortText
uriReference = shortASCIIFromParser_ uriReferenceBytes
{-# INLINE uriReference #-}


-- | Validate an RFC 3986 URI-reference without copying the consumed bytes.
uriReferenceBytes :: ParserT st String ()
uriReferenceBytes = reference uriAuthority *> uriFragment
{-# INLINE uriReferenceBytes #-}


-- | RFC 3987 IRI-reference, retaining the original UTF-8 and percent escapes.
iriReference :: ParserT st String ShortText
iriReference =
  shortASCIIFromParser_ (uriReferenceBytes <* notFollowedBy nonAscii)
    <|> withByteString iriReferenceBytes (\() bytes -> maybe failed pure (ST.fromByteString bytes))
  where
    nonAscii = do
      byte <- anyWord8
      if byte >= 0x80 then pure () else failed


iriReferenceBytes :: ParserT st String ()
iriReferenceBytes =
  referenceWith iriAuthority (iriChars ucschar) (iriChars queryUnicode queryChars)
    *> branch $(char '#') (iriChars ucschar queryChars) (pure ())
  where
    iriAuthority = authorityWith (referenceHostPort iriHost) (iriChars ucschar userInfoChars)
    iriHost = skipIPLiteralParser <|> iriChars ucschar regNameChars
    iriChars unicode allowed = charRun allowed *> skipMany ((pctEncoded <|> satisfy_ unicode) *> charRun allowed)
    queryUnicode c = ucschar c || privateCodePoint (ord c)


-- RFC 3987 §2.2 limits both the planes and the end of each plane.
-- The parser validates the consumed UTF-8 slice before producing ShortText.
ucschar :: Char -> Bool
ucschar c =
  (0xa0 <= n && n <= 0xd7ff)
    || (0xf900 <= n && n <= 0xfdcf)
    || (0xfdf0 <= n && n <= 0xffef)
    || (0x10000 <= n && n <= 0xefffd && (n < 0xe0000 || n >= 0xe1000) && n .&. 0xffff <= 0xfffd)
  where
    n = ord c


privateCodePoint :: Int -> Bool
privateCodePoint n =
  (0xe000 <= n && n <= 0xf8ff)
    || (0xf0000 <= n && n <= 0xffffd)
    || (0x100000 <= n && n <= 0x10fffd)


-- | An absolute-URI or a partial-URI, as used by Content-Location.
fragmentlessReference :: ParserT st String ShortText
fragmentlessReference = shortASCIIFromParser_ (reference uriAuthority)
{-# INLINE fragmentlessReference #-}


-- | RFC 3986 absolute-URI, with a required scheme and no fragment.
absoluteURI :: ParserT st String ShortText
absoluteURI = shortASCIIFromParser_ absoluteURIBytes
{-# INLINE absoluteURI #-}


-- | Validate an absolute URI without retaining its spelling.
absoluteURIBytes :: ParserT st String ()
absoluteURIBytes = uriSchemeBytes *> $(char ':') *> absolutePart uriAuthority
{-# INLINE absoluteURIBytes #-}


-- | RFC 3986 URI with a required scheme and an optional fragment.
uriWithFragment :: ParserT st String ShortText
uriWithFragment = shortASCIIFromParser_ (absoluteURIBytes *> uriFragment)
{-# INLINE uriWithFragment #-}


uriFragment :: ParserT st String ()
uriFragment = branch $(char '#') (uriChars queryChars) (pure ())
{-# INLINE uriFragment #-}


-- | RFC 3986 path-absolute with an optional query, as in WebDAV Simple-ref.
absolutePathReference :: ParserT st String ShortText
absolutePathReference =
  shortASCIIFromParser_ ($(char '/') *> notFollowedBy $(char '/') *> uriChars pathChars *> uriQuery)
{-# INLINE absolutePathReference #-}


-- | A slash-led URI path with its optional query and fragment.
uriPathSuffixBytes :: ParserT st String ()
uriPathSuffixBytes = $(char '/') *> uriChars pathChars *> uriQuery *> uriFragment
{-# NOINLINE uriPathSuffixBytes #-}


-- | A single nonempty path segment, preserving percent escapes.
nonEmptyPathSegment :: ParserT st String ShortText
nonEmptyPathSegment = shortASCIIFromParser_ $ case segmentChars of
  AsciiSet low high ->
    (skipSome (skipSatisfyAscii (asciiMember low high)) <|> (pctEncoded *> charRun segmentChars))
      *> skipMany (pctEncoded *> charRun segmentChars)
{-# INLINE nonEmptyPathSegment #-}


-- | RFC 4918 Coded-URL, retaining the URI without its enclosing brackets.
codedURL :: ParserT st String ShortText
codedURL = $(char '<') *> absoluteURI <* $(char '>')
{-# INLINE codedURL #-}


-- | A fragmentless reference with no userinfo in its authority.
referrerReference :: ParserT st String ShortText
referrerReference = shortASCIIFromParser_ (reference uriHostPort)
{-# INLINE referrerReference #-}


reference :: ParserT st String () -> ParserT st String ()
reference authority = referenceWith authority uriChars (uriChars queryChars)
{-# INLINE reference #-}


referenceWith :: ParserT st String () -> (AsciiSet -> ParserT st String ()) -> ParserT st String () -> ParserT st String ()
referenceWith authority chars query = do
  branch (uriSchemeBytes *> $(char ':')) (hierPart authority chars pathChars) (hierPart authority chars firstRelativeSegment)
  branch $(char '?') query (pure ())
{-# INLINE referenceWith #-}


absolutePart :: ParserT st String () -> ParserT st String ()
absolutePart authority = hierPart authority uriChars pathChars *> uriQuery
{-# INLINE absolutePart #-}


uriQuery :: ParserT st String ()
uriQuery =
  branch $(char '?') (uriChars queryChars) (pure ())
{-# INLINE uriQuery #-}


hierPart :: ParserT st String () -> (AsciiSet -> ParserT st String ()) -> AsciiSet -> ParserT st String ()
hierPart authority chars first =
  branch
    $(string "//")
    (authority *> pathSuffix)
    (branch (lookahead $(char '/')) (chars pathChars) (chars first *> pathSuffix))
  where
    pathSuffix = branch $(char '/') (chars pathChars) (pure ())
{-# INLINE hierPart #-}


uriAuthority :: ParserT st String ()
uriAuthority = authorityWith uriHostPort (uriChars userInfoChars)
{-# INLINE uriAuthority #-}


authorityWith :: ParserT st String () -> ParserT st String () -> ParserT st String ()
authorityWith host user = (host <* authorityEnd) <|> (user *> $(char '@') *> host)
{-# INLINE authorityWith #-}


-- The URI grammar only gives ports a decimal spelling. An application
-- interpreting an authority as a transport endpoint checks the numeric bound.
uriHostPort :: ParserT st String ()
uriHostPort = referenceHostPort (uriHostBytes <|> pure ())
{-# INLINE uriHostPort #-}


referenceHostPort :: ParserT st String () -> ParserT st String ()
referenceHostPort host = host *> branch $(char ':') (skipMany (skipSatisfyAscii isDigit)) (pure ())
{-# INLINE referenceHostPort #-}


authorityEnd :: ParserT st String ()
authorityEnd = notFollowedBy (skipSatisfyAscii (\c -> not (c == '/' || c == '?' || c == '#' || c == '"' || c == '>' || isOWS c)))
{-# INLINE authorityEnd #-}


uriChars :: AsciiSet -> ParserT st String ()
uriChars allowed = charRun allowed *> skipMany (pctEncoded *> charRun allowed)
{-# INLINE uriChars #-}


charRun :: AsciiSet -> ParserT st String ()
charRun (AsciiSet low high) = skipMany (skipSatisfyAscii (asciiMember low high))
{-# INLINE charRun #-}


regNameChars, userInfoChars, firstRelativeSegment, segmentChars, pathChars, queryChars :: AsciiSet
regNameChars = asciiSet regName
userInfoChars = asciiSet userInfo
firstRelativeSegment = asciiSet (regName <> "@")
segmentChars = asciiSet (userInfo <> "@")
pathChars = asciiSet path
queryChars = asciiSet (path <> "?")


regName, userInfo, path :: CharSet
regName = unreserved <> "!$&'()*+,;="
userInfo = regName <> ":"
path = userInfo <> "@/"


unreserved :: CharSet
unreserved = CS.range 'a' 'z' <> CS.range 'A' 'Z' <> CS.range '0' '9' <> "-._~"


{- | ASCII wire syntax of the URL serializer used by Ping-From and Ping-To.
The opaque path and query have different character rules from RFC 3986.
This parser checks the serialized components and preserves the spelling. It
does not resolve references or canonicalize hosts, paths, or ports.
-}
browserURLBytes :: ParserT st String ()
browserURLBytes = do
  branch $(string "file:") (fileURL *> specialQuery) $
    branch specialScheme (specialURL *> specialQuery) $
      browserScheme *> otherURL *> query browserQueryChars
  branch $(char '#') (charRun browserFragmentChars) (pure ())
  where
    browserScheme =
      skipSatisfyAscii isLowerLetter
        *> skipMany (skipSatisfyAscii (\c -> isLowerLetter c || isDigit c || c == '+' || c == '-' || c == '.'))
        *> $(char ':')
    isLowerLetter c = 'a' <= c && c <= 'z'
    specialScheme =
      $(string "https:")
        <|> $(string "http:")
        <|> $(string "wss:")
        <|> $(string "ws:")
        <|> $(string "ftp:")
    fileURL = $(string "//") *> (browserHost browserDomainChars <|> pure ()) *> $(char '/') *> charRun specialPathChars
    specialURL = $(string "//") *> browserAuthority (browserHost browserDomainChars) *> $(char '/') *> charRun specialPathChars
    specialQuery = query specialQueryChars
    query allowed = branch $(char '?') (charRun allowed) (pure ())
    otherURL =
      branch
        $(string "//")
        ((browserAuthority (browserHost browserHostChars) <|> pure ()) *> branch $(char '/') (charRun browserPathChars) (pure ()))
        (branch $(char '/') (charRun browserPathChars) opaquePath)
    opaquePath = charRun opaquePathChars *> skipMany (skipSome $(char ' ') *> lookahead opaqueContinuation *> charRun opaquePathChars)
    opaqueContinuation = skipSatisfyAscii (\c -> '!' <= c && c <= '~')
{-# INLINE browserURLBytes #-}


browserAuthority :: ParserT st String () -> ParserT st String ()
browserAuthority host = (hostPort <|> (charRun browserUserInfoChars *> optionalPassword *> $(char '@') *> hostPort))
  where
    hostPort = host *> branch $(char ':') (() <$ boundedDecimalWord 65_535 failed) (pure ()) <* authorityEnd
    optionalPassword = branch $(char ':') (charRun browserUserInfoChars) (pure ())
{-# INLINE browserAuthority #-}


browserHost :: AsciiSet -> ParserT st String ()
browserHost (AsciiSet low high) =
  ($(char '[') *> skipIPv6Parser *> $(char ']')) <|> skipSome (skipSatisfyAscii (asciiMember low high))
{-# INLINE browserHost #-}


browserHostChars, browserDomainChars, browserUserInfoChars, browserPathChars, specialPathChars :: AsciiSet
browserHostChars = asciiSet browserHostSet
browserDomainChars = asciiSet (CS.difference browserHostSet "%")
browserUserInfoChars = asciiSet (CS.difference browserPathSet "/:;=@[\\]^|")
browserPathChars = asciiSet browserPathSet
specialPathChars = asciiSet (CS.difference browserPathSet "\\")


browserQueryChars, specialQueryChars, browserFragmentChars, opaquePathChars :: AsciiSet
browserQueryChars = asciiSet browserQuerySet
specialQueryChars = asciiSet (CS.difference browserQuerySet "'")
browserFragmentChars = asciiSet (CS.difference visibleASCII "\"<>`")
opaquePathChars = asciiSet (CS.difference visibleASCII "?#")


browserHostSet, browserPathSet, browserQuerySet, visibleASCII :: CharSet
browserHostSet = CS.difference visibleASCII "#/:<>?@[\\]^|"
browserPathSet = CS.difference visibleASCII "\"#<>?`{}"
browserQuerySet = CS.difference visibleASCII "\"#<>"
visibleASCII = CS.range '!' '~'
