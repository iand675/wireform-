{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
@Expect-CT@ — an /obsolete/ response header by which a host requested that
clients evaluate its connections against its Certificate Transparency policy,
optionally enforcing the requirement and/or reporting failures. Expect-CT was
deprecated in favour of CT being enforced by default in major browsers, but the
wire grammar is well specified, so this module captures it structurally.

== Grammar

@
Expect-CT           = 1#expect-ct-directive
expect-ct-directive = directive-name [ "=" directive-value ]
directive-name      = token
directive-value     = token / quoted-string
@

Directives form one comma-separated list across field lines. The defined
directives are @max-age=\<delta-seconds\>@ (required), the valueless flag
@enforce@, and @report-uri="\<absolute-URI\>"@. Directive names are
case-insensitive and unique. Unknown directives are ignored.

Spec: <https://www.rfc-editor.org/rfc/rfc9163#section-2.1>

See also: "Network.HTTP.Headers.PublicKeyPins", "Network.HTTP.Headers.PublicKeyPinsReportOnly", "Network.HTTP.Headers.StrictTransportSecurity".
-}
module Network.HTTP.Headers.ExpectCT (
  ExpectCT (..),
  expectCTParser,
  renderExpectCT,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder.Prim as Prim
import qualified Data.ByteString.Builder.Prim.Internal as Prim
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short as SBS
import Data.CaseInsensitive (CI)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import qualified Data.Text.Short as ST
import Foreign.Ptr (minusPtr, plusPtr)
import Network.HTTP.Grammar.URIReference (outboundAbsoluteURIBytes, validURIText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hExpectCT)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (copyText)
import Network.HTTP.Headers.TransportPolicy (directiveEnd, extension, policyAge, quoted, reportURI, valueless)
import Wireform.Encode.Direct (dBytes, dWord8)


-- | Parsed @Expect-CT@ policy.
data ExpectCT = ExpectCT
  { expectCtMaxAge :: !Word
  -- ^ @max-age@ delta-seconds: how long the CT policy is to be remembered.
  , expectCtEnforce :: !Bool
  -- ^ Whether the host requested enforcement (rather than report-only).
  , expectCtReportUri :: !(Maybe ST.ShortText)
  -- ^ Optional URI to which CT-compliance failures should be reported.
  }
  deriving stock (Eq, Show)


instance KnownHeader ExpectCT where
  type ParseFailure ExpectCT = String
  type Cardinality ExpectCT = 'ZeroOrOne
  type Direction ExpectCT = 'Response


  parseFromHeaders _ (line :| []) = parseFieldLine "Expect-CT" expectCTParser line
  parseFromHeaders _ lines' = parseListFieldLines "Expect-CT" directives lines' >>= applyDirectives


  renderToHeaders _ = M.toStrictByteString . renderExpectCT


  renderToHeadersChecked _ (ExpectCT age enforce uri)
    | maybe True (validURIText outboundAbsoluteURIBytes) uri =
        Just $ BSI.unsafeCreateUptoN capacity $ \ptr -> do
          start <- dBytes ptr 0 "max-age="
          end <- Prim.runB Prim.wordDec age (ptr `plusPtr` start)
          let ageEnd = end `minusPtr` ptr
          afterEnforce <- if enforce then dBytes ptr ageEnd ", enforce" else pure ageEnd
          case uri of
            Nothing -> pure afterEnforce
            Just endpoint -> do
              opened <- dBytes ptr afterEnforce ", report-uri=\""
              closed <- copyText ptr opened endpoint
              dWord8 ptr closed 34
    | otherwise = Nothing
    where
      capacity = BSI.checkedAdd "Expect-CT" (28 + if enforce then 9 else 0)
        (maybe 0 (BSI.checkedAdd "Expect-CT" 15 . SBS.length . ST.toShortByteString) uri)


  headerName _ = hExpectCT


data Directive
  = MaxAge !Word
  | Enforce
  | ReportURI !ST.ShortText
  | Extension !(CI ByteString)


expectCTParser :: ParserT st String ExpectCT
expectCTParser = directives >>= either err pure . applyDirectives


directives :: ParserT st String [Directive]
directives =
  httpList $
    $( switchAsciiCI
         [|
           case _ of
             "max-age" -> known (MaxAge <$> policyAge)
             "enforce" -> known (Enforce <$ valueless)
             "report-uri" -> known (ReportURI <$> reportURI)
           |]
     )
      <|> (Extension <$> extension)
  where
    known action = directiveEnd *> (action <|> err "Invalid Expect-CT directive")


applyDirectives :: [Directive] -> Either String ExpectCT
applyDirectives = go Nothing False Nothing Set.empty
  where
    go !age !enforce !uri !seen = \case
      [] -> case age of
        Just n -> Right (ExpectCT n enforce uri)
        Nothing -> Left "Expect-CT requires max-age"
      d : ds -> case d of
        MaxAge n -> case age of
          Just _ -> Left "Duplicate max-age directive"
          Nothing -> go (Just n) enforce uri seen ds
        Enforce
          | enforce -> Left "Duplicate enforce directive"
          | otherwise -> go age True uri seen ds
        ReportURI u -> case uri of
          Just _ -> Left "Duplicate report-uri directive"
          Nothing -> go age enforce (Just u) seen ds
        Extension name
          | Set.member name seen -> Left "Duplicate Expect-CT extension"
          | otherwise -> go age enforce uri (Set.insert name seen) ds


renderExpectCT :: ExpectCT -> M.Builder
renderExpectCT (ExpectCT age enforce uri) =
  "max-age="
    <> M.wordDec age
    <> (if enforce then ", enforce" else mempty)
    <> maybe mempty (\u -> ", report-uri=" <> quoted u) uri
