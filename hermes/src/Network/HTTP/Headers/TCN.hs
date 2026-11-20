{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 2295 §8.5 @TCN@ signals a transparently negotiated response.
The ordered directive list carries the response type, an optional server
override, and extensions. A present empty field is allowed.

Spec: <https://www.rfc-editor.org/rfc/rfc2295.html#section-8.5>
-}
module Network.HTTP.Headers.TCN (
  TCN (..),
  TCNDirective (..),
  TCNDirectiveKind (..),
  TCNResponseType (..),
  TCNOverride (..),
  tcnDirectiveKind,
  tcnDirectiveFromKind,
  tcnParser,
  renderTCN,
) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Short as SBS
import Data.Char (isAsciiUpper, toLower)
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (renderHttpQuotedString, shortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hTCN)
import qualified Network.HTTP.Headers.Mason as M


data TCNDirective = TCNDirective
  { tcnName :: !ST.ShortText
  , tcnValue :: !(Maybe ST.ShortText)
  }
  deriving stock (Eq, Show)


newtype TCN = TCN {tcnDirectives :: [TCNDirective]}
  deriving stock (Eq, Show)


data TCNDirectiveKind
  = TCNResponse !TCNResponseType
  | TCNServerOverride !TCNOverride
  | TCNExtension !ST.ShortText !(Maybe ST.ShortText)
  deriving stock (Eq, Show)


data TCNResponseType = TCNList | TCNChoice | TCNAdhoc
  deriving stock (Eq, Show)


data TCNOverride = TCNReChoose | TCNKeep
  deriving stock (Eq, Show)


-- | Recognize the bare built-in directives and retain other entries as extensions.
tcnDirectiveKind :: TCNDirective -> TCNDirectiveKind
tcnDirectiveKind (TCNDirective name value) = case value of
  Just _ -> extension
  Nothing -> case folded of
    "list" -> TCNResponse TCNList
    "choice" -> TCNResponse TCNChoice
    "adhoc" -> TCNResponse TCNAdhoc
    "re-choose" -> TCNServerOverride TCNReChoose
    "keep" -> TCNServerOverride TCNKeep
    _ -> extension
  where
    extension = TCNExtension name value
    raw = ST.toByteString name
    folded
      | BSC.any isAsciiUpper raw = BSC.map toLower raw
      | otherwise = raw


tcnDirectiveFromKind :: TCNDirectiveKind -> TCNDirective
tcnDirectiveFromKind = \case
  TCNResponse TCNList -> bare "list"
  TCNResponse TCNChoice -> bare "choice"
  TCNResponse TCNAdhoc -> bare "adhoc"
  TCNServerOverride TCNReChoose -> bare "re-choose"
  TCNServerOverride TCNKeep -> bare "keep"
  TCNExtension name value -> TCNDirective name value
  where
    bare name = TCNDirective name Nothing


instance KnownHeader TCN where
  type ParseFailure TCN = String
  type Cardinality TCN = 'ZeroOrOne
  type Direction TCN = 'Response


  parseFromHeaders _ = fmap TCN . parseListFieldLines "TCN" directiveList


  renderToHeaders _ = M.toStrictByteString . renderTCN


  headerName _ = hTCN


tcnParser :: ParserT st String TCN
tcnParser = TCN <$> directiveList


directiveList :: ParserT st String [TCNDirective]
directiveList = httpList $ do
  name <- rfc9110Token
  ows
  value <- optional ($(char '=') *> ows *> (rfc9110Token <|> (ST.fromText <$> httpQuotedString)))
  pure (TCNDirective name value)


renderTCN :: TCN -> M.Builder
renderTCN (TCN directives) = case directives of
  [] -> mempty
  first : rest -> renderDirective first <> foldMap ((", " <>) . renderDirective) rest
  where
    renderDirective (TCNDirective name value) =
      shortText name <> maybe mempty (\v -> M.char7 '=' <> renderValue v) value
    renderValue v
      | not (SBS.null bytes) && SBS.all (isTokenChar . toEnum . fromIntegral) bytes = M.shortByteString bytes
      | SBS.all (\b -> b == 9 || (b >= 32 && b < 127 && b /= 34 && b /= 92)) bytes =
          M.char7 '"' <> M.shortByteString bytes <> M.char7 '"'
      | otherwise = renderHttpQuotedString (ST.toText v)
      where
        bytes = ST.toShortByteString v
{-# INLINE renderTCN #-}
