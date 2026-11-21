{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
@DASL@ (RFC 5323 §3.2) — advertises the WebDAV SEARCH grammars supported
by a resource. The advertised grammars are a nonempty list of coded URLs.

Spec: <https://www.rfc-editor.org/rfc/rfc5323.html#section-3.2>

See also: "Network.HTTP.Headers.DAV", "Network.HTTP.Headers.Depth", "Network.HTTP.Headers.If".
-}
module Network.HTTP.Headers.DASL (
  DASL (DASL, DASLGrammars, daslGrammar, daslGrammars),
  daslParser,
  renderDASL,
) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.Rendering (encodeCommaSeparatedText)
import Network.HTTP.Grammar.URIReference (codedURL, outboundAbsoluteURIBytes, validURIText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hDASL)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (sepByCommas1, shortText)


-- | All advertised grammar identifiers, including their angle brackets.
newtype DASL = DASLGrammars {daslGrammars :: NonEmpty ST.ShortText}
  deriving stock (Eq, Show)


-- | Construct a singleton or read the first grammar through the legacy API.
pattern DASL :: ST.ShortText -> DASL
pattern DASL {daslGrammar} <- DASLGrammars (daslGrammar :| _)
  where
    DASL grammar = DASLGrammars (grammar :| [])


{-# COMPLETE DASL #-}


instance KnownHeader DASL where
  type ParseFailure DASL = String
  type Cardinality DASL = 'ZeroOrOne
  type Direction DASL = 'Response


  parseFromHeaders _ fields =
    DASLGrammars <$> (parseListFieldLines "DASL" (httpList grammarParser) fields >>= requireNonEmptyList "DASL")


  renderToHeaders _ = M.toStrictByteString . renderDASL


  renderToHeadersChecked _ (DASLGrammars grammars)
    | all valid grammars = Just (encodeCommaSeparatedText grammars)
    | otherwise = Nothing
    where
      valid = validURIText ($(char '<') *> outboundAbsoluteURIBytes <* $(char '>'))


  headerName _ = hDASL


daslParser :: ParserT st String DASL
daslParser = DASLGrammars <$> (httpList grammarParser >>= maybe failed pure . NE.nonEmpty)


grammarParser :: ParserT st String ST.ShortText
grammarParser = shortASCIIFromParser_ codedURL
{-# INLINE grammarParser #-}


renderDASL :: DASL -> M.Builder
renderDASL = sepByCommas1 . fmap shortText . daslGrammars
