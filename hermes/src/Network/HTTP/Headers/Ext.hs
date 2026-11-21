{- | @Ext@ is an empty response header acknowledging that the recipient
fulfilled the request's mandatory end-to-end extension declarations.

Spec: <https://www.rfc-editor.org/rfc/rfc2774.html#section-4.3>

See also: "Network.HTTP.Headers.Man", "Network.HTTP.Headers.Opt", "Network.HTTP.Headers.PEP".
-}
module Network.HTTP.Headers.Ext (
  Ext (..),
  extAcknowledged,
  extParser,
  renderExt,
) where

import qualified Data.ByteString.Char8 as BSC
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (shortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hExt)
import qualified Network.HTTP.Headers.Mason as M


-- | An acknowledgement with its received optional field whitespace.
newtype Ext = Ext {rawExt :: ST.ShortText}
  deriving stock (Eq, Show)


-- | The canonical empty acknowledgement.
extAcknowledged :: Ext
extAcknowledged = Ext ""


instance KnownHeader Ext where
  type ParseFailure Ext = String
  type Cardinality Ext = 'ZeroOrOne
  type Direction Ext = 'Response


  parseFromHeaders _ ("" :| []) = Right extAcknowledged
  parseFromHeaders _ (wire :| []) = parseFieldLine "Ext" extParser wire
  parseFromHeaders _ _ = Left "Multiple Ext field lines"


  renderToHeaders _ = ST.toByteString . rawExt


  renderToHeadersChecked _ (Ext text)
    | BSC.all isOWS wire = Just wire
    | otherwise = Nothing
    where
      wire = ST.toByteString text
  {-# INLINE renderToHeadersChecked #-}


  headerName _ = hExt


extParser :: ParserT st String Ext
extParser = Ext <$> shortASCIIFromParser_ ows


renderExt :: Ext -> M.Builder
renderExt = shortText . rawExt
