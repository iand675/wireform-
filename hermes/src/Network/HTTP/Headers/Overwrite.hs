{- |
RFC 4918 §10.6 @Overwrite@ request header — specifies whether a
@COPY@ or @MOVE@ may clobber an existing resource at the destination.

== Grammar

@
Overwrite = "T" | "F"
@

@T@ (true) permits overwriting; @F@ (false) forbids it.

Spec: <https://www.rfc-editor.org/rfc/rfc4918#section-10.6>

See also: "Network.HTTP.Headers.Destination", "Network.HTTP.Headers.Depth", "Network.HTTP.Headers.If".
-}
module Network.HTTP.Headers.Overwrite (
  Overwrite (..),
  overwriteParser,
  renderOverwrite,
) where

import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hOverwrite)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | @True@ when overwriting the destination is permitted (@T@), @False@ otherwise (@F@).
newtype Overwrite = Overwrite {overwriteAllowed :: Bool}
  deriving stock (Eq, Show)


instance KnownHeader Overwrite where
  type ParseFailure Overwrite = String
  type Cardinality Overwrite = 'ZeroOrOne
  type Direction Overwrite = 'Request


  parseFromHeaders _ = parseTFFieldLine "Overwrite" Overwrite


  renderToHeaders _ = M.toStrictByteString . renderOverwrite


  headerName _ = hOverwrite


overwriteParser :: ParserT st String Overwrite
overwriteParser = Overwrite <$> tfFlag


renderOverwrite :: Overwrite -> M.Builder
renderOverwrite (Overwrite True) = "T"
renderOverwrite (Overwrite False) = "F"
