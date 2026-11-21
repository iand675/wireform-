{-# LANGUAGE TemplateHaskell #-}

{- |
@MIME-Version@ — declares the version of the MIME protocol used to construct
the message, as a @major.minor@ pair. RFC 2045 defines @1.0@ and requires
recipients to ignore comments between lexical elements.

== Grammar

@
version := \"MIME-Version\" \":\" 1*DIGIT \".\" 1*DIGIT
@

Spec: <https://www.rfc-editor.org/rfc/rfc2045#section-4>,
<https://www.rfc-editor.org/rfc/rfc9112.html#appendix-B.1>

See also: "Network.HTTP.Headers.ContentType", "Network.HTTP.Headers.ContentEncoding", "Network.HTTP.Headers.ContentDisposition".
-}
module Network.HTTP.Headers.MIMEVersion (
  MIMEVersion (..),
  mimeVersionParser,
  renderMIMEVersion,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.List.NonEmpty (NonEmpty (..))
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (encodeDecimalVersion)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hMIMEVersion)
import qualified Network.HTTP.Headers.Mason as M


-- | A MIME protocol version with nonnegative components in the range of 'Int'.
data MIMEVersion = MIMEVersion
  { mimeVersionMajor :: {-# UNPACK #-} !Int
  , mimeVersionMinor :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Show)


instance KnownHeader MIMEVersion where
  type ParseFailure MIMEVersion = String
  type Cardinality MIMEVersion = 'ZeroOrOne
  type Direction MIMEVersion = 'RequestAndResponse


  -- Match the standard 1.0 spelling without setting up the comment parser.
  parseFromHeaders _ (wire :| [])
    | BS.length wire == 3
    , BSU.unsafeIndex wire 0 == 49
    , BSU.unsafeIndex wire 1 == 46
    , BSU.unsafeIndex wire 2 == 48 =
        Right (MIMEVersion 1 0)
  parseFromHeaders _ headers = parseSingleFieldLine "MIME-Version" mimeVersionParser headers


  renderToHeaders _ (MIMEVersion 1 0) = "1.0"
  renderToHeaders _ value = M.toStrictByteString (renderMIMEVersion value)


  renderToHeadersChecked _ (MIMEVersion 1 0) = Just "1.0"
  renderToHeadersChecked _ (MIMEVersion major minor)
    | major >= 0 && minor >= 0 = Just (encodeDecimalVersion 0 (fromIntegral major) (fromIntegral minor))
    | otherwise = Nothing
  {-# INLINE renderToHeadersChecked #-}


  headerName _ = hMIMEVersion


mimeVersionParser :: ParserT st String MIMEVersion
mimeVersionParser = do
  spacing
  major <- component
  spacing
  $(char '.')
  spacing
  minor <- component
  spacing
  pure MIMEVersion {mimeVersionMajor = major, mimeVersionMinor = minor}
  where
    spacing = skipMany (rws <|> skipComment)
    component = fromIntegral <$> boundedDecimalWord (fromIntegral (maxBound :: Int)) (err "MIME-Version component exceeds Int")


renderMIMEVersion :: MIMEVersion -> M.Builder
renderMIMEVersion (MIMEVersion major minor) =
  M.intDec major <> M.char7 '.' <> M.intDec minor
