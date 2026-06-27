{-# LANGUAGE TemplateHaskell #-}

{- |
@MIME-Version@ — declares the version of the MIME protocol used to construct
the message, as a @major.minor@ pair. Standard HTTP/MIME messages use the
literal @1.0@.

== Grammar

@
version := \"MIME-Version\" \":\" 1*DIGIT \".\" 1*DIGIT
@

Spec: <https://www.rfc-editor.org/rfc/rfc2045#section-4>

See also: "Network.HTTP.Headers.ContentType", "Network.HTTP.Headers.ContentEncoding", "Network.HTTP.Headers.ContentDisposition".
-}
module Network.HTTP.Headers.MIMEVersion (
  MIMEVersion (..),
  mimeVersionParser,
  renderMIMEVersion,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hMIMEVersion)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | A MIME protocol version: @major.minor@ (e.g. @1.0@).
data MIMEVersion = MIMEVersion
  { mimeVersionMajor :: !Int
  , mimeVersionMinor :: !Int
  }
  deriving stock (Eq, Show)


instance KnownHeader MIMEVersion where
  type ParseFailure MIMEVersion = String
  type Cardinality MIMEVersion = 'ZeroOrOne
  type Direction MIMEVersion = 'RequestAndResponse


  parseFromHeaders _ headers = case runParser mimeVersionParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing MIME-Version header: " <> show rest
    Fail -> Left "Failed to parse MIME-Version header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderMIMEVersion


  headerName _ = hMIMEVersion


mimeVersionParser :: ParserT st String MIMEVersion
mimeVersionParser = do
  ows
  major <- anyAsciiDecimalInt
  ows
  $(char '.')
  ows
  minor <- anyAsciiDecimalInt
  pure MIMEVersion {mimeVersionMajor = major, mimeVersionMinor = minor}


renderMIMEVersion :: MIMEVersion -> M.Builder
renderMIMEVersion (MIMEVersion major minor) =
  M.intDec major <> M.char7 '.' <> M.intDec minor
