{-# LANGUAGE TemplateHaskell #-}

{- | RFC 2660 §4.1 @Security-Scheme@ advertises support for S-HTTP/1.4.
The scheme is retained as a case-insensitively matched literal.

Spec: <https://www.rfc-editor.org/rfc/rfc2660.html#section-4.1>
-}
module Network.HTTP.Headers.SecurityScheme (
  SecurityScheme (..),
  secureHTTP14,
  securitySchemeParser,
  renderSecurityScheme,
) where

import Data.Bits ((.|.))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (shortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSecurityScheme)
import qualified Network.HTTP.Headers.Mason as M


newtype SecurityScheme = SecurityScheme {securitySchemeValue :: ST.ShortText}
  deriving stock (Eq, Show)


-- | The S-HTTP/1.4 advertisement in its standard spelling.
secureHTTP14 :: SecurityScheme
secureHTTP14 = SecurityScheme "S-HTTP/1.4"


instance KnownHeader SecurityScheme where
  type ParseFailure SecurityScheme = String
  type Cardinality SecurityScheme = 'ZeroOrOne
  type Direction SecurityScheme = 'RequestAndResponse


  parseFromHeaders _ ("S-HTTP/1.4" :| []) = Right secureHTTP14
  parseFromHeaders _ fields = parseSingleFieldLine "Security-Scheme" securitySchemeParser fields


  renderToHeaders _ = ST.toByteString . securitySchemeValue


  renderToHeadersChecked _ (SecurityScheme text)
    | text == "S-HTTP/1.4" = Just "S-HTTP/1.4"
    | otherwise =
        let wire = ST.toByteString text
        in either (const Nothing) (const (Just wire)) $
             parseSingleFieldLine "Security-Scheme" securitySchemeVersion (wire :| [])


  headerName _ = hSecurityScheme


securitySchemeParser :: ParserT st String SecurityScheme
securitySchemeParser = SecurityScheme <$> shortASCIIFromParser_ securitySchemeVersion


securitySchemeVersion :: ParserT st String ()
securitySchemeVersion = do
  prefix <- anyWord64be
  -- Fold only the letters of "S-HTTP/1".
  if prefix .|. 0x2000202020200000 == 0x732d687474702f31
    then $(string ".4")
    else failed


renderSecurityScheme :: SecurityScheme -> M.Builder
renderSecurityScheme = shortText . securitySchemeValue
