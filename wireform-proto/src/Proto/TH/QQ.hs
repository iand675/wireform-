{- | QuasiQuoter for inline protobuf definitions.

Usage:

@
{\-\# LANGUAGE QuasiQuotes \#-\}
{\-\# LANGUAGE TemplateHaskell \#-\}
import Proto.TH.QQ

[proto|
  syntax = "proto3";
  message SearchRequest {
    string query = 1;
    int32 page_number = 2;
    int32 result_per_page = 3;
  }
|]

-- Now SearchRequest is a regular Haskell type with encode\/decode instances.
@
-}
module Proto.TH.QQ (
  proto,
) where

import Data.Text qualified as T
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Proto.IDL.Parser (parseProtoFile, renderParseError)
import Proto.TH (protoFileToDecls)


{- | QuasiQuoter for inline protobuf definitions.

Parses the proto IDL at compile time and generates data types
and typeclass instances in the current module.

Only the declaration splice position is supported (top-level).
-}
proto :: QuasiQuoter
proto =
  QuasiQuoter
    { quoteExp = \_ -> fail "proto quasiquoter can only be used for declarations"
    , quotePat = \_ -> fail "proto quasiquoter can only be used for declarations"
    , quoteType = \_ -> fail "proto quasiquoter can only be used for declarations"
    , quoteDec = protoDec
    }


protoDec :: String -> Q [Dec]
protoDec src = do
  let txt = T.pack src
  case parseProtoFile "<quasiquote>" txt of
    Left err -> fail (renderParseError err)
    Right pf -> protoFileToDecls pf
