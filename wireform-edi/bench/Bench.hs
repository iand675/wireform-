{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Criterion.Main
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Vector qualified as V
import EDI.Class
import EDI.Derive (deriveEDI)
import EDI.EDIFACT
import EDI.Value
import GHC.Generics (Generic)
import Wireform.Derive


data Ftx = Ftx
  { first :: !Text
  , second :: !Text
  , third :: !Text
  }
  deriving stock (Generic)
  deriving anyclass (NFData)
{-# ANN Ftx (forBackend backendEDI (rename "FTX")) #-}


deriveEDI ''Ftx


advice :: ServiceStringAdvice
advice = defaultServiceStringAdvice


sample :: Text -> Int -> Interchange
sample payload count =
  Interchange (serviceStringSyntax advice) $
    V.replicate
      count
      ( Segment
          "FTX"
          ( V.fromList
              [ Simple "AAA"
              , Composite (V.fromList ["alpha", "", "beta"])
              , Simple payload
              , Simple ""
              , Simple "end "
              ]
          )
      )


encode :: Interchange -> Either String ByteString
encode = encodeEDIFACTBS advice


encodeTyped :: Ftx -> Either String ByteString
encodeTyped record = encode (toEDI record) {interchangeSyntax = serviceStringSyntax advice}


decodeTyped :: ByteString -> Either String Ftx
decodeTyped input = decodeEDIFACTBS input >>= fromEDI


main :: IO ()
main =
  defaultMain $
    map
      workload
      [ ("plain 1", sample "unescaped payload" 1)
      , ("plain 1000", sample "unescaped payload" 1000)
      , ("released 1", sample "literal ? + : ' and 雪" 1)
      , ("released 1000", sample "literal ? + : ' and 雪" 1000)
      ]
      <> [ env (pure (Ftx "AAA" "literal ? + : ' and 雪" "end ")) $ \record ->
             bgroup
               "typed FTX"
               [ bench "encode" $ nf encodeTyped record
               , env (either fail pure (encodeTyped record)) $ \encoded ->
                   bench "decode" $ nf decodeTyped encoded
               ]
         ]
  where
    workload (name, doc) = env (checked doc) $ \prepared ->
      bgroup
        name
        [ bench "encode" $ nf encode prepared
        , env (either fail pure (encode prepared)) $ \encoded ->
            bench "decode" $ nf decodeEDIFACTBS encoded
        ]


checked :: Interchange -> IO Interchange
checked doc = do
  encoded <- either fail pure (encode doc)
  unless (decodeEDIFACTBS encoded == Right doc) (fail "EDIFACT benchmark fixture does not round-trip")
  pure doc
