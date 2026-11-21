{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Framing (tests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (chr)
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers
import Network.HTTP.Headers.AltUsed (AltUsed (..))
import qualified Network.HTTP.Headers.Close as Close
import qualified Network.HTTP.Headers.Connection as Connection
import Network.HTTP.Headers.ContentLength (ContentLength (..))
import Network.HTTP.Headers.HeaderFieldName (hContentLength, hHost, hTE, hTransferEncoding)
import Network.HTTP.Headers.Host (Host (..))
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.MaxForwards as MaxForwards
import Network.HTTP.Headers.Parsing.Util (Result (..), eof, httpQuotedString, runParser)
import Network.HTTP.Headers.Rendering.Util (renderHttpQuotedString)
import qualified Network.HTTP.Headers.TE as TE
import qualified Network.HTTP.Headers.Trailer as Trailer
import qualified Network.HTTP.Headers.TransferEncoding as Transfer
import qualified Network.HTTP.Headers.Upgrade as Upgrade
import qualified Network.HTTP.Headers.Via as Via
import Test.Syd
import Test.Syd.Hedgehog ()


-- Helpers -----------------------------------------------------------------

finish :: Result String a -> Either String a
finish = \case
  OK v leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err e -> Left e


parseTE :: ByteString -> Either String TE.TE
parseTE = finish . runParser TE.teParser


parseTrailer :: ByteString -> Either String Trailer.Trailer
parseTrailer = finish . runParser Trailer.trailerParser


parseUpgrade :: ByteString -> Either String Upgrade.Upgrade
parseUpgrade = finish . runParser Upgrade.upgradeParser


parseVia :: ByteString -> Either String Via.Via
parseVia = finish . runParser Via.viaParser


parseMaxForwards :: ByteString -> Either String MaxForwards.MaxForwards
parseMaxForwards = finish . runParser MaxForwards.maxForwardsParser


parseClose :: ByteString -> Either String Close.Close
parseClose = finish . runParser Close.closeParser


roundtrip
  :: (Eq a, Show a)
  => Gen a
  -> (ByteString -> Either String a)
  -> (a -> M.Builder)
  -> Property
roundtrip gen parse render = property $ do
  x <- forAll gen
  let bs = M.toStrictByteString (render x)
  case parse bs of
    Right y -> y === x
    Left e -> error (e <> " on " <> show bs)


-- Generators --------------------------------------------------------------

genToken :: Gen ST.ShortText
genToken = ST.fromString <$> Gen.string (Range.linear 1 8) (Gen.element tokenChars)
  where
    tokenChars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-._"


genNE :: Gen a -> Gen (NonEmpty a)
genNE g = (:|) <$> g <*> Gen.list (Range.linear 0 3) g


genWeight :: Gen (Maybe Double)
genWeight = Gen.maybe (Gen.element [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9])


genTE :: Gen TE.TE
genTE = TE.TE <$> genNE (TE.TECoding <$> genToken <*> genWeight)


genTrailer :: Gen Trailer.Trailer
genTrailer = Trailer.Trailer <$> genNE genToken


genUpgrade :: Gen Upgrade.Upgrade
genUpgrade = Upgrade.Upgrade <$> genNE (Upgrade.Protocol <$> genToken <*> Gen.maybe genToken)


genReceivedBy :: Gen ST.ShortText
genReceivedBy =
  Gen.choice
    [ genToken
    , do
        h <- genToken
        p <- Gen.int (Range.linear 1 65535)
        pure (h <> ST.fromString (':' : show p))
    ]


genComment :: Gen (Maybe Text)
genComment = Gen.maybe (T.pack <$> Gen.string (Range.linear 1 10) (Gen.element commentChars))
  where
    commentChars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> " -._"


genVia :: Gen Via.Via
genVia = Via.Via <$> genNE entry
  where
    entry =
      Via.ViaEntry
        <$> (Via.ReceivedProtocol <$> Gen.maybe genToken <*> genToken)
        <*> genReceivedBy
        <*> genComment


genMaxForwards :: Gen MaxForwards.MaxForwards
genMaxForwards = MaxForwards.MaxForwards <$> Gen.int (Range.linear 0 1_000_000)


-- TE ----------------------------------------------------------------------

unit_te_parse :: Spec
unit_te_parse = it "parses transfer-codings with weights" $
  case parseTE "gzip, trailers, deflate;q=0.5" of
    Right (TE.TE codings) ->
      map (\c -> (TE.teCodingName c, TE.teCodingWeight c)) (NE.toList codings)
        `shouldBe` [ (ST.fromString "gzip", Nothing)
                   , (ST.fromString "trailers", Nothing)
                   , (ST.fromString "deflate", Just 0.5)
                   ]
    other -> error (show other)


unit_te_render :: Spec
unit_te_render =
  it "renders TE" $
    let v = TE.TE (TE.TECoding (ST.fromString "trailers") Nothing :| [TE.TECoding (ST.fromString "gzip") (Just 0.5)])
    in M.toStrictByteString (TE.renderTE v) `shouldBe` "trailers, gzip;q=0.5"


unit_te_parameters :: Spec
unit_te_parameters = it "combines TE fields and preserves extension parameters and weights" $ do
  let params = Transfer.TransferParameter "note" (Transfer.TransferQuoted "a,b") :| []
      value = TE.TE (TE.TECoding "Trailers" Nothing :| [TE.TEParameterizedCoding "custom" (Just 1) params, TE.TECoding "gzip" (Just 0)])
      fields = headerMapFromList [("TE", " , Trailers,"), ("TE", "custom ; note \t= \"a,b\";Q=1., gzip;q=0.")]
  lookupHeader fields `shouldBe` Right (Just value)
  lookupRawHeader hTE (setHeader value fields) `shouldBe` Just ("Trailers, custom;note=\"a,b\";q=1, gzip;q=0" :| [])
  lookupHeader @TE.TE (headerMapFromList [("TE", " , "), ("TE", "")]) `shouldBe` Right (Just TE.TEEmpty)
  renderToHeaders defaultHeaderSettings TE.TEEmpty `shouldBe` [""]


unit_te_rejects_invalid_weights :: Spec
unit_te_rejects_invalid_weights =
  it "enforces TE keyword, coding parameter, and qvalue rules" $
    mapM_
      reject
      [ "ChUnKeD"
      , "trailers;q=1"
      , "trailers;hint=token"
      , "gzip;level=1"
      , "custom;q=1;Q=0"
      , "custom;q=1;level=1"
      , "custom;q = 1"
      , "custom;q=\"1\""
      , "custom;q=0.0001"
      , "custom;q=1.001"
      , "custom;q=2"
      ]
  where
    reject wire = lookupHeader @TE.TE (headerMapFromList [("TE", wire)]) `shouldSatisfy` isLeft


unit_te_decimal_values :: Spec
unit_te_decimal_values =
  it "round trips every representable TE qvalue" $
    mapM_ check [0 .. 1000 :: Int]
  where
    check thousandths =
      let value = TE.TE (TE.TECoding "gzip" (Just (fromIntegral thousandths / 1000)) :| [])
      in parseTE (M.toStrictByteString (TE.renderTE value)) `shouldBe` Right value


unit_http_quoted_octets :: Spec
unit_http_quoted_octets =
  it "checks every HTTP quoted-string octet against the wire grammar" $
    mapM_ check [0 .. 255]
  where
    check byte = do
      let allowed = byte == 9 || byte >= 32 && byte /= 127
          special = byte == 34 || byte == 92
          content = T.singleton (chr (fromIntegral byte))
          directly = BS.pack [34, byte, 34]
          quoted = BS.pack [34, 92, byte, 34]
      decode directly `shouldBe` if allowed && not special then Just content else Nothing
      decode quoted `shouldBe` if allowed then Just content else Nothing
      if allowed
        then M.toStrictByteString (renderHttpQuotedString content) `shouldBe` if special then quoted else directly
        else pure ()
    decode bytes = case runParser (httpQuotedString <* eof) bytes of
      OK value "" -> Just value
      _ -> Nothing


unit_transfer_coding_order :: Spec
unit_transfer_coding_order = it "combines Transfer-Encoding lines before applying framing rules" $ do
  let fields = headerMapFromList [("Transfer-Encoding", " gzip, "), ("Transfer-Encoding", ", Chunked ")]
      value = Transfer.TransferEncoding ("gzip" :| ["Chunked"])
      closeDelimited = Transfer.TransferEncoding ("chunked" :| ["gzip"])
  lookupHeader fields `shouldBe` Right (Just value)
  lookupRawHeader hTransferEncoding (setHeader value fields) `shouldBe` Just ("gzip, Chunked" :| [])
  Transfer.validateRequestTransferEncoding value `shouldBe` Right ()
  lookupHeader @Transfer.TransferEncoding (headerMapFromList [("Transfer-Encoding", "chunked, gzip")])
    `shouldBe` Right (Just closeDelimited)
  Transfer.transferEncodingEndsInChunked closeDelimited `shouldBe` False
  Transfer.validateRequestTransferEncoding closeDelimited `shouldSatisfy` isLeft
  lookupHeader @Transfer.TransferEncoding (headerMapFromList [("Transfer-Encoding", " , ")])
    `shouldBe` Right (Just Transfer.TransferEncodingEmpty)
  Transfer.validateRequestTransferEncoding Transfer.TransferEncodingEmpty `shouldSatisfy` isLeft
  lookupHeader @Transfer.TransferEncoding (headerMapFromList [("Transfer-Encoding", "gzip-custom, chunked-extra, chunked")])
    `shouldBe` Right (Just (Transfer.TransferEncoding ("gzip-custom" :| ["chunked-extra", "chunked"])))


unit_transfer_coding_parameters :: Spec
unit_transfer_coding_parameters = it "retains quoted transfer parameter bytes and rejects invalid standard codings" $ do
  let quoted = "a, \\\"b\\\" " <> BS.singleton 0xff
      fields = headerMapFromList [("Transfer-Encoding", "example; mode\t = fast; note=\"" <> quoted <> "\""), ("Transfer-Encoding", "chunked")]
      params =
        [ Transfer.TransferParameter "mode" (Transfer.TransferToken "fast")
        , Transfer.TransferParameter "note" (Transfer.TransferQuoted "a, \"b\" \xff")
        ]
      value = Transfer.ParameterizedTransferEncoding (Transfer.TransferCoding "example" params :| [Transfer.TransferCoding "chunked" []])
  lookupHeader fields `shouldBe` Right (Just value)
  renderToHeaders defaultHeaderSettings value `shouldBe` ("example;mode=fast;note=\"" <> quoted <> "\", chunked")
  Transfer.validateRequestTransferEncoding value `shouldBe` Right ()
  mapM_
    (\wire -> lookupHeader @Transfer.TransferEncoding (headerMapFromList [("Transfer-Encoding", wire)]) `shouldSatisfy` isLeft)
    ["chunked;hint=1", "GZip;level=1, chunked", "chunked, GZIP, CHUNKED", "custom;mode=", "custom;mode=\"unfinished"]
  lookupHeader @Transfer.TransferEncoding (headerMapFromList [("Transfer-Encoding", "chunked"), ("Transfer-Encoding", "CHUNKED")])
    `shouldSatisfy` isLeft


-- Trailer -----------------------------------------------------------------

unit_transfer_checked_write :: Spec
unit_transfer_checked_write = it "rejects invalid authored transfer codings before replacing field lines" $ do
  let fields = headerMapFromList [("Transfer-Encoding", "gzip"), ("Transfer-Encoding", "chunked")]
      plain names = Transfer.TransferEncoding ("chunked" :| names)
      param name key value =
        Transfer.ParameterizedTransferEncoding
          (Transfer.TransferCoding name [Transfer.TransferParameter key value] :| [Transfer.TransferCoding "chunked" []])
      token = Transfer.TransferToken
  forM_
    [ plain ["CHUNKED"]
    , plain [""]
    , plain ["gzip, example"]
    , param "GZip" "level" (token "2")
    , param "CHUNKED" "level" (token "2")
    , param "ext" "bad name" (token "2")
    , param "ext" "level" (token "")
    , param "ext" "level" (token "one;two")
    , param "ext" "label" (Transfer.TransferQuoted "a\DELb")
    ]
    $ \value -> do
      renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
      setHeader value fields `shouldBe` fields
  let response = Transfer.TransferEncoding ("chunked" :| ["gzip"])
  renderHeaderLines defaultHeaderSettings response `shouldBe` Just ("chunked, gzip" :| [])
  renderHeaderLines defaultHeaderSettings Transfer.TransferEncodingEmpty `shouldBe` Just ("" :| [])


unit_te_checked_write :: Spec
unit_te_checked_write = it "rejects invalid authored TE members while preserving optional weight syntax" $ do
  let fields = headerMapFromList [("TE", "trailers"), ("TE", "gzip;q=0.5")]
      te member = TE.TE (TE.TECoding "trailers" Nothing :| [member])
      param name key value =
        TE.TEParameterizedCoding name (Just 0.5) (Transfer.TransferParameter key value :| [])
      token = Transfer.TransferToken
  forM_
    [ TE.TECoding "CHUNKED" Nothing
    , TE.TECoding "TRAILERS" (Just 1)
    , TE.TECoding "gzip, example" Nothing
    , param "Trailers" "foo" (token "bar")
    , param "X-GZIP" "foo" (token "bar")
    , param "example" "Q" (token "1")
    , param "example" "note" (Transfer.TransferQuoted "a\DELb")
    ]
    $ \member -> do
      let value = te member
      renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
      setHeader value fields `shouldBe` fields
  renderHeaderLines defaultHeaderSettings (te (TE.TECoding "GZip" (Just 1))) `shouldBe` Just ("trailers, GZip;q=1" :| [])
  renderHeaderLines defaultHeaderSettings TE.TEEmpty `shouldBe` Just ("" :| [])


unit_trailer_parse :: Spec
unit_trailer_parse = it "parses a field-name list" $
  case parseTrailer "Expires, Content-MD5" of
    Right (Trailer.Trailer names) ->
      NE.toList names `shouldBe` [ST.fromString "Expires", ST.fromString "Content-MD5"]
    other -> error (show other)


unit_trailer_render :: Spec
unit_trailer_render =
  it "renders Trailer" $
    let v = Trailer.Trailer (ST.fromString "Expires" :| [ST.fromString "X-Checksum"])
    in M.toStrictByteString (Trailer.renderTrailer v) `shouldBe` "Expires, X-Checksum"


-- Upgrade -----------------------------------------------------------------

unit_upgrade_parse :: Spec
unit_upgrade_parse = it "parses a protocol list" $
  case parseUpgrade "HTTP/2.0, websocket" of
    Right (Upgrade.Upgrade protos) ->
      map (\p -> (Upgrade.protocolName p, Upgrade.protocolVersion p)) (NE.toList protos)
        `shouldBe` [ (ST.fromString "HTTP", Just (ST.fromString "2.0"))
                   , (ST.fromString "websocket", Nothing)
                   ]
    other -> error (show other)


unit_upgrade_render :: Spec
unit_upgrade_render =
  it "renders Upgrade" $
    let v =
          Upgrade.Upgrade
            ( Upgrade.Protocol (ST.fromString "h2c") Nothing
                :| [Upgrade.Protocol (ST.fromString "HTTP") (Just (ST.fromString "1.1"))]
            )
    in M.toStrictByteString (Upgrade.renderUpgrade v) `shouldBe` "h2c, HTTP/1.1"


-- Via ---------------------------------------------------------------------

unit_via_parse :: Spec
unit_via_parse = it "parses hops with optional comment" $
  case parseVia "1.1 vegur, HTTP/1.1 proxy.example.com:8080 (test-proxy)" of
    Right (Via.Via entries) -> case NE.toList entries of
      [e1, e2] -> do
        Via.viaReceivedProtocol e1 `shouldBe` Via.ReceivedProtocol Nothing (ST.fromString "1.1")
        Via.viaReceivedBy e1 `shouldBe` ST.fromString "vegur"
        Via.viaComment e1 `shouldBe` Nothing
        Via.viaReceivedProtocol e2 `shouldBe` Via.ReceivedProtocol (Just (ST.fromString "HTTP")) (ST.fromString "1.1")
        Via.viaReceivedBy e2 `shouldBe` ST.fromString "proxy.example.com:8080"
        Via.viaComment e2 `shouldBe` Just (T.pack "test-proxy")
      other -> error (show other)
    other -> error (show other)


unit_via_render :: Spec
unit_via_render =
  it "renders Via" $
    let v =
          Via.Via
            ( Via.ViaEntry
                (Via.ReceivedProtocol (Just (ST.fromString "HTTP")) (ST.fromString "1.1"))
                (ST.fromString "proxy.example.com")
                (Just (T.pack "fast"))
                :| []
            )
    in M.toStrictByteString (Via.renderVia v) `shouldBe` "HTTP/1.1 proxy.example.com (fast)"


unit_via_typed_lists :: Spec
unit_via_typed_lists = it "combines Via hops and shares the nested comment codec" $ do
  let headers = headerMapFromList [("Via", ", 1.1 proxy (at (edge), west), "), ("Via", "HTTP/2 cache,")]
      value =
        Via.Via
          ( Via.ViaEntry (Via.ReceivedProtocol Nothing "1.1") "proxy" (Just "at (edge), west")
              :| [Via.ViaEntry (Via.ReceivedProtocol (Just "HTTP") "2") "cache" Nothing]
          )
  lookupHeader headers `shouldBe` Right (Just value)
  renderToHeaders defaultHeaderSettings value `shouldBe` ["1.1 proxy (at \\(edge\\), west), HTTP/2 cache"]
  lookupHeader @Via.Via (headerMapFromList [("Via", " , ")]) `shouldBe` Right (Just Via.ViaEmpty)
  lookupHeader @Via.Via (headerMapFromList [("Via", "1.1 proxy\rname")]) `shouldSatisfy` isLeft


unit_content_length :: Spec
unit_content_length = it "bounds Content-Length and checks the complete singleton field" $ do
  let headers = headerMapFromList [("Content-Length", "\t 0042 ")]
      single value = lookupHeader @ContentLength (headerMapFromList [("Content-Length", value)])
      largest = M.toStrictByteString (M.wordDec maxBound)
  lookupHeader headers `shouldBe` Right (Just (ContentLength 42))
  lookupRawHeader hContentLength (setHeader (ContentLength 42) headers) `shouldBe` Just ("42" :| [])
  single largest `shouldBe` Right (Just (ContentLength maxBound))
  single (BS.replicate 1000 48 <> "42") `shouldBe` Right (Just (ContentLength 42))
  mapM_
    (\value -> single value `shouldSatisfy` isLeft)
    [largest <> "0", BS.replicate 1000 57, "42, 42", "4 2", "-1", "+1", ""]
  lookupHeader @ContentLength (headerMapFromList [("Content-Length", "42"), ("Content-Length", "42")])
    `shouldSatisfy` isLeft


unit_host_authority :: Spec
unit_host_authority = it "parses Host literals, escaped registered names, and bounded ports" $ do
  let lookupHost wire = lookupHeader @Host (headerMapFromList [("Host", wire)])
      headers = headerMapFromList [("Host", "\t[2001:DB8::1]:00443 ")]
      host = Host "[2001:DB8::1]" (Just 443)
  lookupHeader headers `shouldBe` Right (Just host)
  lookupRawHeader hHost (setHeader host headers) `shouldBe` Just ("[2001:DB8::1]:443" :| [])
  lookupHost "[::ffff:192.0.2.1]:65535" `shouldBe` Right (Just (Host "[::ffff:192.0.2.1]" (Just 65535)))
  lookupHost (BS.drop 1 "x[2001:DB8::1]") `shouldBe` Right (Just (Host "[2001:DB8::1]" Nothing))
  lookupHost "[::]:" `shouldBe` Right (Just (Host "[::]" Nothing))
  lookupHost "[vF.node:transport]" `shouldBe` Right (Just (Host "[vF.node:transport]" Nothing))
  lookupHost "node%2dname.example:" `shouldBe` Right (Just (Host "node%2dname.example" Nothing))
  lookupHost "%61pp%2d%62.example" `shouldBe` Right (Just (Host "%61pp%2d%62.example" Nothing))
  lookupHost "\t " `shouldBe` Right (Just (Host "" Nothing))
  mapM_
    (\wire -> lookupHost wire `shouldSatisfy` isLeft)
    [ "[127.0.0.1]"
    , "[2001:::1]"
    , "[2001:db8::1"
    , "2001:db8::1"
    , "name%4x"
    , "name%"
    , ":80"
    , "node:65536"
    , "node:" <> BS.replicate 1000 57
    , "node:-1"
    , "node:80 "
        <> "more"
    , "user@node"
    , "node/path"
    ]
  lookupHeader @Host (headerMapFromList [("Host", "first"), ("Host", "second")]) `shouldSatisfy` isLeft


unit_via_authority :: Spec
unit_via_authority = it "accepts an authority or pseudonym for a Via hop" $ do
  let lookupVia wire = lookupHeader @Via.Via (headerMapFromList [("Via", wire)])
      protocol = Via.ReceivedProtocol Nothing "1.1"
      value = Via.Via (Via.ViaEntry protocol "[::1]:443" Nothing :| [Via.ViaEntry protocol "edge#1:8443" Nothing])
  lookupVia "1.1 [::1]:443, 1.1 edge#1:8443" `shouldBe` Right (Just value)
  mapM_
    (\wire -> lookupVia wire `shouldSatisfy` isLeft)
    ["1.1 [::bad::1]", "1.1 host:65536", "1.1 user@host", "1.1 host/path", "1.1 [::1](comment)"]


-- Max-Forwards ------------------------------------------------------------

unit_maxforwards_parse :: Spec
unit_maxforwards_parse =
  it "parses an integer" $
    parseMaxForwards "10" `shouldBe` Right (MaxForwards.MaxForwards 10)


unit_maxforwards_render :: Spec
unit_maxforwards_render =
  it "renders Max-Forwards" $
    M.toStrictByteString (MaxForwards.renderMaxForwards (MaxForwards.MaxForwards 0)) `shouldBe` "0"


unit_maxforwards_bounds :: Spec
unit_maxforwards_bounds = it "checks the supported hop-count range through typed lookup" $ do
  let field wire = ("Max-Forwards", wire)
      typed = lookupHeader @MaxForwards.MaxForwards . headerMapFromList
      wireOf n = BSC.pack (show n)
  typed [field ("\t00" <> wireOf (maxBound :: Int) <> " ")] `shouldBe` Right (Just (MaxForwards.MaxForwards maxBound))
  typed [field "999999999"] `shouldBe` Right (Just (MaxForwards.MaxForwards 999_999_999))
  typed [field "1000000000"] `shouldBe` Right (Just (MaxForwards.MaxForwards 1_000_000_000))
  typed [field "1", field "2"] `shouldSatisfy` isLeft
  mapM_
    (\wire -> typed [field wire] `shouldSatisfy` isLeft)
    [wireOf (toInteger (maxBound :: Int) + 1), "18446744073709551616", "-1", "23x", ""]


unit_maxforwards_invalid_write :: Spec
unit_maxforwards_invalid_write = it "rejects malformed authored Max-Forwards hop counts" $ do
  let original = headerMapFromList [("Max-Forwards", "1"), ("Max-Forwards", "2")]
  mapM_
    ( \value -> do
        renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
        setHeader value original `shouldBe` original
    )
    [MaxForwards.MaxForwards (-1), MaxForwards.MaxForwards minBound]
  renderHeaderLines defaultHeaderSettings (MaxForwards.MaxForwards 0) `shouldBe` Just ("0" :| [])


-- Close -------------------------------------------------------------------

unit_close_parse :: Spec
unit_close_parse = it "parses the close token case-insensitively" $ do
  parseClose "close" `shouldBe` Right Close.Close
  parseClose "Close" `shouldBe` Right Close.Close


unit_close_render :: Spec
unit_close_render =
  it "renders close" $
    M.toStrictByteString (Close.renderClose Close.Close) `shouldBe` "close"


unit_close_connection :: Spec
unit_close_connection = it "reads and writes close as a Connection option" $ do
  let fields = headerMapFromList [("Connection", "keep-alive"), ("Connection", "upgrade, ClOsE"), ("Close", "close")]
      authored = setHeader Close.Close (headerMapFromList [])
  lookupHeader @Close.Close fields `shouldBe` Right (Just Close.Close)
  lookupHeader @Close.Close (headerMapFromList [("Close", "close")]) `shouldBe` Right Nothing
  lookupRawHeader "Connection" authored `shouldBe` Just ("close" :| [])
  lookupRawHeader "Close" authored `shouldBe` Nothing
  fmap (fmap Connection.connectionHasClose) (lookupHeader @Connection.Connection fields) `shouldBe` Right (Just True)
  fmap (fmap Connection.connectionHasClose) (lookupHeader @Connection.Connection (headerMapFromList [("Connection", "upgrade")]))
    `shouldBe` Right (Just False)
  lookupHeader @Close.Close (headerMapFromList [("Connection", "close"), ("Connection", "bad option")])
    `shouldSatisfy` isLeft


-- Suite -------------------------------------------------------------------

checkedAuthorityWrites :: Spec
checkedAuthorityWrites = describe "rejects invalid authored authorities" $ do
  it "Host" $ do
    let previous = headerMapFromList [("Host", "one.example"), ("Host", "two.example")]
    mapM_
      ( \value -> do
          renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
          setHeader value previous `shouldBe` previous
      )
      [Host "" (Just 443), Host "one.example:443" Nothing, Host "user@one.example" Nothing, Host "[bad]" Nothing]
    renderHeaderLines defaultHeaderSettings (Host "" Nothing) `shouldBe` Just ("" :| [])
  it "Alt-Used" $ do
    let previous = headerMapFromList [("Alt-Used", "one.example"), ("Alt-Used", "two.example")]
    mapM_
      ( \value -> do
          renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
          setHeader value previous `shouldBe` previous
      )
      [AltUsed "" Nothing, AltUsed "one.example:443" Nothing, AltUsed "[bad]" Nothing]
  it "Via" $ do
    let previous = headerMapFromList [("Via", "1.1 one"), ("Via", "2 two")]
        protocol = Via.ReceivedProtocol Nothing "1.1"
    mapM_
      ( \entry -> do
          let value = Via.Via (Via.ViaEntry protocol "one" Nothing :| [entry])
          renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
          setHeader value previous `shouldBe` previous
      )
      [ Via.ViaEntry (Via.ReceivedProtocol (Just "HTTP/2") "1.1") "two" Nothing
      , Via.ViaEntry (Via.ReceivedProtocol Nothing "two versions") "two" Nothing
      , Via.ViaEntry protocol "two, 1.1 three" Nothing
      , Via.ViaEntry protocol "[bad]" Nothing
      , Via.ViaEntry protocol "two" (Just "bad\DEL")
      ]


tests :: Spec
tests =
  describe "Framing" $
    sequence_
      [ unit_te_parse
      , unit_te_render
      , unit_te_parameters
      , unit_te_rejects_invalid_weights
      , unit_te_decimal_values
      , unit_http_quoted_octets
      , it "TE round-trip" (roundtrip genTE parseTE TE.renderTE)
      , unit_transfer_coding_order
      , unit_transfer_coding_parameters
      , unit_transfer_checked_write
      , unit_te_checked_write
      , unit_trailer_parse
      , unit_trailer_render
      , it "Trailer round-trip" (roundtrip genTrailer parseTrailer Trailer.renderTrailer)
      , unit_upgrade_parse
      , unit_upgrade_render
      , it "Upgrade round-trip" (roundtrip genUpgrade parseUpgrade Upgrade.renderUpgrade)
      , unit_via_parse
      , unit_via_render
      , it "Via round-trip" (roundtrip genVia parseVia Via.renderVia)
      , unit_via_typed_lists
      , unit_content_length
      , unit_host_authority
      , unit_via_authority
      , checkedAuthorityWrites
      , unit_maxforwards_parse
      , unit_maxforwards_render
      , it "Max-Forwards round-trip" (roundtrip genMaxForwards parseMaxForwards MaxForwards.renderMaxForwards)
      , unit_maxforwards_bounds
      , unit_maxforwards_invalid_write
      , unit_close_parse
      , unit_close_render
      , unit_close_connection
      ]
