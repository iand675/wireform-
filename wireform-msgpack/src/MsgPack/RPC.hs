-- | MessagePack-RPC message format (msgpack-rpc specification).
--
-- Three message types:
--
-- * Request:      @[type=0, msgid, method, params]@
-- * Response:     @[type=1, msgid, error, result]@
-- * Notification: @[type=2, method, params]@
module MsgPack.RPC
  ( RPCMessage(..)
  , encodeRPC
  , decodeRPC
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word32, Word64)
import qualified Data.Vector as V

import qualified MsgPack.Value as MV
import MsgPack.Encode (encode)
import MsgPack.Decode (decode)

data RPCMessage
  = RPCRequest !Word32 !Text !(Vector MV.Value)
  | RPCResponse !Word32 !(Maybe MV.Value) !(Maybe MV.Value)
  | RPCNotification !Text !(Vector MV.Value)
  deriving stock (Show, Eq)

encodeRPC :: RPCMessage -> ByteString
encodeRPC (RPCRequest msgid method params) =
  encode $ MV.Array $ V.fromList
    [ MV.Word 0
    , MV.Word (fromIntegral msgid)
    , MV.String method
    , MV.Array params
    ]
encodeRPC (RPCResponse msgid err result) =
  encode $ MV.Array $ V.fromList
    [ MV.Word 1
    , MV.Word (fromIntegral msgid)
    , maybeToValue err
    , maybeToValue result
    ]
encodeRPC (RPCNotification method params) =
  encode $ MV.Array $ V.fromList
    [ MV.Word 2
    , MV.String method
    , MV.Array params
    ]

decodeRPC :: ByteString -> Either String RPCMessage
decodeRPC bs = do
  val <- decode bs
  case val of
    MV.Array arr
      | V.length arr == 4 -> decode4 arr
      | V.length arr == 3 -> decode3 arr
      | otherwise -> Left $ "MsgPack.RPC: expected array of 3 or 4 elements, got " ++ show (V.length arr)
    _ -> Left "MsgPack.RPC: expected array at top level"

decode4 :: Vector MV.Value -> Either String RPCMessage
decode4 arr = do
  ty <- getWord64 (arr V.! 0) "type"
  case ty of
    0 -> do
      msgid <- getWord32 (arr V.! 1) "msgid"
      method <- getText (arr V.! 2) "method"
      params <- getArray (arr V.! 3) "params"
      Right $ RPCRequest msgid method params
    1 -> do
      msgid <- getWord32 (arr V.! 1) "msgid"
      let err = valueToMaybe (arr V.! 2)
          result = valueToMaybe (arr V.! 3)
      -- msgpack-rpc spec: in a Response, exactly one of `error` and
      -- `result` must be Nil. Both-nil is an error-success ambiguity;
      -- both-non-nil is forbidden.
      case (err, result) of
        (Just _, Just _) ->
          Left "MsgPack.RPC: response has both error and result populated"
        _ -> Right ()
      Right $ RPCResponse msgid err result
    _ -> Left $ "MsgPack.RPC: unknown type in 4-element array: " ++ show ty

decode3 :: Vector MV.Value -> Either String RPCMessage
decode3 arr = do
  ty <- getWord64 (arr V.! 0) "type"
  case ty of
    2 -> do
      method <- getText (arr V.! 1) "method"
      params <- getArray (arr V.! 2) "params"
      Right $ RPCNotification method params
    _ -> Left $ "MsgPack.RPC: unknown type in 3-element array: " ++ show ty

getWord64 :: MV.Value -> String -> Either String Word64
getWord64 (MV.Word w) _ = Right w
getWord64 (MV.Int i) _ | i >= 0 = Right (fromIntegral i)
getWord64 v field = Left $ "MsgPack.RPC: expected unsigned int for " ++ field ++ ", got " ++ showType v

getWord32 :: MV.Value -> String -> Either String Word32
getWord32 v field = do
  w <- getWord64 v field
  if w <= fromIntegral (maxBound :: Word32)
    then Right (fromIntegral w)
    else Left $ "MsgPack.RPC: " ++ field ++ " exceeds Word32 range"

getText :: MV.Value -> String -> Either String Text
getText (MV.String t) _ = Right t
getText v field = Left $ "MsgPack.RPC: expected string for " ++ field ++ ", got " ++ showType v

getArray :: MV.Value -> String -> Either String (Vector MV.Value)
getArray (MV.Array a) _ = Right a
getArray v field = Left $ "MsgPack.RPC: expected array for " ++ field ++ ", got " ++ showType v

maybeToValue :: Maybe MV.Value -> MV.Value
maybeToValue Nothing  = MV.Nil
maybeToValue (Just v) = v

valueToMaybe :: MV.Value -> Maybe MV.Value
valueToMaybe MV.Nil = Nothing
valueToMaybe v      = Just v

showType :: MV.Value -> String
showType MV.Nil            = "nil"
showType (MV.Bool _)       = "bool"
showType (MV.Int _)        = "int"
showType (MV.Word _)       = "word"
showType (MV.Float _)      = "float"
showType (MV.Double _)     = "double"
showType (MV.String _)     = "string"
showType (MV.Binary _)     = "binary"
showType (MV.Array _)      = "array"
showType (MV.Map _)        = "map"
showType (MV.Ext _ _)      = "ext"
showType (MV.Timestamp _ _) = "timestamp"
