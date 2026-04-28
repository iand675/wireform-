-- | Convert parsed Avro IDL AST to the standard Avro schema and protocol types.
--
-- Transforms the intermediate 'Avro.IDL.AvroIDL' representation into
-- the canonical 'Avro.Schema.AvroType' and 'Avro.Protocol.AvroProtocol'
-- types that the rest of the library works with.
--
-- @
-- import Avro.IDL (parseAvroIDL)
-- import Avro.IDLConvert (idlToProtocol)
--
-- let Right idl = parseAvroIDL input
-- let protocol = idlToProtocol idl
-- @
module Avro.IDLConvert
  ( idlToProtocol
  , idlToType
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Vector as V

import Avro.IDL
import Avro.Protocol (AvroProtocol(..), AvroMessage(..), AvroParam(..))
import Avro.Schema (AvroType(..), AvroSchema(..), AvroField(..), SortOrder(..), LogicalType(..))

-- | Convert a parsed 'AvroIDL' protocol to the standard 'AvroProtocol'.
idlToProtocol :: AvroIDL -> AvroProtocol
idlToProtocol idl = AvroProtocol
  { protoName      = aidlProtocolName idl
  , protoNamespace = aidlNamespace idl
  , protoDoc       = Nothing
  , protoTypes     = map idlToType (V.toList (aidlDeclarations idl))
  , protoMessages  = map convertMessage (V.toList (aidlMessages idl))
  }

-- | Convert an IDL declaration to an 'AvroType'.
idlToType :: AvroIDLDecl -> AvroType
idlToType (IDLRecord name fields doc aliases) =
  AvroRecord
    { avroRecordName      = name
    , avroRecordNamespace  = Nothing
    , avroRecordDoc        = doc
    , avroRecordAliases    = aliases
    , avroRecordFields     = V.map convertField fields
    , avroRecordProps      = Map.empty
    }
idlToType (IDLEnum name syms doc) =
  AvroEnum
    { avroEnumName      = name
    , avroEnumNamespace  = Nothing
    , avroEnumDoc        = doc
    , avroEnumAliases    = V.empty
    , avroEnumSymbols    = syms
    , avroEnumDefault    = Nothing
    }
idlToType (IDLFixed name size) =
  AvroFixed
    { avroFixedName      = name
    , avroFixedNamespace  = Nothing
    , avroFixedSize       = size
    , avroFixedAliases    = V.empty
    }
idlToType (IDLError name fields doc) =
  AvroRecord
    { avroRecordName      = name
    , avroRecordNamespace  = Nothing
    , avroRecordDoc        = doc
    , avroRecordAliases    = V.empty
    , avroRecordFields     = V.map convertField fields
    , avroRecordProps      = Map.singleton "error" "true"
    }

convertField :: AvroIDLField -> AvroField
convertField f = AvroField
  { avroFieldName    = ifdName f
  , avroFieldType    = convertType (ifdType f)
  , avroFieldDefault = convertDefault (ifdDefault f) (ifdType f)
  , avroFieldOrder   = convertOrder (ifdOrder f)
  , avroFieldAliases = V.empty
  , avroFieldDoc     = ifdDoc f
  , avroFieldProps   = convertAnnotations (ifdAnnotations f)
  }

convertType :: AvroIDLType -> AvroType
convertType ITNull    = AvroPrimitive AvroNull
convertType ITBoolean = AvroPrimitive AvroBool
convertType ITInt     = AvroPrimitive AvroInt
convertType ITLong    = AvroPrimitive AvroLong
convertType ITFloat   = AvroPrimitive AvroFloat
convertType ITDouble  = AvroPrimitive AvroDouble
convertType ITBytes   = AvroPrimitive AvroBytes
convertType ITString  = AvroPrimitive AvroString
convertType (ITArray inner) = AvroArray (convertType inner)
convertType (ITMap inner) = AvroMap (convertType inner)
convertType (ITUnion branches) = AvroUnion (V.map convertType branches)
convertType (ITNamed name) = AvroPrimitive (AvroSchemaRef name)
convertType (ITDecimal prec scl) =
  AvroLogical
    { avroLogicalBase = AvroPrimitive AvroBytes
    , avroLogicalType = DecimalLogical prec scl
    }
convertType ITDate =
  AvroLogical
    { avroLogicalBase = AvroPrimitive AvroInt
    , avroLogicalType = DateLogical
    }
convertType ITTimeMillis =
  AvroLogical
    { avroLogicalBase = AvroPrimitive AvroInt
    , avroLogicalType = TimeMillisLogical
    }
convertType ITTimestampMillis =
  AvroLogical
    { avroLogicalBase = AvroPrimitive AvroLong
    , avroLogicalType = TimestampMillisLogical
    }
convertType ITLocalTimestampMillis =
  AvroLogical
    { avroLogicalBase = AvroPrimitive AvroLong
    , avroLogicalType = CustomLogical "local-timestamp-millis"
    }
convertType ITUuid =
  AvroLogical
    { avroLogicalBase = AvroPrimitive AvroString
    , avroLogicalType = UuidLogical
    }

convertDefault :: Maybe Text -> AvroIDLType -> Maybe AvroSchema
convertDefault Nothing _       = Nothing
convertDefault (Just "null") _ = Just AvroNull
convertDefault (Just _) _     = Just AvroNull

convertOrder :: Maybe Text -> Maybe SortOrder
convertOrder Nothing            = Nothing
convertOrder (Just "ascending")  = Just Ascending
convertOrder (Just "descending") = Just Descending
convertOrder (Just "ignore")     = Just Ignore
convertOrder (Just _)           = Nothing

convertAnnotations :: V.Vector (Text, Text) -> Map.Map Text Text
convertAnnotations = Map.fromList . V.toList

convertMessage :: AvroIDLMessage -> (Text, AvroMessage)
convertMessage msg = (imName msg, AvroMessage
  { msgRequest  = map convertParam (V.toList (imParams msg))
  , msgResponse = convertType (imReturn msg)
  , msgErrors   = if V.null (imErrors msg) then Nothing
                  else Just (AvroUnion (V.map (\e -> AvroPrimitive (AvroSchemaRef e)) (imErrors msg)))
  , msgOneWay   = imOneway msg
  })

convertParam :: (AvroIDLType, Text) -> AvroParam
convertParam (ty, name) = AvroParam
  { paramName = name
  , paramType = convertType ty
  }
