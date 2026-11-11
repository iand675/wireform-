{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module THFixtures where

import Data.Map.Strict qualified as Map
import Proto.Repr
  ( FieldRep (..), RepConfig (..), defaultFieldRep, defaultRepConfig
  , lazyBytesAdapter, listAdapter, seqAdapter, shortBytesAdapter, unboxedVectorAdapter
  )
import Proto.TH (LoadOpts (..), defaultLoadOpts, loadProto, loadProtoWith)


$(loadProto "test-regression/proto/edition2023.proto")
$(loadProto "test-regression/proto/edition2024.proto")
$(loadProto "test-regression/proto/json_numbers.proto")


$(loadProto "test-regression/proto/integerMaps.proto")
$(loadProto "test-regression/proto/json_temporal.proto")
$(loadProto "test-regression/proto/rpc.proto")
$(loadProto "test-regression/proto/my__file.proto")
$(loadProtoWith defaultLoadOpts {loIncludeDirs = ["test-regression/proto"]} "test-regression/proto/my_file.proto")
$(loadProtoWith
    defaultLoadOpts
      { loRepConfig =
          defaultRepConfig
            { configFieldOverrides =
                Map.fromList
                  [ (("JsonTypedMaps", "bytes_by_id"), defaultFieldRep {fieldBytes = lazyBytesAdapter})
                  , (("JsonTypedMaps", "bytes_by_flag"), defaultFieldRep {fieldBytes = shortBytesAdapter})
                  , (("JsonRepeatedNumbers", "ratios"), defaultFieldRep {fieldRepeated = listAdapter})
                  , (("JsonRepeatedNumbers", "offsets"), defaultFieldRep {fieldRepeated = seqAdapter})
                  , (("JsonRepeatedNumbers", "fractions"), defaultFieldRep {fieldRepeated = unboxedVectorAdapter})
                  , (("JsonEnumMode", "colors"), defaultFieldRep {fieldRepeated = listAdapter})
                  , (("JsonEnumMode", "more_colors"), defaultFieldRep {fieldRepeated = seqAdapter})
                  ]
            }
      }
    "test-regression/proto/json_collections.proto"
 )
$(loadProtoWith defaultLoadOpts {loIncludeDirs = ["test-regression/proto"]} "test-regression/proto/packing_legacy.proto")
$(loadProto "test-regression/proto/groups.proto")
$(loadProto "test-regression/proto/edition_delimited.proto")
$(loadProto "test-regression/proto/extension_utf8.proto")
$(loadProto "data/proto/google/protobuf/any.proto")
