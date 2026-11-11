{- | Resolve Editions field presence before either generator chooses field
carriers. The public parser retains the original source-level labels.
-}
module Proto.Internal.FieldPresence (resolveFieldPresence, explicitFieldPresence, repeatedFieldPacked, enumIsClosed, messageEncodingDelimited, verifyStringUtf8, featureOption) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.List (find)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Proto.IDL.AST
import Proto.IDL.Annotations (lookupSimpleOption, optionAsBool)


-- | Whether the label calls for a presence-tracking carrier.
explicitFieldPresence :: Maybe FieldLabel -> Bool
explicitFieldPresence = \case
  Just Optional -> True
  Just Required -> True
  _ -> False


resolveFieldPresence :: ProtoFile -> Either Text ProtoFile
resolveFieldPresence pf = case protoSyntax pf of
  Editions edition -> do
    filePresence <- presenceOption (featureFieldPresence (featuresForEdition edition)) (protoOptions pf)
    when (filePresence == LegacyRequired) $
      Left "features.field_presence = LEGACY_REQUIRED cannot be a file default"
    topLevels <- traverse (topLevel filePresence) (protoTopLevels pf)
    pure pf {protoTopLevels = topLevels}
  _ -> Right pf
  where
    topLevel presence = \case
      TLMessage msg -> TLMessage <$> message presence msg
      TLExtend name fields -> TLExtend name <$> traverse extension fields
      other -> Right other
    message presence msg = do
      elements <- traverse (element presence) (msgElements msg)
      pure msg {msgElements = elements}
    element presence entry = case entry of
      MEField fd -> MEField <$> field presence fd
      MEMessage msg -> MEMessage <$> message presence msg
      MEExtend name fields -> MEExtend name <$> traverse extension fields
      MEMapField mf -> entry <$ rejectPresence "Map fields" (mapOptions mf)
      MEOneof oneof -> entry <$ traverse_ (rejectPresence "Oneof fields" . oneofFieldOptions) (oneofFields oneof)
      other -> Right other
    field presence fd = case fieldLabel fd of
      Just Repeated -> fd <$ rejectPresence "Repeated fields" (fieldOptions fd)
      _ -> do
        resolved <- presenceOption presence (fieldOptions fd)
        pure
          fd
            { fieldLabel = case resolved of
                ExplicitPresence -> Just Optional
                ImplicitPresence -> Nothing
                LegacyRequired -> Just Required
            }
    extension fd = do
      rejectPresence "Extension fields" (fieldOptions fd)
      pure fd {fieldLabel = if fieldLabel fd == Just Repeated then Just Repeated else Just Optional}
    rejectPresence target opts = when (isJust (featureOption "field_presence" opts)) $
      Left (target <> " cannot specify features.field_presence")


-- Field presence is scoped to files and fields, not enclosing messages.
-- Accept both dotted and aggregate feature-option spellings.
presenceOption :: FieldPresenceFeature -> [OptionDef] -> Either Text FieldPresenceFeature
presenceOption fallback opts = case featureOption "field_presence" opts of
  Nothing -> Right fallback
  Just (CIdent "EXPLICIT") -> Right ExplicitPresence
  Just (CIdent "IMPLICIT") -> Right ImplicitPresence
  Just (CIdent "LEGACY_REQUIRED") -> Right LegacyRequired
  Just _ -> Left "features.field_presence must be EXPLICIT, IMPLICIT, or LEGACY_REQUIRED"


-- | Pick the write representation for a packable repeated field.
-- Readers accept both representations independently of this setting.
repeatedFieldPacked :: Syntax -> [OptionDef] -> [OptionDef] -> Either Text Bool
repeatedFieldPacked syntax fileOpts fieldOpts = case syntax of
  Proto2 -> Right (fromMaybe False legacy)
  Proto3 -> Right (fromMaybe True legacy)
  Editions edition ->
    case packingFeature fieldOpts <|> packingFeature fileOpts of
      Nothing -> Right (fromMaybe defaultPacked legacy)
      Just (CIdent "PACKED") -> Right True
      Just (CIdent "EXPANDED") -> Right False
      Just _ -> Left "features.repeated_field_encoding must be PACKED or EXPANDED"
    where
      defaultPacked = featureRepeatedFieldEncoding (featuresForEdition edition) == PackedEncoding
  where
    legacy = lookupSimpleOption "packed" fieldOpts >>= optionAsBool
    packingFeature = featureOption "repeated_field_encoding"


-- | Enum openness is declared by the file defining the enum.
enumIsClosed :: Syntax -> [OptionDef] -> [OptionDef] -> Either Text Bool
enumIsClosed syntax fileOpts enumOpts = case syntax of
  Proto2 -> Right True
  Proto3 -> Right False
  Editions edition -> case featureOption "enum_type" enumOpts <|> featureOption "enum_type" fileOpts of
    Nothing -> Right (featureEnumType (featuresForEdition edition) == ClosedEnum)
    Just (CIdent "OPEN") -> Right False
    Just (CIdent "CLOSED") -> Right True
    Just _ -> Left "features.enum_type must be OPEN or CLOSED"


-- | Resolve the wire encoding of a message field. Map entries and
-- their values always use length-prefixing, independent of file options.
messageEncodingDelimited :: Syntax -> [OptionDef] -> [OptionDef] -> Either Text Bool
messageEncodingDelimited syntax fileOpts fieldOpts = case syntax of
  Editions edition -> case encoding fieldOpts <|> encoding fileOpts of
    Nothing -> Right (featureMessageEncoding (featuresForEdition edition) == DelimitedEncoding)
    Just (CIdent "LENGTH_PREFIXED") -> Right False
    Just (CIdent "DELIMITED") -> Right True
    Just _ -> Left "features.message_encoding must be LENGTH_PREFIXED or DELIMITED"
  _ -> Right False
  where
    encoding = featureOption "message_encoding"


verifyStringUtf8 :: Syntax -> [OptionDef] -> [OptionDef] -> Either Text Bool
verifyStringUtf8 syntax fileOpts fieldOpts = case syntax of
  Proto2 -> Right False
  Proto3 -> Right True
  Editions edition -> case validation fieldOpts <|> validation fileOpts of
    Nothing -> Right (featureUtf8Validation (featuresForEdition edition) == Utf8Verify)
    Just (CIdent "VERIFY") -> Right True
    Just (CIdent "NONE") -> Right False
    Just _ -> Left "features.utf8_validation must be VERIFY or NONE"
  where
    validation = featureOption "utf8_validation"


featureOption :: Text -> [OptionDef] -> Maybe Constant
featureOption name opts =
  fmap optValue (find matches opts) <|> case lookupSimpleOption "features" opts of
    Just (CAggregate values) -> lookup name values
    _ -> Nothing
  where
    matches option = case optNameParts (optName option) of
      [SimpleOption "features", SimpleOption part] -> part == name
      [SimpleOption part] -> part == "features." <> name
      _ -> False
