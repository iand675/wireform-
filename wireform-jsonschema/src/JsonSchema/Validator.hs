-- | JSON Schema validation engine
--
-- High-performance validation with detailed error reporting using JSON Pointer paths.
-- Supports all JSON Schema versions from draft-04 through 2020-12.
module JsonSchema.Validator
  ( -- * Main Validation Functions
    validateValue
  , validateValueWithRegistry
  , validateValueWithContext
  , compileValidator

    -- * Configuration
  , ValidationConfig(..)
  , defaultValidationConfig
  , strictValidationConfig

    -- * Validator Type
  , Validator(..)

    -- * Errors
  , CompileError(..)
  ) where

import JsonSchema.Types
import qualified JsonSchema.Validator.Result as VR
import qualified JsonSchema.Parser.Internal as ParserInternal
import Control.Applicative ((<|>))
-- Pluggable keyword system
import JsonSchema.Keywords.Standard
  ( standardKeywordRegistry
  , draft04Registry
  , draft06Registry
  , draft07Registry
  , draft201909Registry
  , draft202012Registry
  )
import JsonSchema.Keyword (KeywordRegistry(..), keywordMap, lookupKeyword)
import qualified JsonSchema.Keyword as Keyword
import qualified JsonSchema.Keyword.Types as Keyword.Types
import JsonSchema.Keyword.Types (KeywordNavigation(..), combineValidationResults)
import JsonSchema.Keyword.Compile (compileKeywords, buildCompilationContext, CompiledKeywords(..))
import qualified JsonSchema.Keyword.Validate as KeywordValidate
import JsonSchema.Validator.Annotations
  ( annotateItems
  , annotateProperties
  , arrayIndexPointer
  , propertyPointer
  , shiftAnnotations
  )
-- Applicator keywords (composition and conditional)
import qualified JsonSchema.Keywords.AllOf as AllOf
import qualified JsonSchema.Keywords.AnyOf as AnyOf
import qualified JsonSchema.Keywords.OneOf as OneOf
import qualified JsonSchema.Keywords.Not as Not
import qualified JsonSchema.Keywords.Conditional as Cond
-- Unevaluated keywords
import qualified JsonSchema.Keywords.UnevaluatedProperties as UnevaluatedProperties
import qualified JsonSchema.Keywords.UnevaluatedItems as UnevaluatedItems
import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as Base64
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Scientific as Sci
import Data.Foldable (toList)
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, maybeToList)
import Control.Monad (mplus, guard)
import Data.Vector (Vector, (!), fromList)
import qualified JsonSchema.Regex as Regex
import qualified Data.UUID as UUID
import qualified Data.Time.Format.ISO8601 as Time
import qualified Data.Time.Clock as Time
import qualified Data.Time.Calendar as Time
import qualified Text.Read as Read
import qualified Data.Time.RFC3339 as RFC3339
import qualified Text.Email.Validate as Email
import qualified Network.URI as URI
import qualified Data.IP as IP
import qualified Numeric
import qualified Network.URI.Template.Parser as URITemplate

-- | Error during validator compilation
data CompileError = CompileError Text
  deriving (Eq, Show)

-- | Compiled validator for repeated use
newtype Validator = Validator { runValidator :: Value -> ValidationResult }

-- | Default validation configuration (latest version, format annotation)
defaultValidationConfig :: ValidationConfig
defaultValidationConfig = ValidationConfig
  { validationVersion = Draft202012
  , validationFormatAssertion = False  -- Format as annotation
  , validationDialectFormatBehavior = Nothing  -- No dialect override
  , validationContentAssertion = False  -- Content as annotation
  , validationShortCircuit = False     -- Collect all errors
  , validationCollectAnnotations = False
  , validationCustomValidators = Map.empty
  , validationReferenceLoader = Nothing  -- No external reference loading by default
  }

-- | Strict validation (format assertion, all errors collected)
strictValidationConfig :: ValidationConfig
strictValidationConfig = defaultValidationConfig
  { validationFormatAssertion = True
  , validationDialectFormatBehavior = Just FormatAssertion
  , validationContentAssertion = True
  , validationShortCircuit = False
  }

-- | Validate a value against a schema (without external references)
validateValue :: ValidationConfig -> Schema -> Value -> ValidationResult
validateValue config schema val =
  let -- Build registry by walking the schema tree
      registry = registerSchemaInRegistry (schemaId schema) schema emptyRegistry
      
      ctx = ValidationContext
        { contextConfig = config
        , contextSchemaRegistry = registry  -- Use built registry for $ref resolution
        , contextRootSchema = Just schema   -- Store root schema for local $ref
        , contextBaseURI = schemaId schema
        , contextDynamicScope = []
        , contextEvaluatedProperties = Set.empty
        , contextEvaluatedItems = Set.empty
        , contextVisitedSchemas = Set.empty
        , contextResolvingRefs = Set.empty
        , contextRecursionDepth = 0
        }
  in validateValueWithContext ctx schema val

-- | Validate with a pre-built registry (enables external $ref resolution)
-- Use 'buildRegistryWithExternalRefs' to build the registry with external schemas loaded
validateValueWithRegistry :: ValidationConfig -> SchemaRegistry -> Schema -> Value -> ValidationResult
validateValueWithRegistry config registry schema val =
  let ctx = ValidationContext
        { contextConfig = config
        , contextSchemaRegistry = registry  -- Use provided registry
        , contextRootSchema = Just schema
        , contextBaseURI = schemaId schema
        , contextDynamicScope = []
        , contextEvaluatedProperties = Set.empty
        , contextEvaluatedItems = Set.empty
        , contextVisitedSchemas = Set.empty
        , contextResolvingRefs = Set.empty
        , contextRecursionDepth = 0
        }
  in validateValueWithContext ctx schema val

-- | Validate with explicit context
validateValueWithContext :: ValidationContext -> Schema -> Value -> ValidationResult
validateValueWithContext ctx schema val =
  -- Note: Cycle detection is handled by contextResolvingRefs in validateAgainstObject
  -- We don't use contextVisitedSchemas here because schemas can be legitimately
  -- referenced multiple times (e.g., a schema referencing itself via $ref)
  let ctxWithBase = applySchemaContext ctx schema
  in case schemaCore schema of
       BooleanSchema True -> ValidationSuccess mempty
       BooleanSchema False -> ValidationFailure $ ValidationErrors $ pure $
         validationError "Schema is 'false'"
       ObjectSchema obj ->
         -- First validate against standard keywords, then custom keywords
         combineValidationResults
           [ validateAgainstObject ctx ctxWithBase schema obj val
           , validateCustomKeywords ctxWithBase schema val
           ]

-- | Get the effective vocabularies for a schema based on its metaschema.
-- Returns Nothing if all vocabularies should be active (pre-2019-09 or no vocabulary restrictions).
-- Returns Just (set of vocab URIs) if vocabularies are explicitly declared.
getEffectiveVocabularies :: ValidationContext -> Schema -> Maybe (Set Text)
getEffectiveVocabularies ctx schema =
  case schemaVersion schema of
    -- Pre-2019-09 drafts don't have $vocabulary concept - all keywords active
    Just Draft04 -> Nothing
    Just Draft06 -> Nothing
    Just Draft07 -> Nothing
    -- 2019-09 and later support $vocabulary
    Just Draft201909 -> getVocabulariesFrom201909 ctx schema
    Just Draft202012 -> getVocabulariesFrom202012 ctx schema
    Nothing -> Nothing  -- No version, assume all vocabularies
  where
    getVocabulariesFrom201909 ctx' schema' =
      case schemaVocabulary schema' of
        Just vocabMap -> Just $ Set.fromList (Map.keys vocabMap)
        Nothing -> getVocabulariesFromMetaschema ctx' schema'

    getVocabulariesFrom202012 ctx' schema' =
      case schemaVocabulary schema' of
        Just vocabMap -> Just $ Set.fromList (Map.keys vocabMap)
        Nothing -> getVocabulariesFromMetaschema ctx' schema'

    -- Look up the metaschema in the registry and extract its vocabularies
    getVocabulariesFromMetaschema ctx' schema' =
      case schemaMetaschemaURI schema' of
        Nothing -> Nothing  -- No custom metaschema
        Just metaURI ->
          case Map.lookup metaURI (registrySchemas $ contextSchemaRegistry ctx') of
            Nothing -> Nothing  -- Metaschema not found in registry
            Just metaschema ->
              case schemaVocabulary metaschema of
                Just vocabMap -> Just $ Set.fromList (Map.keys vocabMap)
                Nothing -> Nothing  -- Metaschema has no vocabulary declaration

-- | Check if a vocabulary is active for validation.
-- Returns True if the vocabulary should be used, False if it should be ignored.
isVocabularyActive :: ValidationContext -> Schema -> Text -> Bool
isVocabularyActive ctx schema vocabURI =
  case getEffectiveVocabularies ctx schema of
    Nothing -> True  -- No restrictions, all vocabularies active
    Just vocabs -> Set.member vocabURI vocabs

-- | Validation vocabulary URIs for 2019-09 and 2020-12
validationVocabularyURI201909 :: Text
validationVocabularyURI201909 = "https://json-schema.org/draft/2019-09/vocab/validation"

validationVocabularyURI202012 :: Text
validationVocabularyURI202012 = "https://json-schema.org/draft/2020-12/vocab/validation"

-- | Check if validation vocabulary is active for a schema
-- Uses the root schema from context to determine vocabulary restrictions
isValidationVocabularyActive :: ValidationContext -> Schema -> Bool
isValidationVocabularyActive ctx _schema =
  -- Use the root schema for vocabulary checking, as sub-schemas inherit
  -- vocabulary restrictions from their root
  case contextRootSchema ctx of
    Nothing -> True  -- No root schema, assume validation is active
    Just rootSchema ->
      case schemaVersion rootSchema of
        Just Draft201909 -> isVocabularyActive ctx rootSchema validationVocabularyURI201909
        Just Draft202012 -> isVocabularyActive ctx rootSchema validationVocabularyURI202012
        _ -> True  -- Pre-2019-09 or no version: validation is always active

-- | Apply schema-specific context updates (base URI, root schema)
applySchemaContext :: ValidationContext -> Schema -> ValidationContext
applySchemaContext ctx schema =
  let parentBase = contextBaseURI ctx
      -- Check if the schema's $id has already been applied to the parent base
      -- This happens when following a $ref to a schema with a relative $id
      idAlreadyApplied = case (schemaId schema, parentBase) of
        (Just idText, Just baseText)
          -- If $id is relative and parent base ends with the same path, assume already applied
          | not (T.isPrefixOf "#" idText)
          , not (T.any (== ':') idText) -- No scheme, so it's relative
          , T.isSuffixOf idText baseText ->
              True
        _ -> False
      newBase = if idAlreadyApplied
                  then parentBase  -- Don't recompute, use existing base
                  else schemaEffectiveBase parentBase schema
      baseChanged = newBase /= parentBase
  in ctx
      { contextBaseURI = newBase
      , contextRootSchema =
          let hasOwnId = isJust (schemaId schema)
              shouldSetRoot = (baseChanged || hasOwnId) && not (Map.null (schemaRawKeywords schema))
          in if shouldSetRoot
            then Just schema
            else case contextRootSchema ctx of
                   Just existing -> Just existing
                   Nothing -> Just schema
      }

-- | Compile a validator for repeated use
compileValidator :: ValidationConfig -> Schema -> Either CompileError Validator
compileValidator config schema =
  Right $ Validator $ \val -> validateValue config schema val

-- | Collect metadata annotations from schema (title, description, default, etc.)
collectMetadataAnnotations :: SchemaObject -> ValidationAnnotations
collectMetadataAnnotations obj =
  let anns = schemaAnnotations obj
      annMap = Map.fromList $ catMaybes
        [ ("title",) . Aeson.String <$> annotationTitle anns
        , ("description",) . Aeson.String <$> annotationDescription anns
        , ("default",) <$> annotationDefault anns
        , if null (annotationExamples anns)
            then Nothing
            else Just ("examples", Aeson.Array $ fromList $ annotationExamples anns)
        , ("deprecated",) . Aeson.Bool <$> annotationDeprecated anns
        , ("readOnly",) . Aeson.Bool <$> annotationReadOnly anns
        , ("writeOnly",) . Aeson.Bool <$> annotationWriteOnly anns
        ]
  in if Map.null annMap
       then mempty
       else ValidationAnnotations $ Map.singleton emptyPointer annMap

-- | Validate against object schema
validateAgainstObject :: ValidationContext -> ValidationContext -> Schema -> SchemaObject -> Value -> ValidationResult
validateAgainstObject parentCtx ctx schema obj val =
  let version = validationVersion (contextConfig ctx)
      refIsApplicator = version >= Draft201909
      -- Update context with dynamic scope BEFORE processing refs
      -- Push schema resources (schemas with $id) onto the scope
      -- For 2020-12: push schemas with $dynamicAnchor or schemas with $id
      -- For 2019-09: push schemas with $recursiveAnchor: true or schemas with $id
      shouldPushToScope = case version of
        Draft201909 -> schemaRecursiveAnchor obj == Just True || isJust (contextBaseURI ctx)
        _ -> isJust (schemaDynamicAnchor obj) || isJust (contextBaseURI ctx)
      ctxWithDynamicScope =
        if shouldPushToScope
          then
            let effectiveId = case contextBaseURI ctx of
                  Just base -> Just base
                  Nothing -> schemaId schema
                schema' = schema { schemaId = effectiveId }
            in ctx { contextDynamicScope = schema' : contextDynamicScope ctx }
          else ctx
  in if refIsApplicator
    then
      -- 2019-09+: $ref/$dynamicRef are applicators, combine with other keywords
      -- We need to pass ref annotations to content validation so unevaluatedProperties
      -- can see properties evaluated by the ref
      -- Use ctxWithDynamicScope so refs can see this schema in dynamic scope
      let refResult = validateRef parentCtx ctxWithDynamicScope obj val
          refAnnotations = case refResult of
            ValidationSuccess anns -> anns
            ValidationFailure _ -> mempty
          contentResult = validateObjectSchemaContentWithRefAnnotations ctxWithDynamicScope schema obj val refAnnotations
      in combineValidationResults [refResult, contentResult]
    else
      -- Pre-2019-09: $ref short-circuits, ignores sibling keywords
      case schemaRef obj of
        Just ref ->
          -- RECURSIVE REFERENCE FIX: Same depth-based cycle detection for pre-2019-09 $ref
          if contextRecursionDepth ctx >= 100
            then ValidationSuccess mempty  -- Break recursion at depth limit
            else
              let ctxWithDepth = ctx { contextRecursionDepth = contextRecursionDepth ctx + 1 }
              in case resolveReference ref refCtx of
                Just (resolvedSchema, maybeBase, maybeRootForRefs) ->
                  let ctxForRef = case maybeBase of
                        Just baseDoc ->
                          let baseChanged = contextBaseURI ctxWithDepth /= Just baseDoc
                              -- When base URI changes (remote ref), use the root schema for refs
                              -- This is either the base schema (for fragments) or resolved schema
                              rootValue =
                                if baseChanged
                                  then maybeRootForRefs `mplus` Just resolvedSchema
                                  else case contextRootSchema ctxWithDepth of
                                         Just existing -> Just existing
                                         Nothing -> maybeRootForRefs `mplus` Just resolvedSchema
                          in ctxWithDepth { contextBaseURI = Just baseDoc
                                          , contextRootSchema = rootValue
                                          -- Preserve dynamic scope when following $ref
                                          , contextDynamicScope = contextDynamicScope ctxWithDepth
                                          }
                        Nothing -> ctxWithDepth { contextRootSchema = maybeRootForRefs `mplus` contextRootSchema ctxWithDepth
                                                -- Preserve dynamic scope when following $ref
                                                , contextDynamicScope = contextDynamicScope ctxWithDepth
                                                }
                      -- Update context if crossing draft boundaries
                      ctxForRefWithVersion = updateContextForCrossDraftRef ctxForRef resolvedSchema
                  in validateValueWithContext ctxForRefWithVersion resolvedSchema val
                Nothing ->
                  ValidationFailure $ ValidationErrors $ pure $ VR.ValidationError
                    { VR.errorKeyword = "$ref"
                    , VR.errorSchemaPath = emptyPointer
                    , VR.errorInstancePath = emptyPointer
                    , VR.errorMessage = "Unable to resolve reference: " <> showReference ref
                    }
        Nothing -> validateObjectSchemaContent ctx schema obj val
  where
    refCtx =
      let inheritedBase = contextBaseURI parentCtx
          currentRoot =
            case contextRootSchema ctx of
              Just root -> Just root
              Nothing ->
                case contextRootSchema parentCtx of
                  Just root -> Just root
                  Nothing -> Just schema
      in ctx
           { contextBaseURI = inheritedBase
           , contextRootSchema = currentRoot
           }

    -- Validate $ref and $dynamicRef (treated as applicators in 2019-09+)
    validateRef parentCtx' ctx' obj' val' =
      case schemaRef obj' of
        Just ref ->
          -- RECURSIVE REFERENCE FIX: Check recursion depth limit (prevent infinite recursion)
          -- Previously used Set-based cycle detection which was too aggressive and broke valid
          -- recursive schemas like: tree → node → tree → node → ...
          --
          -- Now using depth-based approach (limit: 100) which allows recursive validation
          -- of nested data structures while still preventing truly infinite recursion.
          -- See detailed design note in Types.hs for ValidationContext.contextRecursionDepth
          if contextRecursionDepth ctx' >= 100
            then ValidationSuccess mempty  -- Break recursion at depth limit
            else
              let ctxWithDepth = ctx' { contextRecursionDepth = contextRecursionDepth ctx' + 1 }
              in case resolveReference ref refCtx of
                Just (resolvedSchema, maybeBase, maybeRootForRefs) ->
                  let ctxForRef = case maybeBase of
                        Just baseDoc ->
                          let baseChanged = contextBaseURI ctxWithDepth /= Just baseDoc
                              -- When base URI changes (remote ref), use the root schema for refs
                              -- This is either the base schema (for fragments) or resolved schema
                              rootValue =
                                if baseChanged
                                  then maybeRootForRefs `mplus` Just resolvedSchema
                                  else case contextRootSchema ctxWithDepth of
                                         Just existing -> Just existing
                                         Nothing -> maybeRootForRefs `mplus` Just resolvedSchema
                          in ctxWithDepth { contextBaseURI = Just baseDoc
                                          , contextRootSchema = rootValue
                                          -- Preserve dynamic scope when following $ref
                                          , contextDynamicScope = contextDynamicScope ctxWithDepth
                                          }
                        Nothing -> ctxWithDepth { contextRootSchema = maybeRootForRefs `mplus` contextRootSchema ctxWithDepth
                                                -- Preserve dynamic scope when following $ref
                                                , contextDynamicScope = contextDynamicScope ctxWithDepth
                                                }
                      -- Update context if crossing draft boundaries
                      ctxForRefWithVersion = updateContextForCrossDraftRef ctxForRef resolvedSchema
                      -- Push resolved schema onto dynamic scope if it has $recursiveAnchor: true (2019-09)
                      -- or $dynamicAnchor (2020-12+)
                      version = validationVersion (contextConfig ctxForRefWithVersion)
                      shouldPushResolved = case version of
                        Draft201909 -> case schemaCore resolvedSchema of
                          ObjectSchema obj -> schemaRecursiveAnchor obj == Just True || isJust (schemaId resolvedSchema)
                          _ -> False
                        _ -> case schemaCore resolvedSchema of
                          ObjectSchema obj -> isJust (schemaDynamicAnchor obj) || isJust (schemaId resolvedSchema)
                          _ -> False
                      ctxWithResolvedScope = if shouldPushResolved
                        then
                          let effectiveId = case contextBaseURI ctxForRefWithVersion of
                                Just base -> Just base
                                Nothing -> schemaId resolvedSchema
                              schema' = resolvedSchema { schemaId = effectiveId }
                          in ctxForRefWithVersion { contextDynamicScope = schema' : contextDynamicScope ctxForRefWithVersion }
                        else ctxForRefWithVersion
                  in validateValueWithContext ctxWithResolvedScope resolvedSchema val'
                Nothing ->
                  ValidationFailure $ ValidationErrors $ pure $ VR.ValidationError
                    { VR.errorKeyword = "$ref"
                    , VR.errorSchemaPath = emptyPointer
                    , VR.errorInstancePath = emptyPointer
                    , VR.errorMessage = "Unable to resolve reference: " <> showReference ref
                    }
        Nothing ->
          -- No $ref, check for $dynamicRef (2020-12+)
          case schemaDynamicRef obj' of
            Just dynRef ->
              -- RECURSIVE REFERENCE FIX: Same depth-based cycle detection for $dynamicRef
              if contextRecursionDepth ctx' >= 100
                then ValidationSuccess mempty  -- Break recursion at depth limit
                else
                  let ctxWithDepth = ctx' { contextRecursionDepth = contextRecursionDepth ctx' + 1 }
                  in case resolveDynamicRef dynRef ctxWithDepth of
                    Just resolvedSchema ->
                      -- Push resolved schema onto dynamic scope if it has $dynamicAnchor
                      let version = validationVersion (contextConfig ctxWithDepth)
                          shouldPushResolved = case schemaCore resolvedSchema of
                            ObjectSchema obj -> isJust (schemaDynamicAnchor obj) || isJust (schemaId resolvedSchema)
                            _ -> False
                          ctxWithResolvedScope = if shouldPushResolved
                            then
                              let effectiveId = case contextBaseURI ctxWithDepth of
                                    Just base -> Just base
                                    Nothing -> schemaId resolvedSchema
                                  resolvedSchemaWithId = resolvedSchema { schemaId = effectiveId }
                              in ctxWithDepth { contextDynamicScope = resolvedSchemaWithId : contextDynamicScope ctxWithDepth }
                            else ctxWithDepth
                      in validateValueWithContext ctxWithResolvedScope resolvedSchema val'
                    Nothing ->
                      -- Fallback: treat as regular $ref (resolve statically)
                      case resolveReference dynRef refCtx of
                        Just (resolvedSchema, maybeBase, maybeRootForRefs) ->
                          let ctxForRef = case maybeBase of
                                Just baseDoc ->
                                  let baseChanged = contextBaseURI ctxWithDepth /= Just baseDoc
                                      -- When base URI changes (remote ref), use the root schema for refs
                                      rootValue =
                                        if baseChanged
                                          then maybeRootForRefs `mplus` Just resolvedSchema
                                          else case contextRootSchema ctxWithDepth of
                                                 Just existing -> Just existing
                                                 Nothing -> maybeRootForRefs `mplus` Just resolvedSchema
                                      -- For remote refs with relative $id, keep parent base to avoid
                                      -- double-applying the $id during applySchemaContext
                                      baseToSet =
                                        if baseChanged && schemaId resolvedSchema /= Nothing
                                          then contextBaseURI ctxWithDepth  -- Keep parent base
                                          else Just baseDoc
                                  in ctxWithDepth { contextBaseURI = baseToSet
                                                  , contextRootSchema = rootValue
                                                  -- Preserve dynamic scope when following $dynamicRef fallback
                                                  , contextDynamicScope = contextDynamicScope ctxWithDepth
                                                  }
                                Nothing -> ctxWithDepth { contextRootSchema = maybeRootForRefs `mplus` contextRootSchema ctxWithDepth
                                                        -- Preserve dynamic scope when following $dynamicRef fallback
                                                        , contextDynamicScope = contextDynamicScope ctxWithDepth
                                                      }
                              -- Update context if crossing draft boundaries
                              ctxForRefWithVersion = updateContextForCrossDraftRef ctxForRef resolvedSchema
                              -- Push resolved schema onto dynamic scope if it has $dynamicAnchor (2020-12+)
                              version = validationVersion (contextConfig ctxForRefWithVersion)
                              shouldPushResolved = case schemaCore resolvedSchema of
                                ObjectSchema obj -> isJust (schemaDynamicAnchor obj) || isJust (schemaId resolvedSchema)
                                _ -> False
                              ctxWithResolvedScope = if shouldPushResolved
                                then
                                  let effectiveId = case contextBaseURI ctxForRefWithVersion of
                                        Just base -> Just base
                                        Nothing -> schemaId resolvedSchema
                                      resolvedSchemaWithId = resolvedSchema { schemaId = effectiveId }
                                  in ctxForRefWithVersion { contextDynamicScope = resolvedSchemaWithId : contextDynamicScope ctxForRefWithVersion }
                                else ctxForRefWithVersion
                          in validateValueWithContext ctxWithResolvedScope resolvedSchema val'
                        Nothing ->
                          ValidationFailure $ ValidationErrors $ pure $ VR.ValidationError
                            { VR.errorKeyword = "$dynamicRef"
                            , VR.errorSchemaPath = emptyPointer
                            , VR.errorInstancePath = emptyPointer
                            , VR.errorMessage = "Unable to resolve dynamic reference: " <> showReference dynRef
                            }
            Nothing ->
              -- No $dynamicRef, check for $recursiveRef (2019-09)
              case schemaRecursiveRef obj' of
                Just recRef ->
                  -- Handle $recursiveRef similar to $dynamicRef but with 2019-09 semantics
                  if contextRecursionDepth ctx' >= 100
                    then ValidationSuccess mempty  -- Break recursion at depth limit
                    else
                      let ctxWithDepth = ctx' { contextRecursionDepth = contextRecursionDepth ctx' + 1 }
                      in case resolveRecursiveRef recRef ctxWithDepth of
                        Just resolvedSchema ->
                          validateValueWithContext ctxWithDepth resolvedSchema val'
                        Nothing ->
                          -- Fallback: treat as regular $ref using current context (not parent)
                          case resolveReference recRef ctxWithDepth of
                            Just (resolvedSchema, maybeBase, maybeRootForRefs) ->
                              let ctxForRef = case maybeBase of
                                    Just baseDoc ->
                                      let baseChanged = contextBaseURI ctxWithDepth /= Just baseDoc
                                          rootValue =
                                            if baseChanged
                                              then maybeRootForRefs `mplus` Just resolvedSchema
                                              else case contextRootSchema ctxWithDepth of
                                                     Just existing -> Just existing
                                                     Nothing -> maybeRootForRefs `mplus` Just resolvedSchema
                                          baseToSet =
                                            if baseChanged && schemaId resolvedSchema /= Nothing
                                              then contextBaseURI ctxWithDepth
                                              else Just baseDoc
                                      in ctxWithDepth { contextBaseURI = baseToSet
                                                      , contextRootSchema = rootValue
                                                      , contextDynamicScope = contextDynamicScope ctxWithDepth
                                                      }
                                    Nothing -> ctxWithDepth { contextRootSchema = maybeRootForRefs `mplus` contextRootSchema ctxWithDepth
                                                            , contextDynamicScope = contextDynamicScope ctxWithDepth
                                                            }
                                  -- Update context if crossing draft boundaries
                                  ctxForRefWithVersion = updateContextForCrossDraftRef ctxForRef resolvedSchema
                              in validateValueWithContext ctxForRefWithVersion resolvedSchema val'
                            Nothing ->
                              ValidationFailure $ ValidationErrors $ pure $ VR.ValidationError
                                { VR.errorKeyword = "$recursiveRef"
                                , VR.errorSchemaPath = emptyPointer
                                , VR.errorInstancePath = emptyPointer
                                , VR.errorMessage = "Unable to resolve recursive reference: " <> showReference recRef
                                }
                Nothing -> ValidationSuccess mempty  -- No ref at all

    -- Validate basic validation keywords using pluggable keyword system
    -- Handles: type, enum, const, numeric (multipleOf, maximum, exclusiveMaximum, minimum, exclusiveMinimum),
    --          string (maxLength, minLength, pattern)
    -- Note: Array and object keywords are handled separately due to their complexity
    -- 
    -- Falls back to old validation functions for:
    -- - Draft-04 schemas (different exclusiveMinimum/exclusiveMaximum semantics)
    -- - Manually constructed schemas (empty schemaRawKeywords)
    -- Validate object schema content without ref annotations (for pre-2019-09 or when no ref)
    validateObjectSchemaContent ctx' schema' obj' val' =
      validateObjectSchemaContentWithRefAnnotations ctx' schema' obj' val' mempty

    -- Validate object schema content with additional annotations from refs
    -- Note: Dynamic scope has already been updated in validateAgainstObject
    -- Unevaluated keywords are now handled through the normal keyword registry flow
    validateObjectSchemaContentWithRefAnnotations ctx' schema' obj' val' refAnns =
      let keywordResult =
            compileAndValidateKeywords ctx' schema' val'
              (filterKeywordValues ctx' schema' (schemaRawKeywords schema'))
              refAnns  -- Pass ref annotations so post-validation keywords can see them
          metadataAnns = collectMetadataAnnotations obj'
      in case keywordResult of
           ValidationFailure errs ->
             ValidationFailure errs
           ValidationSuccess anns ->
             -- Combine keyword annotations (which already include ref annotations) and metadata annotations
             ValidationSuccess (anns <> metadataAnns)

-- | Attempt to validate a specific keyword via the registry.
-- | Validate conditional keywords (if/then/else, draft-07+)
-- | Compile and validate a subset of keywords via the registry.
-- | Select the appropriate keyword registry for the schema version.
keywordRegistryForSchemaVersion :: Maybe JsonSchemaVersion -> KeywordRegistry
keywordRegistryForSchemaVersion version =
  case version of
    Just Draft04 -> draft04Registry
    Just Draft06 -> draft06Registry
    Just Draft07 -> draft07Registry
    Just Draft201909 -> draft201909Registry
    Just Draft202012 -> draft202012Registry
    Nothing -> standardKeywordRegistry

validationKeywordNames :: Set Text
validationKeywordNames = Set.fromList
  [ "type", "enum", "const"
  , "multipleOf", "maximum", "exclusiveMaximum", "minimum", "exclusiveMinimum"
  , "maxLength", "minLength", "pattern"
  ]

filterKeywordValues :: ValidationContext -> Schema -> Map Text Value -> Map Text Value
filterKeywordValues ctx schema rawValues
  | isValidationVocabularyActive ctx schema = rawValues
  | otherwise = Map.withoutKeys rawValues validationKeywordNames

compileAndValidateKeywords
  :: ValidationContext
  -> Schema
  -> Value
  -> Map Text Value
  -> ValidationAnnotations  -- ^ Additional annotations to include (e.g., from $ref)
  -> ValidationResult
compileAndValidateKeywords ctx schema value keywordValues additionalAnns
  | Map.null keywordValues = ValidationSuccess mempty
  | otherwise =
      let keywordRegistry = keywordRegistryForSchemaVersion (schemaVersion schema)
          schemaRegistry = contextSchemaRegistry ctx
          registryMap = registrySchemas schemaRegistry
          compilationCtx = buildCompilationContext registryMap keywordRegistry schema []
          keywordDefs = keywordMap keywordRegistry
      in case compileKeywords keywordDefs keywordValues schema compilationCtx of
           Left err ->
             validationFailure "compilation" $ "Failed to compile keywords: " <> err
           Right compiledKeywords ->
             let recursiveValidator sch v = validateValueWithContext ctx sch v
                 keywordCtx = Keyword.Types.ValidationContext'
                   { Keyword.Types.kwContextInstancePath = []
                   , Keyword.Types.kwContextSchemaPath = []
                   }
             in KeywordValidate.validateKeywords recursiveValidator compiledKeywords value keywordCtx (contextConfig ctx) additionalAnns

-- See JsonSchema.Keywords.UnevaluatedProperties and UnevaluatedItems
-- See JsonSchema.Keywords.DependentRequired and DependentSchemas
-- See JsonSchema.Keywords.Dependencies

-- | Update validation context when crossing draft boundaries
-- When a schema references another schema with a different $schema version,
-- we need to validate the referenced schema using its own draft rules
updateContextForCrossDraftRef :: ValidationContext -> Schema -> ValidationContext
updateContextForCrossDraftRef ctx resolvedSchema =
  case schemaVersion resolvedSchema of
    Just refVersion ->
      let currentVersion = validationVersion (contextConfig ctx)
      in if refVersion /= currentVersion
         then -- Switch to the referenced schema's draft version
              let newConfig = (contextConfig ctx) { validationVersion = refVersion }
              in ctx { contextConfig = newConfig }
         else ctx
    Nothing -> ctx  -- No $schema specified, use current context's version

-- | Resolve a $ref reference using the schema registry and base URI
-- Supports:
-- - Root reference: #
-- - JSON Pointer references: #/definitions/foo
-- - Anchor references: #anchor-name
-- - Relative URI references: definitions.json#/foo
-- - Absolute URI references: http://example.com/schema#/foo
-- Also handles URL-encoded fragments (e.g., #/definitions/percent%25field)
-- Returns: (resolved schema, base URI, root schema for internal refs)
resolveReference :: Reference -> ValidationContext -> Maybe (Schema, Maybe Text, Maybe Schema)
resolveReference (Reference refText) ctx
  | T.null refText = Nothing
  | refText == "#" =
      -- Root schema reference
      fmap (\schema -> (schema, contextBaseURI ctx, Just schema)) (contextRootSchema ctx)
  | T.isPrefixOf "#/" refText =
      -- JSON Pointer reference within current document
      -- parsePointer handles URL decoding internally
      let pointer = T.drop 1 refText  -- Remove #
          rootSchema =
            case contextRootSchema ctx of
              Just root -> Just root
              Nothing -> lookupBaseSchema ctx
      -- IMPORTANT: Use resolvePointerInSchemaWithBase to track base URI changes
      -- from $id fields encountered while navigating the pointer
      in case rootSchema of
           Nothing -> Nothing
           Just root ->
             case resolvePointerInSchemaWithBase pointer root (contextBaseURI ctx) of
               Nothing -> Nothing
               Just (schema, effectiveBase) ->
                 -- If the resolved schema has $id, it becomes the new root for subsequent references
                 let newRoot = case schemaId schema of
                                 Just _ -> Just schema  -- Schema has $id, it's a new schema resource
                                 Nothing -> rootSchema  -- No $id, keep the old root
                 in Just (schema, effectiveBase, newRoot)
  | T.isPrefixOf "#" refText =
      -- Anchor reference (#foo) or JSON Pointer in compact form
      let anchorName = T.drop 1 refText
          baseURI = contextBaseURI ctx
          registry = contextSchemaRegistry ctx
          rootSchema = contextRootSchema ctx
          -- First try as an anchor (also check dynamic anchors for $ref resolution)
          -- Use Alternative combinators for cleaner fallback chain
          tryAnchor = lookupAnchorWithBase baseURI anchorName registry
            <|> lookupDynamicAnchorWithBase baseURI anchorName registry
            <|> lookupAnchorWithBase Nothing anchorName registry
            <|> lookupDynamicAnchorWithBase Nothing anchorName registry
            <|> lookupAnyAnchor anchorName registry
          -- If not found as anchor, try as JSON Pointer (without leading slash)
          tryPointer = resolvePointerInCurrentSchema ("/" <> anchorName) ctx >>= \schema ->
            Just (schema, contextBaseURI ctx, rootSchema)
      in case tryAnchor of
        Just result -> Just result
        Nothing -> tryPointer
  | T.any (== '#') refText =
      -- URI with fragment
      let (uriPart, fragment) = T.breakOn "#" refText
          fragmentPart = T.drop 1 fragment
          registry = contextSchemaRegistry ctx
          -- Resolve the URI part against the base URI before lookup
          resolvedUriPart = if T.null uriPart
                              then ""
                              else resolveAgainstBaseURI (contextBaseURI ctx) uriPart
          resolvedFullRef = if T.null uriPart
                              then refText
                              else resolvedUriPart <> "#" <> fragmentPart
          baseContext = if T.null uriPart then Nothing else Just resolvedUriPart
      in case Map.lookup resolvedFullRef (registrySchemas registry) of
            Just schema ->
              Just (schema, baseContext, Just schema)
            Nothing ->
              case Map.lookup resolvedUriPart (registrySchemas registry) of
                Just baseSchema
                  | T.null fragmentPart ->
                      Just (baseSchema, baseContext, Just baseSchema)
                  | T.isPrefixOf "/" fragmentPart ->
                      -- When resolving a fragment, return the resolved fragment as the schema
                      -- to validate against, but return the base schema as the root for
                      -- resolving subsequent internal references.
                      -- Track base URI changes from nested $id during pointer resolution
                      resolvePointerInSchemaWithBase fragmentPart baseSchema baseContext >>= \(resolved, effectiveBase) ->
                        Just (resolved, effectiveBase, Just baseSchema)
                  | otherwise ->
                      -- Try regular anchors first, then dynamic anchors
                      let anchors = registryAnchors registry
                          dynamicAnchors = registryDynamicAnchors registry
                          anchorResult = Map.lookup (resolvedUriPart, fragmentPart) anchors >>= \s ->
                            Just (s, baseContext, Just baseSchema)
                          dynamicAnchorResult = Map.lookup (resolvedUriPart, fragmentPart) dynamicAnchors >>= \s ->
                            Just (s, baseContext, Just baseSchema)
                      in anchorResult
                        <|> dynamicAnchorResult
                        <|> lookupAnyAnchor fragmentPart registry
                Nothing ->
                  lookupAnyAnchor fragmentPart registry
  | otherwise =
      -- Plain URI reference
      let registry = contextSchemaRegistry ctx
          resolved = resolveAgainstBaseURI (contextBaseURI ctx) refText
          baseForContext =
            if T.null resolved
              then contextBaseURI ctx
              else Just resolved
      in Map.lookup resolved (registrySchemas registry) >>= \schema ->
           Just (schema, baseForContext, Just schema)
  where
    lookupBaseSchema :: ValidationContext -> Maybe Schema
    lookupBaseSchema ctx' = do
      baseUri <- contextBaseURI ctx'
      Map.lookup baseUri (registrySchemas $ contextSchemaRegistry ctx')

    -- Helper functions for anchor lookup with fallback strategies
    -- These implement the anchor resolution order specified in JSON Schema:
    -- 1. Try with explicit base URI (regular anchors)
    -- 2. Try with explicit base URI (dynamic anchors, draft-2020-12+)
    -- 3. Try with empty base URI (regular anchors)
    -- 4. Try with empty base URI (dynamic anchors)
    -- 5. Try any anchor matching the name (fallback)
    
    -- | Look up a regular anchor with a specific base URI
    --
    -- Returns the schema, base URI context, and root schema if found.
    lookupAnchorWithBase :: Maybe Text -> Text -> SchemaRegistry -> Maybe (Schema, Maybe Text, Maybe Schema)
    lookupAnchorWithBase baseUri anchorName registry =
      let uri = fromMaybe "" baseUri
      in Map.lookup (uri, anchorName) (registryAnchors registry) >>= \s ->
           Just (s, baseUri, Just s)

    -- | Look up a dynamic anchor with a specific base URI
    --
    -- Dynamic anchors are resolved in the same way as regular anchors but
    -- are stored separately for draft-2020-12+ compatibility.
    lookupDynamicAnchorWithBase :: Maybe Text -> Text -> SchemaRegistry -> Maybe (Schema, Maybe Text, Maybe Schema)
    lookupDynamicAnchorWithBase baseUri anchorName registry =
      let uri = fromMaybe "" baseUri
      in Map.lookup (uri, anchorName) (registryDynamicAnchors registry) >>= \s ->
           Just (s, baseUri, Just s)

    -- | Look up an anchor by name across all base URIs
    --
    -- This is the final fallback when anchor lookup with specific base URIs fails.
    -- It searches both regular and dynamic anchors, returning the first match found.
    lookupAnyAnchor :: Text -> SchemaRegistry -> Maybe (Schema, Maybe Text, Maybe Schema)
    lookupAnyAnchor anchorName registry =
      -- Try regular anchors first
      let regularAnchors = do
            ((base, name), schema) <- Map.toList (registryAnchors registry)
            guard (name == anchorName)
            pure (base, schema)
      in case regularAnchors of
        ((baseUri, schema):_) ->
          let baseResult = if T.null baseUri then Nothing else Just baseUri
          in Just (schema, baseResult, Just schema)
        [] ->
          -- Also check dynamic anchors (2020-12+: $ref can resolve $dynamicAnchor)
          let dynamicAnchors = do
                ((base, name), schema) <- Map.toList (registryDynamicAnchors registry)
                guard (name == anchorName)
                pure (base, schema)
          in case dynamicAnchors of
            ((baseUri, schema):_) ->
              let baseResult = if T.null baseUri then Nothing else Just baseUri
              in Just (schema, baseResult, Just schema)
            [] -> Nothing

-- | Resolve a JSON Pointer within the current schema context
resolvePointerInCurrentSchema :: Text -> ValidationContext -> Maybe Schema
resolvePointerInCurrentSchema pointer ctx =
  case contextRootSchema ctx of
    Nothing -> Nothing
    Just rootSchema -> resolvePointerInSchema pointer rootSchema

-- | Resolve a JSON Pointer within a specific schema
resolvePointerInSchema :: Text -> Schema -> Maybe Schema
resolvePointerInSchema pointer schema =
  fmap fst (resolvePointerInSchemaWithBase pointer schema Nothing)

-- | Resolve a JSON Pointer within a specific schema, tracking base URI changes
-- Returns (resolved schema, effective base URI after navigating through schemas with $id)
--
-- This now uses the pluggable keyword navigation system, allowing custom keywords
-- to define their own schema containment and navigation behavior.
resolvePointerInSchemaWithBase :: Text -> Schema -> Maybe Text -> Maybe (Schema, Maybe Text)
resolvePointerInSchemaWithBase pointer schema currentBase =
  case parsePointer pointer of
    Left _ -> Nothing
    Right (JsonPointer segments) -> followPointer segments schema currentBase
  where
    -- Get the appropriate keyword registry based on schema version
    getRegistry :: Schema -> Keyword.KeywordRegistry
    getRegistry s = case schemaVersion s of
      Just Draft04 -> draft04Registry
      _ -> standardKeywordRegistry

    followPointer :: [Text] -> Schema -> Maybe Text -> Maybe (Schema, Maybe Text)
    followPointer [] s base = Just (s, base)
    followPointer (seg:rest) s base = case schemaCore s of
      BooleanSchema _ -> Nothing
      ObjectSchema obj -> navigateKeyword seg rest s obj base

    navigateKeyword :: Text -> [Text] -> Schema -> SchemaObject -> Maybe Text -> Maybe (Schema, Maybe Text)
    navigateKeyword seg rest parentSchema obj currentBase =
      let registry = getRegistry parentSchema
          keywordDef = Keyword.lookupKeyword seg registry
      in case keywordDef of
        -- If keyword has navigation, use it
        Just kwDef -> case Keyword.keywordNavigation kwDef of
          NoNavigation -> Nothing
          
          SingleSchema navFunc ->
            case navFunc parentSchema of
              Just subSchema -> updateBaseAndFollow subSchema
              Nothing -> Nothing
          
          SchemaMap navFunc ->
            case rest of
              (key:remaining) ->
                case navFunc parentSchema of
                  Just schemaMap ->
                    case Map.lookup key schemaMap of
                      Just subSchema -> followPointerFrom remaining subSchema
                      Nothing -> Nothing
                  Nothing -> Nothing
              [] -> Nothing  -- Need a key for SchemaMap navigation
          
          SchemaArray navFunc ->
            case rest of
              (idx:remaining) ->
                case reads (T.unpack idx) :: [(Int, String)] of
                  [(n, "")] | n >= 0 ->
                    case navFunc parentSchema of
                      Just schemas | n < length schemas ->
                        followPointerFrom remaining (schemas !! n)
                      _ -> Nothing
                  _ -> Nothing
              [] -> Nothing  -- Need an index for SchemaArray navigation
          
          CustomNavigation navFunc ->
            -- CustomNavigation gets the current segment and remaining path
            -- and returns (nextSchema, remainingPath)
            case navFunc parentSchema seg rest of
              Just (nextSchema, remaining) -> followPointerFrom remaining nextSchema
              Nothing -> Nothing
        
        -- No registered keyword - check special cases and extensions
        Nothing -> navigateFallback seg rest parentSchema obj currentBase
      where
        -- Update base URI if subschema has $id, then continue following
        updateBaseAndFollow :: Schema -> Maybe (Schema, Maybe Text)
        updateBaseAndFollow subSchema =
          let newBase = case schemaId subSchema of
                Just sid -> Just $ resolveAgainstBaseURI currentBase sid
                Nothing -> currentBase
          in followPointer rest subSchema newBase
        
        followPointerFrom :: [Text] -> Schema -> Maybe (Schema, Maybe Text)
        followPointerFrom remaining subSchema =
          let newBase = case schemaId subSchema of
                Just sid -> Just $ resolveAgainstBaseURI currentBase sid
                Nothing -> currentBase
          in followPointer remaining subSchema newBase
    
    -- Fallback for special cases not handled by registered keywords
    navigateFallback :: Text -> [Text] -> Schema -> SchemaObject -> Maybe Text -> Maybe (Schema, Maybe Text)
    navigateFallback seg rest parentSchema obj currentBase
      -- Special case: $defs and definitions are aliases (handled as SchemaMap)
      | seg == "$defs" || seg == "definitions" =
          case rest of
            (defName:remaining) ->
              case Map.lookup defName (schemaDefs obj) of
                Just subSchema ->
                  let newBase = case schemaId subSchema of
                        Just sid -> Just $ resolveAgainstBaseURI currentBase sid
                        Nothing -> currentBase
                  in followPointer remaining subSchema newBase
                Nothing -> Nothing
            [] -> Nothing
      
      -- Check schemaExtensions and schemaRawKeywords for arbitrary/unknown keywords
      | otherwise =
          -- First check schemaExtensions (for backward compatibility)
          case Map.lookup seg (schemaExtensions parentSchema) of
            Just val ->
              let version = fromMaybe Draft07 (schemaVersion parentSchema)
              in case ParserInternal.parseSchemaValue version val of
                Right schema -> followPointer rest schema currentBase
                Left _ ->
                  -- Not a direct schema - might be an array of schemas
                  case (val, rest) of
                    (Aeson.Array arr, (idx:remaining)) ->
                      case reads (T.unpack idx) :: [(Int, String)] of
                        [(n, "")] | n >= 0 && n < length arr ->
                          case ParserInternal.parseSchemaValue version (arr ! n) of
                            Right schema -> followPointer remaining schema currentBase
                            Left _ -> Nothing
                        _ -> Nothing
                    _ -> Nothing
            Nothing ->
              -- Fall back to schemaRawKeywords (where unknown keywords are stored)
              case Map.lookup seg (schemaRawKeywords parentSchema) of
                Just val ->
                  let version = fromMaybe Draft07 (schemaVersion parentSchema)
                  in case ParserInternal.parseSchemaValue version val of
                    Right schema -> followPointer rest schema currentBase
                    Left _ ->
                      -- Not a direct schema - might be an array of schemas
                      case (val, rest) of
                        (Aeson.Array arr, (idx:remaining)) ->
                          case reads (T.unpack idx) :: [(Int, String)] of
                            [(n, "")] | n >= 0 && n < length arr ->
                              case ParserInternal.parseSchemaValue version (arr ! n) of
                                Right schema -> followPointer remaining schema currentBase
                                Left _ -> Nothing
                            _ -> Nothing
                        _ -> Nothing
                Nothing -> Nothing

-- | Show a reference for error messages
showReference :: Reference -> Text
showReference (Reference t) = t

-- | Resolve a $dynamicRef by searching the dynamic scope stack (2020-12+)
-- $dynamicRef references are resolved by looking for a schema with a matching
-- $dynamicAnchor in the dynamic scope stack (most recent first)
-- Supports both simple anchors (#anchor) and URI references (uri#anchor)
resolveDynamicRef :: Reference -> ValidationContext -> Maybe Schema
resolveDynamicRef (Reference refText) ctx
  | T.any (== '#') refText =
      -- Reference contains a fragment - could be "#anchor" or "uri#anchor"
      let (uriPart, fragment) = T.breakOn "#" refText
          anchorName = T.drop 1 fragment  -- Remove the # prefix
          registry = contextSchemaRegistry ctx
          baseURI = contextBaseURI ctx

          -- BOOKENDING REQUIREMENT: For dynamic resolution to work, the initial
          -- resolution target must have a $dynamicAnchor. If it doesn't, return Nothing
          -- to trigger fallback to static $ref resolution.
          checkBookending schemaToCheck =
            case schemaCore schemaToCheck of
              ObjectSchema obj -> isJust (schemaDynamicAnchor obj)
              _ -> False

      in if T.null uriPart
           then
             -- Simple fragment reference: #anchor
             -- First check if there's a bookending $dynamicAnchor at the target
             let staticTarget = case baseURI of
                   Just uri -> Map.lookup (uri, anchorName) (registryDynamicAnchors registry)
                   Nothing -> Nothing
             in case staticTarget of
               Just targetSchema | checkBookending targetSchema ->
                 -- Target has $dynamicAnchor, search dynamic scope
                 -- Search from outermost (oldest/root) to innermost (most recent) for $dynamicRef
                 -- Reverse the scope list to search outermost first
                 case findSchemaWithDynamicAnchor anchorName (reverse (contextDynamicScope ctx)) of
                   Just schema -> Just schema
                   Nothing -> Just targetSchema  -- Use the target itself
               _ ->
                 -- No bookending $dynamicAnchor at target, return Nothing to trigger fallback
                 Nothing
           else
             -- URI with fragment: uri#anchor
             -- First resolve the URI part statically to find the target
             case resolveReference (Reference refText) ctx of
               Just (resolvedSchema, maybeBase, _) | checkBookending resolvedSchema ->
                 -- Target has $dynamicAnchor, search dynamic scope
                 -- Search from outermost (oldest/root) to innermost (most recent) for $dynamicRef
                 -- Reverse the scope list to search outermost first
                 let scope = reverse (contextDynamicScope ctx)
                 in case findSchemaWithDynamicAnchor anchorName scope of
                   Just schema -> Just schema
                   Nothing ->
                     -- If not in dynamic scope, check the registry
                     case maybeBase of
                       Just uri -> Map.lookup (uri, anchorName) (registryDynamicAnchors registry)
                       Nothing -> Just resolvedSchema
               _ ->
                 -- No bookending $dynamicAnchor, return Nothing to trigger fallback
                 Nothing
  | otherwise = Nothing  -- Not a valid $dynamicRef format
  where
    -- Search for a schema with matching $dynamicAnchor in the dynamic scope
    -- Search from outermost (root/oldest) to innermost (most recent)
    -- The list is reversed before calling this function to achieve outermost-first search
    -- According to the spec, $dynamicRef resolves to the FIRST $dynamicAnchor encountered in scope
    findSchemaWithDynamicAnchor :: Text -> [Schema] -> Maybe Schema
    findSchemaWithDynamicAnchor _anchor [] = Nothing
    findSchemaWithDynamicAnchor anchor (schema:rest) = case schemaCore schema of
      ObjectSchema obj ->
        -- Search the entire schema resource for the matching $dynamicAnchor
        case findDynamicAnchorInSchemaResource anchor schema of
          Just foundSchema -> Just foundSchema
          Nothing -> findSchemaWithDynamicAnchor anchor rest
      _ -> findSchemaWithDynamicAnchor anchor rest

    -- Search for a $dynamicAnchor anywhere within a schema resource
    -- This includes the schema itself, its $defs, and recursively through composition keywords
    -- (allOf, anyOf, oneOf) since they're all part of the same schema resource
    findDynamicAnchorInSchemaResource :: Text -> Schema -> Maybe Schema
    findDynamicAnchorInSchemaResource anchor schema = case schemaCore schema of
      ObjectSchema obj ->
        -- First check if this schema itself has the matching $dynamicAnchor
        case schemaDynamicAnchor obj of
          Just dynAnchor | dynAnchor == anchor -> Just schema
          _ ->
            -- Search in $defs
            case findInDefs anchor (schemaDefs obj) of
              Just foundSchema -> Just foundSchema
              Nothing ->
                -- Search in composition keywords (allOf, anyOf, oneOf)
                -- These are part of the same schema resource if they don't have their own $id
                case findInComposition anchor obj of
                  Just foundSchema -> Just foundSchema
                  Nothing -> Nothing
      _ -> Nothing

    -- Search for a $dynamicAnchor in $defs
    -- Only search schemas that don't have their own $id (same schema resource)
    findInDefs :: Text -> Map Text Schema -> Maybe Schema
    findInDefs anchor defs =
      case [ defSchema
           | defSchema <- Map.elems defs
           , not (isJust (schemaId defSchema))  -- Exclude schemas with their own $id
           , case schemaCore defSchema of
               ObjectSchema defObj ->
                 case schemaDynamicAnchor defObj of
                   Just dynAnchor -> dynAnchor == anchor
                   Nothing -> False
               _ -> False
           ] of
        (found:_) -> Just found
        [] -> Nothing

    -- Search for a $dynamicAnchor in composition keywords (allOf, anyOf, oneOf)
    -- Only search in schemas that don't have their own $id (same schema resource)
    findInComposition :: Text -> SchemaObject -> Maybe Schema
    findInComposition anchor obj =
      let searchSchemas schemas = listToMaybe
            [ result
            | schema <- schemas
            , let schemaHasId = isJust (schemaId schema)
            , not schemaHasId  -- Only search if it's part of the same resource
            , Just result <- [findDynamicAnchorInSchemaResource anchor schema]
            ]
      in case schemaAllOf obj of
           Just schemas -> case searchSchemas (NE.toList schemas) of
             Just found -> Just found
             Nothing -> case schemaAnyOf obj of
               Just schemas' -> case searchSchemas (NE.toList schemas') of
                 Just found' -> Just found'
                 Nothing -> case schemaOneOf obj of
                   Just schemas'' -> searchSchemas (NE.toList schemas'')
                   Nothing -> Nothing
               Nothing -> case schemaOneOf obj of
                 Just schemas'' -> searchSchemas (NE.toList schemas'')
                 Nothing -> Nothing
           Nothing -> case schemaAnyOf obj of
             Just schemas' -> case searchSchemas (NE.toList schemas') of
               Just found' -> Just found'
               Nothing -> case schemaOneOf obj of
                 Just schemas'' -> searchSchemas (NE.toList schemas'')
                 Nothing -> Nothing
             Nothing -> case schemaOneOf obj of
               Just schemas'' -> searchSchemas (NE.toList schemas'')
               Nothing -> Nothing

-- | Resolve $recursiveRef (2019-09)
-- Similar to $dynamicRef but uses $recursiveAnchor (boolean) instead of $dynamicAnchor (named)
-- When $recursiveRef points to "#", it resolves to the outermost schema resource with $recursiveAnchor: true
-- BOOKENDING: The current schema must have $recursiveAnchor: true for dynamic resolution
resolveRecursiveRef :: Reference -> ValidationContext -> Maybe Schema
resolveRecursiveRef (Reference refText) ctx
  | refText == "#" =
      let registry = contextSchemaRegistry ctx
          baseURI = contextBaseURI ctx
          -- Check if the current schema (most recent in dynamic scope) has $recursiveAnchor: true
          currentSchemaHasAnchor = case contextDynamicScope ctx of
            (schema:_) -> case schemaCore schema of
              ObjectSchema obj -> schemaRecursiveAnchor obj == Just True
              _ -> False
            [] -> False
          -- Also check the registry for the base URI
          registryHasAnchor = case baseURI of
            Just uri -> Map.member uri (registryRecursiveAnchors registry)
            Nothing -> False
      in if currentSchemaHasAnchor || registryHasAnchor
         then
           -- Target has $recursiveAnchor: true, search dynamic scope
           -- Search from OUTERMOST to INNERMOST
           -- The dynamic scope list has innermost (most recent) at the head, so reverse to search outermost first
           case findSchemaWithRecursiveAnchor (reverse (contextDynamicScope ctx)) of
             Just schema -> Just schema
             Nothing ->
               -- If not in dynamic scope, check registry
               case baseURI of
                 Just uri -> Map.lookup uri (registryRecursiveAnchors registry)
                 Nothing -> Nothing
         else
           -- No bookending $recursiveAnchor: true, return Nothing to trigger fallback
           Nothing
  | otherwise =
      -- For non-"#" references, treat as regular $ref
      -- This is the fallback behavior in 2019-09
      Nothing
  where
    findSchemaWithRecursiveAnchor :: [Schema] -> Maybe Schema
    findSchemaWithRecursiveAnchor [] = Nothing
    findSchemaWithRecursiveAnchor (schema:rest) = case schemaCore schema of
      ObjectSchema obj ->
        case schemaRecursiveAnchor obj of
          Just True -> Just schema
          _ -> findSchemaWithRecursiveAnchor rest
      _ -> findSchemaWithRecursiveAnchor rest



-- | Validate custom keywords from schema extensions
-- Executes user-provided validators for keywords not in the standard vocabulary
validateCustomKeywords :: ValidationContext -> Schema -> Value -> ValidationResult
validateCustomKeywords ctx schema val =
  let extensions = schemaExtensions schema
      customValidators = validationCustomValidators (contextConfig ctx)
      
      -- Execute validators for extension keywords
      results = 
        [ case Map.lookup keyword customValidators of
            Just validator ->
              -- Execute the custom validator
              case validator val of
                Left err -> ValidationFailure $ ValidationErrors $ pure err
                Right () -> ValidationSuccess mempty
            Nothing ->
              -- No validator registered - treat as annotation only
              ValidationSuccess mempty
        | (keyword, _value) <- Map.toList extensions
        ]
  in combineValidationResults results

