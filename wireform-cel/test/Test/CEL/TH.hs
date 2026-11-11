{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Tests that CEL is compiled at compile time via "CEL.TH" (the spliced
'Expr' is a baked-in constant; no runtime parsing).
-}
module Test.CEL.TH (tests) where

import CEL
import CEL.TH (cel, celFn, compileCel, compileCelFn)
import Data.ByteString qualified as BS
import Data.Map.Lazy qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as V
import Language.Haskell.TH (recover)
import Test.Syd


-- Parsed at compile time into a top-level constant (interpreted at runtime).
arithExpr :: Expr
arithExpr = [cel| 1 + 2 * 3 |]


mapExpr :: Expr
mapExpr = [cel| [1, 2, 3].map(x, x * x) |]


-- Fully compiled to Haskell at compile time (no AST walk at runtime).
arithFn :: Env -> Either CelError Value
arithFn = [celFn| 1 + 2 * 3 |]


varFn :: Env -> Either CelError Value
varFn = [celFn| n * 2 |]


macroFn :: Env -> Either CelError Value
macroFn = [celFn| [1, 2, 3].exists(x, x > 2) && size('abc') == 3 |]


invalidOffsetFn :: Env -> Either CelError Value
invalidOffsetFn = [celFn| timestamp('2026-11-10T00:00:00+00:99') |]


validOffsetFn :: Env -> Either CelError Value
validOffsetFn = [celFn| string(timestamp('1970-01-01t00:59:59.1+01:00')) |]


parseDoubleFn :: Env -> Either CelError Value
parseDoubleFn = [celFn| double(input) |]


listIndexFn :: Env -> Either CelError Value
listIndexFn = [celFn| [42][index] |]


overflowLiteralRejected :: Bool
overflowLiteralRejected =
  $(recover [|True|] (compileCel "1e309" >> [|False|]))
    && $(recover [|True|] (compileCelFn "1e309" >> [|False|]))


duplicateBinderRejected :: Bool
duplicateBinderRejected =
  $(recover [|True|] (compileCel "[4].all(x, x, x == 4)" >> [|False|]))
    && $(recover [|True|] (compileCelFn "[4].transformMap(x, x, true, x)" >> [|False|]))


malformedMacroRejected :: Bool
malformedMacroRejected =
  $(recover [|True|] (compileCel "false && has(1)" >> [|False|]))
    && $(recover [|True|] (compileCelFn "[1].all(42, false)" >> [|False|]))


checkPaths :: Env -> T.Text -> Expr -> (Env -> Either CelError Value) -> Either ErrKind Value -> IO ()
checkPaths env source expr compiled expected =
  map (either (Left . errKind) Right) [run env source, evaluate env expr, compiled env]
    `shouldBe` replicate 3 expected


tests :: Spec
tests =
  describe
    "CEL.TH (compile-time compilation)"
    $ sequence_
      [ it "cel quasiquoter: arithmetic" $
          evaluate emptyEnv arithExpr `shouldBe` Right (VInt 7)
      , it "cel quasiquoter: macro" $
          evaluate emptyEnv mapExpr `shouldBe` Right (VList (V.fromList [VInt 1, VInt 4, VInt 9]))
      , it "compileCel splice" $
          evaluate emptyEnv $(compileCel "'hello'.size()") `shouldBe` Right (VInt 5)
      , it "celFn: compiled arithmetic" $
          arithFn emptyEnv `shouldBe` Right (VInt 7)
      , it "celFn: compiled program reads the environment" $
          varFn (bind "n" (VInt 5) emptyEnv) `shouldBe` Right (VInt 10)
      , it "celFn: compiled macros + functions" $
          macroFn emptyEnv `shouldBe` Right (VBool True)
      , it "separates protobuf field presence from selection and ordinary map keys" $ do
          let fields = Map.fromList [("implicit", CelField False (VInt 0)), ("explicit", CelField True (VInt 0))]
              message = VMessage (CelMessage "example.Presence" fields)
              values = VMap (celMapFromList [(VString "implicit", VInt 0)])
              env = bindAll [("m", message), ("values", values)] emptyEnv
          checkPaths env
            "m.implicit == 0 && !has(m.implicit) && has(m.explicit) && has(values.implicit)"
            [cel| m.implicit == 0 && !has(m.implicit) && has(m.explicit) && has(values.implicit) |]
            [celFn| m.implicit == 0 && !has(m.implicit) && has(m.explicit) && has(values.implicit) |]
            (Right (VBool True))
          checkPaths env "has(m.unknown)" [cel| has(m.unknown) |] [celFn| has(m.unknown) |] (Left ErrNoSuchField)
          checkPaths env "type(m)" [cel| type(m) |] [celFn| type(m) |] (Right (VType (TyMessage "example.Presence")))
      , it "constructs registered messages and resolves their type names through the container" $ do
          let person fields = case Map.lookupMin (Map.delete "id" fields) of
                Just (name, _) -> Left (CelError ErrNoSuchField name)
                Nothing -> case Map.lookup "id" fields of
                  Just value@(VInt _) -> Right (Map.singleton "id" (CelField True value))
                  Nothing -> Right (Map.singleton "id" (CelField False (VInt 0)))
                  Just _ -> Left (CelError ErrNoOverload "id must be an int")
              env = withContainer "example.api" $
                addMessageType "example.Person" person $
                  addMessageType "example.api.Person" person emptyEnv
          checkPaths env
            "has(Person{id: 0}.id) && !has(Person{}.id) && Person{}.id == 0 && type(Person{}) == Person"
            [cel| has(Person{id: 0}.id) && !has(Person{}.id) && Person{}.id == 0 && type(Person{}) == Person |]
            [celFn| has(Person{id: 0}.id) && !has(Person{}.id) && Person{}.id == 0 && type(Person{}) == Person |]
            (Right (VBool True))
          checkPaths env "type(.example.Person{})" [cel| type(.example.Person{}) |] [celFn| type(.example.Person{}) |]
            (Right (VType (TyMessage "example.Person")))
          checkPaths env "Person{unknown: 1}" [cel| Person{unknown: 1} |] [celFn| Person{unknown: 1} |] (Left ErrNoSuchField)
          checkPaths env "Person{id: 'bad'}" [cel| Person{id: 'bad'} |] [celFn| Person{id: 'bad'} |] (Left ErrNoOverload)
          checkPaths env "Person{id: 1, id: 2}" [cel| Person{id: 1, id: 2} |] [celFn| Person{id: 1, id: 2} |]
            (Left ErrInvalid)
      , it "constructs Empty and keeps absolute well-known names independent of the container" $ do
          let env = withContainer "example" $
                addMessageType "example.google.protobuf.Int32Value" (const (Right Map.empty)) emptyEnv
          checkPaths env
            "type(google.protobuf.Empty{}) == google.protobuf.Empty && google.protobuf.Empty{} == .google.protobuf.Empty{}"
            [cel| type(google.protobuf.Empty{}) == google.protobuf.Empty && google.protobuf.Empty{} == .google.protobuf.Empty{} |]
            [celFn| type(google.protobuf.Empty{}) == google.protobuf.Empty && google.protobuf.Empty{} == .google.protobuf.Empty{} |]
            (Right (VBool True))
          checkPaths env "google.protobuf.Empty{unknown: 1}" [cel| google.protobuf.Empty{unknown: 1} |]
            [celFn| google.protobuf.Empty{unknown: 1} |] (Left ErrNoSuchField)
          checkPaths env
            "google.protobuf.ListValue{values: [google.protobuf.Empty{}]} == [{}] && google.protobuf.Struct{fields: {'x': google.protobuf.Empty{}}} == {'x': {}}"
            [cel| google.protobuf.ListValue{values: [google.protobuf.Empty{}]} == [{}] && google.protobuf.Struct{fields: {'x': google.protobuf.Empty{}}} == {'x': {}} |]
            [celFn| google.protobuf.ListValue{values: [google.protobuf.Empty{}]} == [{}] && google.protobuf.Struct{fields: {'x': google.protobuf.Empty{}}} == {'x': {}} |]
            (Right (VBool True))
          checkPaths env "type(google.protobuf.Int32Value{})" [cel| type(google.protobuf.Int32Value{}) |]
            [celFn| type(google.protobuf.Int32Value{}) |] (Right (VType (TyMessage "example.google.protobuf.Int32Value")))
          checkPaths env ".google.protobuf.Int32Value{}" [cel| .google.protobuf.Int32Value{} |]
            [celFn| .google.protobuf.Int32Value{} |] (Right (VInt 0))
      , it "unpacks Any literals through the registered resolver with runtime and TH parity" $ do
          let resolve "custom.invalid/acme.Example" payload
                | payload == "payload" = Right (VMessage (CelMessage "acme.Example" (Map.singleton "id" (CelField True (VInt 17)))))
                | otherwise = Left (CelError ErrConversion "invalid payload")
              resolve _ _ = Left (CelError ErrConversion "unknown type")
              env = addAnyResolver resolve emptyEnv
          checkPaths env
            "google.protobuf.Any{type_url: 'custom.invalid/acme.Example', value: b'payload'}.id"
            [cel| google.protobuf.Any{type_url: 'custom.invalid/acme.Example', value: b'payload'}.id |]
            [celFn| google.protobuf.Any{type_url: 'custom.invalid/acme.Example', value: b'payload'}.id |]
            (Right (VInt 17))
          checkPaths env "google.protobuf.Any{}" [cel| google.protobuf.Any{} |]
            [celFn| google.protobuf.Any{} |] (Left ErrConversion)
          checkPaths env "google.protobuf.Any{type_url: 'unknown.invalid/example.Missing'}"
            [cel| google.protobuf.Any{type_url: 'unknown.invalid/example.Missing'} |]
            [celFn| google.protobuf.Any{type_url: 'unknown.invalid/example.Missing'} |] (Left ErrConversion)
          checkPaths env "google.protobuf.Any{type_url: 'custom.invalid/acme.Example'}"
            [cel| google.protobuf.Any{type_url: 'custom.invalid/acme.Example'} |]
            [celFn| google.protobuf.Any{type_url: 'custom.invalid/acme.Example'} |] (Left ErrConversion)
          checkPaths env "google.protobuf.Any{type_url: 1}" [cel| google.protobuf.Any{type_url: 1} |]
            [celFn| google.protobuf.Any{type_url: 1} |] (Left ErrNoOverload)
          checkPaths env "google.protobuf.Any{unknown: b'payload'}" [cel| google.protobuf.Any{unknown: b'payload'} |]
            [celFn| google.protobuf.Any{unknown: b'payload'} |] (Left ErrNoSuchField)
          checkPaths emptyEnv "google.protobuf.Any{type_url: 'custom.invalid/acme.Example'}"
            [cel| google.protobuf.Any{type_url: 'custom.invalid/acme.Example'} |]
            [celFn| google.protobuf.Any{type_url: 'custom.invalid/acme.Example'} |] (Left ErrUnsupported)
      , it "reads Any collection members independently with runtime and TH parity" $ do
          let known = VAny "type.googleapis.com/google.protobuf.Int32Value" "\x08\x01" (Right (VInt 1))
              missing = VAny "example.invalid/acme.Missing" "\x08\x01" (Left (CelError ErrConversion "unknown type"))
              fields = Map.fromList
                [ ("items", CelField True (VList (V.fromList [known, missing])))
                , ("entries", CelField True (VMap (celMapFromList [(VString "ok", known), (VString "bad", missing)])))
                ]
              env = bind "m" (VMessage (CelMessage "acme.Envelope" fields)) emptyEnv
          checkPaths env
            "size(m.items) == 2 && size(m.entries) == 2 && 'bad' in m.entries && has(m.entries.ok) && m.items[0] + m.entries.ok == 2 && m.items.exists(v, v == 1)"
            [cel| size(m.items) == 2 && size(m.entries) == 2 && 'bad' in m.entries && has(m.entries.ok) && m.items[0] + m.entries.ok == 2 && m.items.exists(v, v == 1) |]
            [celFn| size(m.items) == 2 && size(m.entries) == 2 && 'bad' in m.entries && has(m.entries.ok) && m.items[0] + m.entries.ok == 2 && m.items.exists(v, v == 1) |]
            (Right (VBool True))
          checkPaths env "m.items[1]" [cel| m.items[1] |] [celFn| m.items[1] |] (Left ErrConversion)
          checkPaths env "m.entries['bad']" [cel| m.entries['bad'] |] [celFn| m.entries['bad'] |] (Left ErrConversion)
          checkPaths env "has(m.entries.bad)" [cel| has(m.entries.bad) |] [celFn| has(m.entries.bad) |] (Left ErrConversion)
          checkPaths env "m.items == [1, 1]" [cel| m.items == [1, 1] |] [celFn| m.items == [1, 1] |] (Left ErrConversion)
          checkPaths env "m.entries == {'ok': 1, 'bad': 1}"
            [cel| m.entries == {'ok': 1, 'bad': 1} |] [celFn| m.entries == {'ok': 1, 'bad': 1} |] (Left ErrConversion)
          checkPaths env "m.items != [2, 1] && m.entries != {'bad': 1, 'ok': 2}"
            [cel| m.items != [2, 1] && m.entries != {'bad': 1, 'ok': 2} |]
            [celFn| m.items != [2, 1] && m.entries != {'bad': 1, 'ok': 2} |] (Right (VBool True))
      , it "retains Any payload types for message equality with runtime and TH parity" $ do
          let resolve "x/google.protobuf.FloatValue" _ = Right (VDouble 1)
              resolve _ _ = Right (VInt 1)
              constructor = Right . Map.map (CelField True)
              packed name = VAny name "\x08\x01" (Right (VInt 1))
              envelope value = VMessage (CelMessage "Envelope" (Map.singleton "items" (CelField True (VList (V.singleton value)))))
              nested value = VAny "type.googleapis.com/google.protobuf.Any" "" (Right value)
              int32 = packed "type.googleapis.com/google.protobuf.Int32Value"
              int64 = packed "type.googleapis.com/google.protobuf.Int64Value"
              env = addAnyResolver resolve $ addMessageType "Envelope" constructor $
                bindAll
                  [ ("int32", envelope int32)
                  , ("int64", envelope int64)
                  , ("nested32", envelope (nested int32))
                  , ("nested64", envelope (nested int64))
                  ] emptyEnv
          checkPaths env "int32 != int64 && int32.items == int64.items"
            [cel| int32 != int64 && int32.items == int64.items |]
            [celFn| int32 != int64 && int32.items == int64.items |] (Right (VBool True))
          checkPaths env "nested32 != nested64 && nested32.items == nested64.items"
            [cel| nested32 != nested64 && nested32.items == nested64.items |]
            [celFn| nested32 != nested64 && nested32.items == nested64.items |] (Right (VBool True))
          checkPaths env
            "Envelope{items: [google.protobuf.Any{type_url: 'x/google.protobuf.Int32Value'}]} == Envelope{items: [google.protobuf.Any{type_url: 'x/google.protobuf.Int64Value'}]}"
            [cel| Envelope{items: [google.protobuf.Any{type_url: 'x/google.protobuf.Int32Value'}]} == Envelope{items: [google.protobuf.Any{type_url: 'x/google.protobuf.Int64Value'}]} |]
            [celFn| Envelope{items: [google.protobuf.Any{type_url: 'x/google.protobuf.Int32Value'}]} == Envelope{items: [google.protobuf.Any{type_url: 'x/google.protobuf.Int64Value'}]} |]
            (Right (VBool True))
          checkPaths env
            "Envelope{items: [google.protobuf.Any{type_url: 'x/google.protobuf.Int32Value'}]} != Envelope{items: [google.protobuf.Any{type_url: 'x/google.protobuf.FloatValue'}]}"
            [cel| Envelope{items: [google.protobuf.Any{type_url: 'x/google.protobuf.Int32Value'}]} != Envelope{items: [google.protobuf.Any{type_url: 'x/google.protobuf.FloatValue'}]} |]
            [celFn| Envelope{items: [google.protobuf.Any{type_url: 'x/google.protobuf.Int32Value'}]} != Envelope{items: [google.protobuf.Any{type_url: 'x/google.protobuf.FloatValue'}]} |]
            (Right (VBool True))
      , it "compares stored protobuf fields when CEL selection fails" $ do
          let message bytes =
                let payload = CelFieldWithSelection True (VBytes bytes) (Left (CelError ErrConversion "unresolved Any"))
                in VMessage (CelMessage "acme.Envelope" (Map.singleton "payload" payload))
              env = bindAll [("left", message "one"), ("same", message "one"), ("different", message "two")] emptyEnv
          checkPaths env "has(left.payload) && left == same && left != different"
            [cel| has(left.payload) && left == same && left != different |]
            [celFn| has(left.payload) && left == same && left != different |] (Right (VBool True))
          checkPaths env "left.payload" [cel| left.payload |] [celFn| left.payload |] (Left ErrConversion)
      , it "compares the full Any payload and selects its CEL projection" $ do
          let name = "google.protobuf.Int64Value"
              wrapped unknown =
                let fields = Map.singleton "value" (CelField True (VInt 1))
                    stored = VMessage (celMessageWithUnknownFields name fields unknown)
                in VAnyPayload ("type.googleapis.com/" <> name) "" (Right (AnyPayload stored (Right (VInt 1))))
              envelope value = VMessage (CelMessage "Envelope" (Map.singleton "payload" (CelField True value)))
              env = addMessageType "Envelope" (Right . Map.map (CelField True)) $
                bindAll [("plain", envelope (wrapped "")), ("extra", envelope (wrapped "\x10\x01"))] emptyEnv
          checkPaths env "plain != extra && plain.payload == extra.payload"
            [cel| plain != extra && plain.payload == extra.payload |]
            [celFn| plain != extra && plain.payload == extra.payload |] (Right (VBool True))
          checkPaths env "plain == Envelope{payload: 1} && extra != Envelope{payload: 1}"
            [cel| plain == Envelope{payload: 1} && extra != Envelope{payload: 1} |]
            [celFn| plain == Envelope{payload: 1} && extra != Envelope{payload: 1} |] (Right (VBool True))
      , it "compares the original unknown protobuf field bytes with runtime and TH parity" $ do
          let message = VMessage . celMessageWithUnknownFields "acme.Envelope" Map.empty
              env = bindAll
                [ ("left", message "\x08\x01")
                , ("same", message "\x08\x01")
                , ("overlong", message "\x08\x81\x00")
                , ("absent", VMessage (CelMessage "acme.Envelope" Map.empty))
                ] emptyEnv
          checkPaths env "left == same && left != overlong && left != absent"
            [cel| left == same && left != overlong && left != absent |]
            [celFn| left == same && left != overlong && left != absent |] (Right (VBool True))
      , it "preserves unknown-field order within each protobuf field number" $ do
          let message = VMessage . celMessageWithUnknownFields "acme.Envelope" Map.empty
              env = bindAll
                [ ("left", message "\x08\x01\x13\x18\x07\x14\x08\x02")
                , ("reordered", message "\x08\x01\x08\x02\x13\x18\x07\x14")
                , ("reversed", message "\x08\x02\x13\x18\x07\x14\x08\x01")
                , ("differentGroup", message "\x08\x01\x13\x18\x08\x14\x08\x02")
                , ("invalid", message "\x12\xff\xff\xff\xff\xff\xff\xff\xff\xff\x01")
                , ("otherInvalid", message "\x12\xfe\xff\xff\xff\xff\xff\xff\xff\xff\x01")
                ] emptyEnv
          checkPaths env "left == reordered && left != reversed && left != differentGroup"
            [cel| left == reordered && left != reversed && left != differentGroup |]
            [celFn| left == reordered && left != reversed && left != differentGroup |] (Right (VBool True))
          checkPaths env "invalid != otherInvalid" [cel| invalid != otherInvalid |]
            [celFn| invalid != otherInvalid |] (Right (VBool True))
      , it "resolves built-in protobuf declarations through the CEL container" $ do
          let env = withContainer "google.protobuf.example" (bind "Duration" (VInt 7) emptyEnv)
          checkPaths env
            "type(duration('1s')) == Duration && .Duration == 7"
            [cel| type(duration('1s')) == Duration && .Duration == 7 |]
            [celFn| type(duration('1s')) == Duration && .Duration == 7 |]
            (Right (VBool True))
          checkPaths env
            "Timestamp == .google.protobuf.Timestamp"
            [cel| Timestamp == .google.protobuf.Timestamp |]
            [celFn| Timestamp == .google.protobuf.Timestamp |]
            (Right (VBool True))
      , it "prefers a complete built-in enum name to field selection on a shorter binding" $ do
          let env = withContainer "google.protobuf" (bind "NullValue" (VInt 12) emptyEnv)
          checkPaths env
            "NullValue.NULL_VALUE"
            [cel| NullValue.NULL_VALUE |]
            [celFn| NullValue.NULL_VALUE |]
            (Right (VInt 0))
      , it "resolves global and namespaced calls against the container and the root" $ do
          let constant n = const (Just (Right (VInt n)))
              env = withContainer "example.api" $
                addFunction "example.answer" (constant 42) $
                  addFunction "example.api.lib.answer" (constant 100) $
                    addFunction "lib.answer" (constant 9) $
                      addFunction "answer" (constant 7) emptyEnv
          checkPaths env
            "[answer(), .answer(), example.answer(), lib.answer(), .lib.answer()]"
            [cel| [answer(), .answer(), example.answer(), lib.answer(), .lib.answer()] |]
            [celFn| [answer(), .answer(), example.answer(), lib.answer(), .lib.answer()] |]
            (Right (VList (V.fromList (map VInt [42, 7, 42, 100, 9]))))
      , it "uses receiver dispatch only when no qualified function is declared" $ do
          let env = withContainer "example" (bind "value" (VString "abc") emptyEnv)
              declared = addFunction "example.value.size" (const Nothing) env
          checkPaths env "value.size()" [cel| value.size() |] [celFn| value.size() |] (Right (VInt 3))
          checkPaths declared "value.size()" [cel| value.size() |] [celFn| value.size() |] (Left ErrNoOverload)
      , it "constructs scalar protobuf wrappers with runtime and TH parity" $ do
          let env n = bind "n" n (withContainer "google.protobuf" emptyEnv)
              signed n =
                checkPaths (env n) "Int32Value{value: n}" [cel| Int32Value{value: n} |] [celFn| Int32Value{value: n} |]
              unsigned n =
                checkPaths
                  (env n)
                  ".google.protobuf.UInt32Value{value: n}"
                  [cel| .google.protobuf.UInt32Value{value: n} |]
                  [celFn| .google.protobuf.UInt32Value{value: n} |]
          signed (VInt (-2147483648)) (Right (VInt (-2147483648)))
          signed (VInt 2147483648) (Left ErrOverflow)
          signed (VUInt 1) (Left ErrNoOverload)
          unsigned (VUInt 4294967295) (Right (VUInt 4294967295))
          unsigned (VUInt 4294967296) (Left ErrOverflow)
          checkPaths (env (VInt 0)) ".Int32Value{}" [cel| .Int32Value{} |] [celFn| .Int32Value{} |] (Left ErrUnsupported)
          checkPaths emptyEnv "google.protobuf.BoolValue{}" [cel| google.protobuf.BoolValue{} |] [celFn| google.protobuf.BoolValue{} |] (Right (VBool False))
          checkPaths
            emptyEnv
            "google.protobuf.BoolValue{value: true, value: false}"
            [cel| google.protobuf.BoolValue{value: true, value: false} |]
            [celFn| google.protobuf.BoolValue{value: true, value: false} |]
            (Left ErrInvalid)
      , it "rounds protobuf float wrappers to binary32 with runtime and TH parity" $ do
          let check input =
                checkPaths
                  (bind "input" (VDouble input) emptyEnv)
                  "google.protobuf.FloatValue{value: input}"
                  [cel| google.protobuf.FloatValue{value: input} |]
                  [celFn| google.protobuf.FloatValue{value: input} |]
          check 16777217 (Right (VDouble 16777216))
          check 1.4e55 (Right (VDouble (1 / 0)))
          check (-9.9e100) (Right (VDouble (-1 / 0)))
          check (1 / 0) (Right (VDouble (1 / 0)))
      , it "constructs FieldMask and applies its ProtoJSON form with runtime and TH parity" $ do
          checkPaths emptyEnv
            "type(google.protobuf.FieldMask{}) == google.protobuf.FieldMask && !has(google.protobuf.FieldMask{}.paths)"
            [cel| type(google.protobuf.FieldMask{}) == google.protobuf.FieldMask && !has(google.protobuf.FieldMask{}.paths) |]
            [celFn| type(google.protobuf.FieldMask{}) == google.protobuf.FieldMask && !has(google.protobuf.FieldMask{}.paths) |]
            (Right (VBool True))
          checkPaths emptyEnv
            "google.protobuf.ListValue{values: [google.protobuf.FieldMask{paths: ['foo_bar.baz_quux', 'many_parts']}, google.protobuf.FieldMask{}]} == ['fooBar.bazQuux,manyParts', '']"
            [cel| google.protobuf.ListValue{values: [google.protobuf.FieldMask{paths: ['foo_bar.baz_quux', 'many_parts']}, google.protobuf.FieldMask{}]} == ['fooBar.bazQuux,manyParts', ''] |]
            [celFn| google.protobuf.ListValue{values: [google.protobuf.FieldMask{paths: ['foo_bar.baz_quux', 'many_parts']}, google.protobuf.FieldMask{}]} == ['fooBar.bazQuux,manyParts', ''] |]
            (Right (VBool True))
          checkPaths emptyEnv
            "google.protobuf.ListValue{values: [google.protobuf.FieldMask{paths: ['foo__bar']}]}"
            [cel| google.protobuf.ListValue{values: [google.protobuf.FieldMask{paths: ['foo__bar']}]} |]
            [celFn| google.protobuf.ListValue{values: [google.protobuf.FieldMask{paths: ['foo__bar']}]} |]
            (Left ErrConversion)
          checkPaths emptyEnv "google.protobuf.FieldMask{paths: [1]}"
            [cel| google.protobuf.FieldMask{paths: [1]} |]
            [celFn| google.protobuf.FieldMask{paths: [1]} |]
            (Left ErrNoOverload)
      , it "constructs protobuf timestamps and durations with runtime and TH parity" $ do
          let checkTime input =
                checkPaths
                  (bind "input" (VInt input) emptyEnv)
                  "google.protobuf.Timestamp{seconds: 253402300799, nanos: input}"
                  [cel| google.protobuf.Timestamp{seconds: 253402300799, nanos: input} |]
                  [celFn| google.protobuf.Timestamp{seconds: 253402300799, nanos: input} |]
              checkDuration input =
                checkPaths
                  (bind "input" (VInt input) emptyEnv)
                  "google.protobuf.Duration{seconds: -1, nanos: input}"
                  [cel| google.protobuf.Duration{seconds: -1, nanos: input} |]
                  [celFn| google.protobuf.Duration{seconds: -1, nanos: input} |]
          checkTime 999999999 (Right (VTimestamp (Timestamp 253402300799 999999999)))
          checkTime (-1) (Left ErrOverflow)
          checkDuration (-1) (Right (VDuration (Duration (-1) (-1))))
          checkDuration 1 (Left ErrInvalid)
      , it "unpacks protobuf JSON literals into CEL collections on all paths" $ do
          let env = withContainer "google.protobuf" emptyEnv
              expected =
                VList $
                  V.fromList
                    [ VDouble 7
                    , VMap (celMapFromList [(VString "items", VList (V.fromList [VBool False, VNull]))])
                    , VString "9007199254740992"
                    , VString "aGk="
                    , VString "0.001s"
                    , VString "2024-01-01T00:00:00.001Z"
                    , VDouble (1 / 0)
                    ]
          checkPaths
            env
            "Value{list_value: [7u, Struct{fields: {'items': [false, null]}}, 9007199254740992, b'hi', duration('1ms'), timestamp('2024-01-01T00:00:00.001Z'), double('Infinity')]}"
            [cel| Value{list_value: [7u, Struct{fields: {'items': [false, null]}}, 9007199254740992, b'hi', duration('1ms'), timestamp('2024-01-01T00:00:00.001Z'), double('Infinity')]} |]
            [celFn| Value{list_value: [7u, Struct{fields: {'items': [false, null]}}, 9007199254740992, b'hi', duration('1ms'), timestamp('2024-01-01T00:00:00.001Z'), double('Infinity')]} |]
            (Right expected)
          checkPaths
            env
            "[Value{}, ListValue{}, Struct{}, Value{null_value: NullValue.NULL_VALUE}]"
            [cel| [Value{}, ListValue{}, Struct{}, Value{null_value: NullValue.NULL_VALUE}] |]
            [celFn| [Value{}, ListValue{}, Struct{}, Value{null_value: NullValue.NULL_VALUE}] |]
            (Right (VList (V.fromList [VNull, VList V.empty, VMap (celMapFromList []), VNull])))
          checkPaths
            env
            "[type(ListValue{values: [9007199254740991u]}[0]), type(Struct{fields: {'x': -9007199254740991}}.x), ListValue{values: [-9007199254740992]}[0]]"
            [cel| [type(ListValue{values: [9007199254740991u]}[0]), type(Struct{fields: {'x': -9007199254740991}}.x), ListValue{values: [-9007199254740992]}[0]] |]
            [celFn| [type(ListValue{values: [9007199254740991u]}[0]), type(Struct{fields: {'x': -9007199254740991}}.x), ListValue{values: [-9007199254740992]}[0]] |]
            (Right (VList (V.fromList [VType TyDouble, VType TyDouble, VString "-9007199254740992"])))
      , it "pads large protobuf JSON byte strings only at the end" $ do
          let input = VBytes (BS.replicate (3 * 1024 * 1024 + 1) 97)
              env = bind "input" input (withContainer "google.protobuf" emptyEnv)
          checkPaths
            env
            "[ListValue{values: [input]}[0]].all(encoded, size(encoded) == 4194308 && encoded.endsWith('YWFhYQ=='))"
            [cel| [ListValue{values: [input]}[0]].all(encoded, size(encoded) == 4194308 && encoded.endsWith('YWFhYQ==')) |]
            [celFn| [ListValue{values: [input]}[0]].all(encoded, size(encoded) == 4194308 && encoded.endsWith('YWFhYQ==')) |]
            (Right (VBool True))
      , it "enforces protobuf JSON field types and oneof cardinality on all paths" $ do
          let env = withContainer "google.protobuf" emptyEnv
              list input =
                checkPaths
                  (bind "input" input env)
                  "ListValue{values: input}"
                  [cel| ListValue{values: input} |]
                  [celFn| ListValue{values: input} |]
              number input =
                checkPaths
                  (bind "input" input env)
                  "Value{number_value: input}"
                  [cel| Value{number_value: input} |]
                  [celFn| Value{number_value: input} |]
              nullValue input =
                checkPaths
                  (bind "input" input env)
                  "Value{null_value: input}"
                  [cel| Value{null_value: input} |]
                  [celFn| Value{null_value: input} |]
          list VNull (Left ErrNoOverload)
          list (VList (V.singleton (VMap (celMapFromList [(VInt 1, VBool True)])))) (Left ErrNoOverload)
          list (VList (V.singleton (VType TyInt))) (Left ErrNoOverload)
          number (VDouble 0) (Right (VDouble 0))
          number (VInt 0) (Left ErrNoOverload)
          nullValue (VInt 42) (Right VNull)
          nullValue (VInt 4294967296) (Left ErrOverflow)
          nullValue VNull (Left ErrNoOverload)
          checkPaths env "Value{missing: 0}" [cel| Value{missing: 0} |] [celFn| Value{missing: 0} |] (Left ErrNoSuchField)
          checkPaths
            env
            "Value{bool_value: false, string_value: 'x'}"
            [cel| Value{bool_value: false, string_value: 'x'} |]
            [celFn| Value{bool_value: false, string_value: 'x'} |]
            (Left ErrInvalid)
      , it "runtime and TH keep member and global overloads distinct" $ do
          let invalid = run emptyEnv "('1').int()"
              valid = "size('banana') == 'banana'.size() && 'banana'.contains('ana')"
              host = addFunction "int" (const (Just (Right (VInt 42)))) emptyEnv
          case invalid of
            Left e -> errKind e `shouldBe` ErrNoOverload
            Right value -> expectationFailure ("Accepted a global-only conversion as a member call: " <> show value)
          evaluate emptyEnv [cel| ('1').int() |] `shouldBe` invalid
          [celFn| ('1').int() |] emptyEnv `shouldBe` invalid
          run emptyEnv valid `shouldBe` Right (VBool True)
          [celFn| size('banana') == 'banana'.size() && 'banana'.contains('ana') |] emptyEnv `shouldBe` Right (VBool True)
          run host "('1').int()" `shouldBe` Right (VInt 42)
          [celFn| ('1').int() |] host `shouldBe` Right (VInt 42)
      , it "bounds decimal integer conversion for long strings in runtime and TH" $ do
          let check source expr compiled input expected = do
                let env = bind "input" (VString input) emptyEnv
                    results = [run env source, evaluate env expr, compiled env]
                map (either (Left . errKind) Right) results `shouldBe` replicate 3 expected
              signed = check "int(input)" [cel| int(input) |] [celFn| int(input) |]
              unsigned = check "uint(input)" [cel| uint(input) |] [celFn| uint(input) |]
              huge = T.replicate 100000 "9"
              padding = T.replicate 100000 "0"
          signed ("-" <> padding <> "9223372036854775808") (Right (VInt minBound))
          unsigned (padding <> "18446744073709551615") (Right (VUInt maxBound))
          signed "9223372036854775808" (Left ErrOverflow)
          unsigned "18446744073709551616" (Left ErrOverflow)
          signed ("-" <> huge) (Left ErrOverflow)
          unsigned huge (Left ErrOverflow)
          signed (huge <> "x") (Left ErrConversion)
          unsigned "+1" (Left ErrConversion)
      , it "bounds integer literal compilation and preserves leading zeros" $ do
          let hugeDecimal = T.replicate 100000 "9"
              hugeHex = "0x" <> T.replicate 100000 "f" <> "u"
              padded = "0x" <> T.replicate 100000 "0" <> "ffffffffffffffffu"
              checkOverflow input = case compile input of
                Left e -> errKind e `shouldBe` ErrParse
                Right expr -> expectationFailure ("Accepted oversized literal " <> show expr)
          checkOverflow hugeDecimal
          checkOverflow hugeHex
          checkOverflow "18446744073709551616u"
          $(recover [|True|] (compileCel (replicate 100000 '9') >> [|False|])) `shouldBe` True
          $(recover [|True|] (compileCelFn ("0x" <> replicate 100000 'f' <> "u") >> [|False|])) `shouldBe` True
          $(recover [|True|] (compileCelFn "18446744073709551616u" >> [|False|])) `shouldBe` True
          run emptyEnv "18446744073709551615u" `shouldBe` Right (VUInt maxBound)
          [celFn| 18446744073709551615u |] emptyEnv `shouldBe` Right (VUInt maxBound)
          run emptyEnv padded `shouldBe` Right (VUInt maxBound)
          $(compileCelFn ("0x" <> replicate 100000 '0' <> "ffffffffffffffffu")) emptyEnv
            `shouldBe` Right (VUInt maxBound)
      , it "runtime and TH match CEL-Go at both double formatting cutoffs" $ do
          let source = "[string(999999.0), string(1000000.0), string(0.0001), string(0.00001)]"
              expected = Right (VList (V.fromList (map VString ["999999", "1e+06", "0.0001", "1e-05"])))
          run emptyEnv source `shouldBe` expected
          evaluate emptyEnv [cel| [string(999999.0), string(1000000.0), string(0.0001), string(0.00001)] |]
            `shouldBe` expected
          [celFn| [string(999999.0), string(1000000.0), string(0.0001), string(0.00001)] |] emptyEnv
            `shouldBe` expected
      , it "runtime and TH use the shortest decimal with ties to even" $ do
          let source = "[string(1e23), string(-1e23), string(1661681331200894.25), string(2.98023223876953125e-8), string(5.9604644775390625e-8), string(5e-324)]"
              expected = Right (VList (V.fromList (map VString
                ["1e+23", "-1e+23", "1.6616813312008942e+15", "2.9802322387695312e-08", "5.960464477539063e-08", "5e-324"])))
          run emptyEnv source `shouldBe` expected
          evaluate emptyEnv
            [cel| [string(1e23), string(-1e23), string(1661681331200894.25), string(2.98023223876953125e-8), string(5.9604644775390625e-8), string(5e-324)] |]
            `shouldBe` expected
          [celFn| [string(1e23), string(-1e23), string(1661681331200894.25), string(2.98023223876953125e-8), string(5.9604644775390625e-8), string(5e-324)] |] emptyEnv
            `shouldBe` expected
      , it "runtime, AST splices and compiled functions agree on fractional durations" $ do
          let expected = Right (VDuration (Duration 0 (-1001)))
          run emptyEnv "duration('-1µs1.9ns')" `shouldBe` expected
          evaluate emptyEnv [cel| duration('-1µs1.9ns') |] `shouldBe` expected
          [celFn| duration('-1µs1.9ns') |] emptyEnv `shouldBe` expected
      , it "runtime and TH agree on nanosecond precision and invalid leap seconds" $ do
          let expected = Right (VString "1969-12-31T23:59:59.999999999Z")
          run emptyEnv "string(timestamp('1969-12-31T23:59:59.9999999999Z'))" `shouldBe` expected
          evaluate emptyEnv [cel| string(timestamp('1969-12-31T23:59:59.9999999999Z')) |] `shouldBe` expected
          [celFn| string(timestamp('1969-12-31T23:59:59.9999999999Z')) |] emptyEnv `shouldBe` expected
          let invalid = run emptyEnv "timestamp('2024-06-30T23:59:60Z')"
          case invalid of
            Left e -> errKind e `shouldBe` ErrConversion
            Right value -> expectationFailure ("Accepted invalid leap second: " <> show value)
          evaluate emptyEnv [cel| timestamp('2024-06-30T23:59:60Z') |] `shouldBe` invalid
          [celFn| timestamp('2024-06-30T23:59:60Z') |] emptyEnv `shouldBe` invalid
      , it "rejects duplicate comprehension binders in runtime and splice compilation" $ do
          let check source = case compile source of
                Left e -> errKind e `shouldBe` ErrParse
                Right expr -> expectationFailure ("Accepted " <> show expr)
          check "[4].all(x, x, x == 4)"
          check "[4].transformList(x, x, x)"
          check "true || [4].existsOne(x, x, x == 4)"
          check "[4].transformMap(x, x, true, x)"
          duplicateBinderRejected `shouldBe` True
      , it "rejects malformed macros before short-circuiting or dispatching to host overloads" $ do
          let env = addFunction "all" (const (Just (Right (VBool True)))) emptyEnv
              check source = case run env source of
                Left e -> errKind e `shouldBe` ErrParse
                Right value -> expectationFailure ("Accepted " <> show value)
          mapM_ check ["[1].all(42, false)", "[1].map(.x, x)", "[1].transformList(i, 42, i)", "false && has(1)"]
          malformedMacroRejected `shouldBe` True
          run env "[1].all(42)" `shouldBe` Right (VBool True)
          [celFn| [1].all(42) |] env `shouldBe` Right (VBool True)
      , it "interpreted and compiled transformMap keep the original list indices" $ do
          let source = "[1, 0, 3].transformMap(i, v, i % 2 == 0, 6 / v)"
              compiled = $(compileCelFn "[1, 0, 3].transformMap(i, v, i % 2 == 0, 6 / v)")
              expected = VMap (celMapFromList [(VInt 0, VInt 6), (VInt 2, VInt 2)])
          run emptyEnv source `shouldBe` Right expected
          compiled emptyEnv `shouldBe` run emptyEnv source
      , it "interpreted and compiled host maps agree on duplicate numeric keys" $ do
          let m = celMapFromList [(VInt 1, VString "first"), (VString "other", VInt 2), (VUInt 1, VString "last")]
              env = bind "m" (VMap m) emptyEnv
              source = "[size(m), m[1u], m.map(k, type(k))]"
              compiled = $(compileCelFn "[size(m), m[1u], m.map(k, type(k))]")
              expected = Right (VList (V.fromList [VInt 2, VString "last", VList (V.fromList [VType TyInt, VType TyString])]))
          run env source `shouldBe` expected
          compiled env `shouldBe` expected
      , it "deduplicates host-supplied double keys only for lossless matches" $ do
          let exactlyRepresented = 9007199254740992
              rounded = exactlyRepresented + 1
              m = celMapFromList [(VInt exactlyRepresented, VInt 1), (VInt rounded, VInt 2), (VDouble 9007199254740992, VInt 3)]
          length (celMapEntries m) `shouldBe` 2
          celMapLookup (VInt exactlyRepresented) m `shouldBe` Just (VInt 3)
          celMapLookup (VInt rounded) m `shouldBe` Just (VInt 2)
      , it "interpreted and compiled regex calls use the same RE2 semantics" $ do
          let expr = $(compileCel "input.matches(pattern)")
              compiled = $(compileCelFn "input.matches(pattern)")
              check regex = do
                let env = bind "input" (VString "FOO") (bind "pattern" (VString regex) emptyEnv)
                    result = run env "input.matches(pattern)"
                evaluate env expr `shouldBe` result
                compiled env `shouldBe` result
                pure result
          check "(?i)foo" >>= (`shouldBe` Right (VBool True))
          check "foo" >>= (`shouldBe` Right (VBool False))
          check "(?=FOO)" >>= \case
            Left e -> errKind e `shouldBe` ErrInvalid
            Right value -> expectationFailure ("Accepted unsupported RE2 lookahead: " <> show value)
      , it "interpreted and compiled timestamps agree on offset normalization and rejection" $ do
          invalidOffsetFn emptyEnv `shouldBe` run emptyEnv "timestamp('2026-11-10T00:00:00+00:99')"
          case invalidOffsetFn emptyEnv of
            Left e -> errKind e `shouldBe` ErrConversion
            Right value -> expectationFailure ("Accepted " <> show value)
          validOffsetFn emptyEnv `shouldBe` run emptyEnv "string(timestamp('1970-01-01t00:59:59.1+01:00'))"
          validOffsetFn emptyEnv `shouldBe` Right (VString "1969-12-31T23:59:59.1Z")
      , it "interpreted and compiled timezone accessors preserve offset seconds and milliseconds" $ do
          let source = "timestamp('1970-01-01T00:00:00.123Z').getSeconds(zone) + timestamp('1970-01-01T00:00:00.123Z').getMilliseconds(zone)"
              compiled =
                $(compileCelFn "timestamp('1970-01-01T00:00:00.123Z').getSeconds(zone) + timestamp('1970-01-01T00:00:00.123Z').getMilliseconds(zone)")
              check zone = do
                let env = bind "zone" (VString zone) emptyEnv
                    result = run env source
                compiled env `shouldBe` result
                pure result
          check "Africa/Monrovia" >>= (`shouldBe` Right (VInt 153))
          check "Not/AZone" >>= \case
            Left e -> errKind e `shouldBe` ErrInvalid
            Right value -> expectationFailure ("Accepted unknown timezone: " <> show value)
      , it "interpreted and compiled double conversions reject overflow" $ do
          let check input = do
                let env = bind "input" (VString input) emptyEnv
                parseDoubleFn env `shouldBe` run env "double(input)"
                case parseDoubleFn env of
                  Left e -> errKind e `shouldBe` ErrConversion
                  Right value -> expectationFailure ("Accepted " <> show value)
          mapM_ check ["1e309", "1.7976931348623159e308", "1e" <> T.replicate 10000 "9"]
      , it "interpreted and compiled doubles round-trip non-finite string values" $ do
          let compiled = $(compileCelFn "double(string(double(input)))")
              check input expected = do
                let env = bind "input" (VString input) emptyEnv
                case (run env "double(string(double(input)))", compiled env) of
                  (Right (VDouble interpreted), Right (VDouble generated)) -> do
                    expected interpreted `shouldBe` True
                    expected generated `shouldBe` True
                  results -> expectationFailure ("Unexpected non-finite conversion: " <> show results)
          check "Infinity" (== 1 / 0)
          check "-iNF" (== -1 / 0)
          check "nAn" isNaN
      , it "interpreted and compiled double conversions handle finite limits and huge underflow" $ do
          let check input expected = do
                let env = bind "input" (VString input) emptyEnv
                parseDoubleFn env `shouldBe` run env "double(input)"
                parseDoubleFn env `shouldBe` Right (VDouble expected)
          check "1.7976931348623157e308" 1.7976931348623157e308
          check "4.9406564584124654e-324" (encodeFloat 1 (-1074))
          check ("0e" <> T.replicate 10000 "9") 0
          let env = bind "input" (VString ("-1e-" <> T.replicate 10000 "9")) emptyEnv
          case (parseDoubleFn env, run env "double(input)") of
            (Right (VDouble compiled), Right (VDouble interpreted)) ->
              (isNegativeZero compiled, isNegativeZero interpreted) `shouldBe` (True, True)
            results -> expectationFailure ("Unexpected underflow result " <> show results)
      , it "rounds long decimal significands consistently in literals and conversions" $ do
          let midpoint = "1.00000000000000011102230246251565404236316680908203125"
              input = midpoint <> T.replicate 100000 "0" <> "1"
              env = bind "input" (VString input) emptyEnv
              expected = Right (VDouble (encodeFloat 4503599627370497 (-52)))
          run emptyEnv midpoint `shouldBe` Right (VDouble 1)
          run emptyEnv input `shouldBe` expected
          run env "double(input)" `shouldBe` expected
          parseDoubleFn env `shouldBe` expected
          evaluate emptyEnv
            $(compileCel ("1.00000000000000011102230246251565404236316680908203125" <> replicate 100000 '0' <> "1"))
            `shouldBe` expected
          $(compileCelFn ("1.00000000000000011102230246251565404236316680908203125" <> replicate 100000 '0' <> "1"))
            emptyEnv `shouldBe` expected
      , it "interpreted and compiled list indexes check bounds before narrowing" $ do
          let check value = do
                let env = bind "index" value emptyEnv
                    result = listIndexFn env
                result `shouldBe` run env "[42][index]"
                pure result
              outOfBounds value = do
                result <- check value
                case result of
                  Left e -> errKind e `shouldBe` ErrNoSuchKey
                  Right v -> expectationFailure ("Out-of-bounds index selected " <> show v)
          mapM_ outOfBounds [VDouble 18446744073709551616, VDouble (-18446744073709551616), VInt maxBound, VUInt maxBound]
          check (VInt 0) >>= (`shouldBe` Right (VInt 42))
          check (VUInt 0) >>= (`shouldBe` Right (VInt 42))
          check (VDouble 0) >>= (`shouldBe` Right (VInt 42))
      , it "source and both splices reject double literal overflow" $ do
          let check input = case run emptyEnv input of
                Left e -> errKind e `shouldBe` ErrParse
                Right value -> expectationFailure ("Accepted out-of-range literal " <> show value)
          check "1e309"
          check ("1e" <> T.replicate 10000 "9")
          overflowLiteralRejected `shouldBe` True
      , it "numeric map lookups use lossless conversion in both execution paths" $ do
          let check src compiled expected = do
                run emptyEnv src `shouldBe` Right expected
                compiled emptyEnv `shouldBe` Right expected
          check "{9007199254740993: 'wrong', 9007199254740992: 'right'}[9007199254740992.0]"
            [celFn| {9007199254740993: 'wrong', 9007199254740992: 'right'}[9007199254740992.0] |]
            (VString "right")
          check "{9223372036854775808u: 42}[9223372036854775808.0]"
            [celFn| {9223372036854775808u: 42}[9223372036854775808.0] |]
            (VInt 42)
          check "18446744073709551616.0 in {18446744073709551615u: 42}"
            [celFn| 18446744073709551616.0 in {18446744073709551615u: 42} |]
            (VBool False)
          let src = "{9223372036854775807: 42}[9223372036854775808.0]"
              compiled = [celFn| {9223372036854775807: 42}[9223372036854775808.0] |] emptyEnv
          compiled `shouldBe` run emptyEnv src
          case compiled of
            Left e -> errKind e `shouldBe` ErrNoSuchKey
            Right v -> expectationFailure ("Out-of-range key selected " <> show v)
      , it "finite source literals and signed underflow agree across both execution paths" $ do
          let maxFinite = Right (VDouble 1.7976931348623157e308)
          run emptyEnv "1.7976931348623157e308" `shouldBe` maxFinite
          [celFn| 1.7976931348623157e308 |] emptyEnv `shouldBe` maxFinite
          let compiled = [celFn| -1e-1000000000 |] emptyEnv
              interpreted = run emptyEnv "-1e-1000000000"
          case (compiled, interpreted) of
            (Right (VDouble a), Right (VDouble b)) -> (isNegativeZero a, isNegativeZero b) `shouldBe` (True, True)
            results -> expectationFailure ("Unexpected underflow result " <> show results)
      ]
