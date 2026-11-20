{-# LANGUAGE OverloadedStrings #-}

{- | Example-based conformance tests for the CEL implementation. The cases are
taken directly from the worked examples in the CEL language definition
(<https://github.com/google/cel-spec/blob/master/doc/langdef.md>) plus a set
of edge cases the prose calls out (overflow, NaN, heterogeneous equality,
error-absorbing logical operators, escape sequences, …).
-}
module Test.CEL.Conformance (tests) where

import CEL
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Test.Syd


-- Structural value equality for assertions: unlike the language's
-- heterogeneous 'valueEq', this distinguishes @1@, @1u@, and @1.0@ so type
-- regressions are caught.
structuralEq :: Value -> Value -> Bool
structuralEq a b = case (a, b) of
  (VNull, VNull) -> True
  (VBool x, VBool y) -> x == y
  (VInt x, VInt y) -> x == y
  (VUInt x, VUInt y) -> x == y
  (VDouble x, VDouble y) -> (isNaN x && isNaN y) || x == y
  (VString x, VString y) -> x == y
  (VBytes x, VBytes y) -> x == y
  (VType x, VType y) -> typeNameText x == typeNameText y
  (VDuration x, VDuration y) -> x == y
  (VTimestamp x, VTimestamp y) -> x == y
  (VList x, VList y) ->
    V.length x == V.length y && and (zipWith structuralEq (V.toList x) (V.toList y))
  (VMap x, VMap y) ->
    let ex = celMapEntries x
        ey = celMapEntries y
    in length ex == length ey
         && and (zipWith (\(k1, v1) (k2, v2) -> structuralEq k1 k2 && structuralEq v1 v2) ex ey)
  _ -> False


ok :: Text -> Value -> Spec
ok = okEnv emptyEnv


okEnv :: Env -> Text -> Value -> Spec
okEnv env src expected = it (T.unpack src) $
  case run env src of
    Right v
      | structuralEq v expected -> pure ()
      | otherwise -> expectationFailure ("expected " ++ show expected ++ " but got " ++ show v)
    Left e -> expectationFailure ("expected a value but got error: " ++ show e)


err :: Text -> Spec
err src = it (T.unpack src ++ " [error]") $
  case run emptyEnv src of
    Left _ -> pure ()
    Right v -> expectationFailure ("expected an error but got " ++ show v)


true, false :: Text -> Spec
true s = ok s (VBool True)
false s = ok s (VBool False)


tests :: Spec
tests =
  describe
    "conformance"
    $ sequence_
      [ literals
      , arithmetic
      , comparisons
      , equality
      , logical
      , conditionals
      , stringsAndBytes
      , collections
      , macros
      , conversions
      , typeValues
      , datetime
      , names
      , parseErrors
      ]


literals :: Spec
literals =
  describe
    "literals"
    $ sequence_
      [ ok "1" (VInt 1)
      , ok "7u" (VUInt 7)
      , ok "7.0" (VDouble 7.0)
      , ok "7e0" (VDouble 7.0)
      , ok ".700e1" (VDouble 7.0)
      , ok "0x1F" (VInt 31)
      , ok "0xFFu" (VUInt 255)
      , ok "true" (VBool True)
      , ok "false" (VBool False)
      , ok "null" VNull
      , ok "-9223372036854775808" (VInt minBound)
      , ok "\"hello\"" (VString "hello")
      , ok "'world'" (VString "world")
      ]


arithmetic :: Spec
arithmetic =
  describe
    "arithmetic"
    $ sequence_
      [ ok "1 + 2" (VInt 3)
      , ok "3.14 + 1.59" (VDouble 4.73)
      , ok "5 - 3" (VInt 2)
      , ok "10.5 - 2.0" (VDouble 8.5)
      , ok "10 / 2" (VInt 5)
      , ok "7.0 / 2.0" (VDouble 3.5)
      , ok "3 % 2" (VInt 1)
      , ok "6u % 3u" (VUInt 0)
      , ok "3.5 * 40.0" (VDouble 140.0)
      , ok "-2 * 6" (VInt (-12))
      , ok "13u * 3u" (VUInt 39)
      , ok "-(5)" (VInt (-5))
      , ok "-(3.14)" (VDouble (-3.14))
      , ok "1 + 2 * 3" (VInt 7)
      , ok "(1 + 2) * 3" (VInt 9)
      , -- mixed-type arithmetic does not dispatch
        err "1 + 1u"
      , err "1 + 2.0"
      , -- division / modulus by zero
        err "1 / 0"
      , err "1 % 0"
      , err "1u / 0u"
      , -- overflow
        err "9223372036854775807 + 1"
      , err "-9223372036854775808 / -1"
      , ok "9223372036854775807 + 0" (VInt maxBound)
      ]


comparisons :: Spec
comparisons =
  describe
    "comparisons"
    $ sequence_
      [ true "2 < 3"
      , true "2 <= 3"
      , true "3 >= 2"
      , true "3 > 2"
      , true "'a' < 'b'"
      , true "'b' >= 'a'"
      , -- cross-type numeric ordering at runtime
        true "-1 < dyn(1u)"
      , false "1 >= dyn(18446744073709551615u)"
      , true "dyn(3.0) == 3"
      , -- NaN ordering is always false
        false "(0.0/0.0) < 1.0"
      , false "(0.0/0.0) > 1.0"
      , false "(0.0/0.0) == (0.0/0.0)"
      , true "(0.0/0.0) != (0.0/0.0)"
      , -- ordering across incompatible types is an error
        err "1 < 'a'"
      ]


equality :: Spec
equality =
  describe
    "equality"
    $ sequence_
      [ true "1 == 1"
      , false "\"hello\" == \"world\""
      , true "1 != 2"
      , false "'a' != 'a'"
      , true "3.0 != 3.1"
      , -- heterogeneous numeric equality
        true "1 == 1u"
      , true "1 == 1.0"
      , true "1u == 1.0"
      , -- different non-numeric types are simply unequal (no error)
        false "1 == 'a'"
      , false "'a' == null"
      , true "null == null"
      , -- aggregate equality
        true "[1, 2, 3] == [1, 2, 3]"
      , false "[1, 2] == [1, 2, 3]"
      , true "{'a': 1, 'b': 2} == {'b': 2, 'a': 1}"
      , true "bytes('hello') == b'hello'"
      ]


logical :: Spec
logical =
  describe
    "logical operators (error-absorbing)"
    $ sequence_
      [ true "true || false"
      , false "false || false"
      , true "true && true"
      , false "true && false"
      , true "!false"
      , false "!true"
      , -- short-circuit / error absorption
        true "true || (1/0 == 0)"
      , false "false && (1/0 == 0)"
      , -- commutative error absorption
        true "(1/0 == 0) || true"
      , false "(1/0 == 0) && false"
      , -- error propagates when not absorbed
        err "(1/0 == 0) && true"
      , err "(1/0 == 0) || false"
      , err "!(1/0 == 0)"
      ]


conditionals :: Spec
conditionals =
  describe
    "conditional"
    $ sequence_
      [ ok "true ? 1 : 2" (VInt 1)
      , ok "false ? \"a\" : \"b\"" (VString "b")
      , ok "(2 < 5) ? 'yes' : 'no'" (VString "yes")
      , -- only the taken branch is evaluated
        ok "false ? (1/0) : 42" (VInt 42)
      , ok "('hello'.size() > 10) ? 1 / 0 : 42" (VInt 42)
      , err "true ? (1/0) : 42"
      ]


stringsAndBytes :: Spec
stringsAndBytes =
  describe
    "strings and bytes"
    $ sequence_
      [ ok "\"Hello, \" + \"world!\"" (VString "Hello, world!")
      , true "\"hello world\".contains(\"world\")"
      , false "\"foobar\".contains(\"baz\")"
      , true "\"hello world\".endsWith(\"world\")"
      , true "\"hello world\".startsWith(\"hello\")"
      , true "\"foobar\".matches(\"foo.*\")"
      , true "matches(\"foobar\", \"foo.*\")"
      , err "contains('banana', 'ana')"
      , err "startsWith('banana', 'ban')"
      , err "endsWith('banana', 'ana')"
      , true "'prefix FOO suffix'.matches('(?i)foo')"
      , true "'123'.matches(r'^(?:\\d+)$')"
      , false "'abc'.matches(r'^(?:\\d+)$')"
      , true "'Καλημέρα'.matches(r'^\\p{Greek}+$')"
      , false "'foo\\nbar'.matches('^bar$')"
      , true "'foo\\nbar'.matches('(?m)^bar$')"
      , true "'a\\x00b'.matches('\\x00b$')"
      , true "''.matches('')"
      , err "'aa'.matches(r'^(a)\\1$')"
      , ok "\"hello\".size()" (VInt 5)
      , ok "size(\"world!\")" (VInt 6)
      , ok "\"fiance\\u0301\".size()" (VInt 7)
      , -- escape sequences
        ok "'\\u00ff'" (VString "\xff")
      , -- raw strings keep backslashes
        ok "r\"\\\\\"" (VString "\\\\")
      , -- bytes literals and size
        ok "b'hello'.size()" (VInt 5)
      , ok "size(b'\\xF0\\x9F\\xA4\\xAA')" (VInt 4)
      , ok "size(string(b'\\xF0\\x9F\\xA4\\xAA'))" (VInt 1)
      , ok "b'\\377'" (VBytes (BS.pack [255]))
      , ok "bytes('🤪')" (VBytes (BS.pack [0xF0, 0x9F, 0xA4, 0xAA]))
      ]


collections :: Spec
collections =
  describe
    "lists and maps"
    $ sequence_
      [ ok "[1, 2, 3]" (VList (V.fromList [VInt 1, VInt 2, VInt 3]))
      , ok "[1] + [2, 3]" (VList (V.fromList [VInt 1, VInt 2, VInt 3]))
      , ok "[1, 2, 3][1]" (VInt 2)
      , true "2 in [1, 2, 3]"
      , false "\"a\" in [\"b\", \"c\"]"
      , ok "['hello', 'world'].size()" (VInt 2)
      , ok "size(['first', 'second', 'third'])" (VInt 3)
      , -- maps
        ok "{'key1': 'value1', 'key2': 'value2'}['key1']" (VString "value1")
      , ok "{'name': 'Bob', 'age': 42}['age']" (VInt 42)
      , true "'key1' in {'key1': 'value1', 'key2': 'value2'}"
      , false "3 in {1: \"one\", 2: \"two\"}"
      , ok "{'hello': 'world'}.size()" (VInt 1)
      , ok "size({1: true, 2: false})" (VInt 2)
      , -- numeric cross-type map key indexing
        ok "{1: 'hello', 2: 'world'}[dyn(1.0)]" (VString "hello")
      , -- selection equals string indexing on maps
        ok "{'foo': 7}.foo" (VInt 7)
      , -- out-of-range / missing key are errors
        err "[1, 2, 3][5]"
      , err "{'a': 1}['b']"
      , -- duplicate map keys are an error
        err "{'a': 1, 'a': 2}"
      , -- invalid key type is an error
        err "{[1]: 2}"
      ]


macros :: Spec
macros =
  describe
    "macros"
    $ sequence_
      [ true "[1, 2, 3].all(x, x > 0)"
      , false "[1, 2, 0].all(x, x > 0)"
      , true "['apple', 'banana', 'cherry'].all(fruit, fruit.size() > 3)"
      , false "[3.14, 2.71, 1.61].all(num, num < 3.0)"
      , false "{'a': 1, 'b': 2, 'c': 3}.all(key, key != 'b')"
      , true "[1, 2, 3].exists(i, i % 2 != 0)"
      , false "[].exists(i, i > 0)"
      , true "[0, -1, 5].exists(num, num < 0)"
      , false "{'x': 'foo', 'y': 'bar'}.exists(key, key.startsWith('z'))"
      , true "[1, 2, 2].exists_one(i, i < 2)"
      , false "{'a': 'hello', 'aa': 'hellohello'}.exists_one(k, k.startsWith('a'))"
      , false "[1, 2, 3, 4].exists_one(num, num % 2 == 0)"
      , ok "[1, 2, 3].filter(x, x > 1)" (VList (V.fromList [VInt 2, VInt 3]))
      , ok "[1, 2, 3].map(x, x * 2)" (VList (V.fromList [VInt 2, VInt 4, VInt 6]))
      , ok "[1, 2, 3].map(n, n * n)" (VList (V.fromList [VInt 1, VInt 4, VInt 9]))
      , ok "[1, 2, 3, 4].map(num, num % 2 == 0, num * 2)" (VList (V.fromList [VInt 4, VInt 8]))
      , ok "{'one': 1, 'two': 2}.map(k, k)" (VList (V.fromList [VString "one", VString "two"]))
      , ok "[1, 2, 3].transformMap(i, v, i + v)" (VMap (celMapFromList [(VInt 0, VInt 1), (VInt 1, VInt 3), (VInt 2, VInt 5)]))
      , ok "[1, 0, 3].transformMap(i, v, i % 2 == 0, 6 / v)" (VMap (celMapFromList [(VInt 0, VInt 6), (VInt 2, VInt 2)]))
      , ok "[].transformMap(i, v, v)" (VMap (celMapFromList []))
      , true "{'a': 1}.transformMap(k, v, v + 1) == {'a': 2}"
      , err "[0].transformMap(i, v, true, 6 / v)"
      , -- error absorption consistent with && / ||
        true "[1, 0, 3].exists(x, 4 / x == 4)"
      , false "[2, 0, 3].all(x, 4 / x == 4)"
      , -- has() on maps
        true "has({'a': 1}.a)"
      , false "has({'a': 1}.b)"
      , true "has({'a': null}.a)"
      ]


conversions :: Spec
conversions =
  describe
    "conversions"
    $ sequence_
      [ ok "bool(true)" (VBool True)
      , ok "bool(\"true\")" (VBool True)
      , ok "bool(\"FALSE\")" (VBool False)
      , err "('1').int()"
      , err "true.string()"
      , err "getSeconds(duration('1s'))"
      , ok "bytes(\"hello\")" (VBytes "hello")
      , ok "double(10)" (VDouble 10.0)
      , ok "double(\"3.14\")" (VDouble 3.14)
      , ok "double(string(double('Infinity')))" (VDouble (1 / 0))
      , ok "double(string(double('-Infinity')))" (VDouble (-1 / 0))
      , ok "double('inf')" (VDouble (1 / 0))
      , ok "double('-iNF')" (VDouble (-1 / 0))
      , ok "double('nAn')" (VDouble (0 / 0))
      , err "double('+NaN')"
      , ok "int(123)" (VInt 123)
      , ok "int(3.14)" (VInt 3)
      , ok "int(\"123\")" (VInt 123)
      , ok "int(-3.9)" (VInt (-3))
      , ok "uint(123)" (VUInt 123)
      , ok "uint(3.14)" (VUInt 3)
      , ok "uint(\"123\")" (VUInt 123)
      , ok "string(123)" (VString "123")
      , ok "string(true)" (VString "true")
      , ok "string(b'hello')" (VString "hello")
      , ok "int(1u)" (VInt 1)
      , ok "uint(1)" (VUInt 1)
      , ok "double(1u)" (VDouble 1.0)
      , -- range / validity errors
        err "uint(-1)"
      , err "int(1e19)"
      , err "uint(-1.0)"
      , err "int(\"abc\")"
      , err "bool(\"maybe\")"
      ]


typeValues :: Spec
typeValues =
  describe
    "type values"
    $ sequence_
      [ ok "type(1)" (VType (typeOfName "int"))
      , ok "type(\"a\")" (VType (typeOfName "string"))
      , false "type(1) == string"
      , true "type(type(1)) == type(string)"
      , true "type(1) == int"
      , true "type(1u) == uint"
      , true "type(3.0) == double"
      , true "type(true) == bool"
      , true "type(null) == null_type"
      , true "type([1]) == list"
      , true "type({1: 2}) == map"
      ]
  where
    typeOfName n = case run emptyEnv n of
      Right (VType t) -> t
      _ -> error ("not a type: " <> T.unpack n)


datetime :: Spec
datetime =
  describe
    "date/time"
    $ sequence_
      [ ok "duration('1m') + duration('1s')" (durSecs 61)
      , ok "duration('1m') - duration('1s')" (durSecs 59)
      , ok "duration('-1.5h')" (durSecs (-5400))
      , ok "duration('0')" (durSecs 0)
      , ok "duration('.5s1.ms2µs3μs')" (VDuration (Duration 0 501005000))
      , ok "duration('0.9ns0.9ns')" (durSecs 0)
      , ok "duration('-1.9ns')" (VDuration (Duration 0 (-1)))
      , err "duration(' 1s ')"
      , err "duration('.s')"
      , ok "string(duration('1m1ms'))" (VString "60.001s")
      , ok "duration('3h').getHours()" (VInt 3)
      , ok "duration('1h30m').getMinutes()" (VInt 90)
      , ok "duration('1m30s').getSeconds()" (VInt 90)
      , ok "duration('1.234s').getMilliseconds()" (VInt 234)
      , ok "timestamp('2023-12-25T00:00:00Z').getDate()" (VInt 25)
      , ok "timestamp('2023-12-25T12:00:00Z').getFullYear()" (VInt 2023)
      , ok "timestamp('2023-12-25T12:00:00Z').getMonth()" (VInt 11)
      , ok "timestamp('2023-12-25T12:00:00Z').getDayOfWeek()" (VInt 1)
      , ok "timestamp('2023-12-25T12:00:00Z').getDayOfYear()" (VInt 358)
      , ok "timestamp('2023-12-25T12:00:00Z').getHours()" (VInt 12)
      , ok "timestamp('2023-12-25T12:30:30Z').getSeconds()" (VInt 30)
      , ok "timestamp('2023-12-25T12:00:00.500Z').getMilliseconds()" (VInt 500)
      , ok "string(timestamp('1970-01-01T00:00:00.9999999999Z'))" (VString "1970-01-01T00:00:00.999999999Z")
      , ok "string(timestamp('9999-12-31T23:59:59.9999999999Z'))" (VString "9999-12-31T23:59:59.999999999Z")
      , err "timestamp('2024-06-30T23:59:60Z')"
      , ok "timestamp('1900-01-01T00:00:00Z').getSeconds('Europe/Paris')" (VInt 21)
      , ok "timestamp('1970-01-01T00:00:00Z').getSeconds('Africa/Monrovia')" (VInt 30)
      , ok "timestamp('1970-01-01T00:44:45Z').getDate('Africa/Monrovia')" (VInt 1)
      , ok "timestamp('1970-01-01T00:00:00.123Z').getMilliseconds('Africa/Monrovia')" (VInt 123)
      , ok "timestamp('1970-01-01T00:00:00.123Z').getMilliseconds('+05:30')" (VInt 123)
      , err "timestamp('1970-01-01T00:00:00.123Z').getMilliseconds('Not/AZone')"
      , ok "string(timestamp('2023-01-01T00:00:00Z') + duration('24h'))" (VString "2023-01-02T00:00:00Z")
      , ok "string(timestamp('2023-01-10T12:00:00Z') - timestamp('2023-01-10T00:00:00Z'))" (VString "43200s")
      , true "timestamp('2023-08-25T12:00:00Z') <= timestamp('2023-08-26T12:00:00Z')"
      , -- timezone with fixed offset
        ok "timestamp('2023-12-25T00:00:00Z').getDate('-08:00')" (VInt 24)
      ]
  where
    durSecs n = VDuration (Duration n 0)


names :: Spec
names =
  describe
    "name resolution"
    $ sequence_
      [ okEnv (bind "x" (VInt 5) emptyEnv) "x + 1" (VInt 6)
      , okEnv (bind "name" (VString "world") emptyEnv) "'Hello, ' + name + '!'" (VString "Hello, world!")
      , okEnv (bind "a" mapAB emptyEnv) "a.b" (VInt 1)
      , okEnv (bind "a" nestedMap emptyEnv) "a.b.c" (VInt 7)
      , okEnv (bind "size" (VInt 9) (bind "requests" (VList (V.fromList [VInt 1, VInt 2])) emptyEnv)) "size(requests) > size" (VBool False)
      , -- comprehension variable shadows outer binding
        okEnv (bind "x" (VInt 100) emptyEnv) "[1, 2, 3].all(x, x < 4)" (VBool True)
      , -- container-relative resolution
        okEnv (withContainer "a.b" (bind "a.b.x" (VInt 1) emptyEnv)) "x" (VInt 1)
      , -- undeclared variable is an error
        err "undefinedVar"
      ]
  where
    mapAB = VMap (celMapFromList [(VString "b", VInt 1)])
    nestedMap = VMap (celMapFromList [(VString "b", VMap (celMapFromList [(VString "c", VInt 7)]))])


parseErrors :: Spec
parseErrors =
  describe
    "parse errors"
    $ sequence_
      [ err "1 +"
      , err "(1"
      , err "for"
      , err "1 2"
      , err "'\\q'"
      , -- UTF-16 surrogate code points are invalid even when escaped
        err "'\\uD83D'"
      ]
