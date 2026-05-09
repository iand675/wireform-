module JsonSchema.JsonPointerSpec (spec) where

import Test.Hspec
import JsonSchema.Types
import Data.Text (Text)
import Data.Either (isLeft)

spec :: Spec
spec = describe "JSON Pointer (RFC 6901)" $ do
  
  describe "escaping and parsing" $ do
    it "handles empty pointer" $ do
      renderPointer emptyPointer `shouldBe` ""
      parsePointer "" `shouldBe` Right emptyPointer
    
    it "handles simple path" $ do
      let ptr = emptyPointer /. "foo" /. "bar"
      renderPointer ptr `shouldBe` "/foo/bar"
      parsePointer "/foo/bar" `shouldBe` Right ptr
    
    it "escapes ~ correctly" $ do
      let ptr = emptyPointer /. "tilde~field"
      renderPointer ptr `shouldBe` "/tilde~0field"
      parsePointer "/tilde~0field" `shouldBe` Right ptr
    
    it "escapes / correctly" $ do
      let ptr = emptyPointer /. "slash/field"
      renderPointer ptr `shouldBe` "/slash~1field"
      parsePointer "/slash~1field" `shouldBe` Right ptr
    
    it "escapes both ~ and / correctly" $ do
      let ptr = emptyPointer /. "~/"
      renderPointer ptr `shouldBe` "/~0~1"
      parsePointer "/~0~1" `shouldBe` Right ptr
    
    it "handles URL-encoded % in pointers" $ do
      -- When pointer is in a URI fragment, % might be encoded as %25
      let ptr = emptyPointer /. "percent%field"
      parsePointer "/percent%25field" `shouldBe` Right ptr
    
    it "roundtrips complex paths" $ do
      let ptr = emptyPointer /. "a/b" /. "c~d" /. "e%f"
      let rendered = renderPointer ptr
      parsePointer rendered `shouldBe` Right ptr
  
  describe "RFC 6901 examples" $ do
    it "parses whole document reference" $ do
      parsePointer "" `shouldBe` Right emptyPointer
    
    it "parses /foo" $ do
      parsePointer "/foo" `shouldBe` Right (emptyPointer /. "foo")
    
    it "parses /foo/0" $ do
      parsePointer "/foo/0" `shouldBe` Right (emptyPointer /. "foo" /. "0")
    
    it "parses /" $ do
      parsePointer "/" `shouldBe` Right (emptyPointer /. "")
    
    it "parses /a~1b (contains /))" $ do
      parsePointer "/a~1b" `shouldBe` Right (emptyPointer /. "a/b")
    
    it "parses /c%d (no escaping for %))" $ do
      parsePointer "/c%d" `shouldBe` Right (emptyPointer /. "c%d")
    
    it "parses /e^f (no escaping for ^)" $ do
      parsePointer "/e^f" `shouldBe` Right (emptyPointer /. "e^f")
    
    it "parses /g|h (no escaping for |)" $ do
      parsePointer "/g|h" `shouldBe` Right (emptyPointer /. "g|h")
    
    it "parses /i\\j (no escaping for \\)" $ do
      parsePointer "/i\\j" `shouldBe` Right (emptyPointer /. "i\\j")
    
    it "parses /k\"l (no escaping for \")" $ do
      parsePointer "/k\"l" `shouldBe` Right (emptyPointer /. "k\"l")
    
    it "parses / (empty string key)" $ do
      parsePointer "/ " `shouldBe` Right (emptyPointer /. " ")
    
    it "parses /m~0n (contains ~)" $ do
      parsePointer "/m~0n" `shouldBe` Right (emptyPointer /. "m~n")
  
  describe "error handling" $ do
    it "rejects pointers not starting with /" $ do
      parsePointer "foo/bar" `shouldSatisfy` isLeft
    
    it "rejects pointers not starting with / (unless empty)" $ do
      parsePointer "x" `shouldSatisfy` isLeft
  
  describe "roundtrip property" $ do
    it "roundtrips single segment" $ do
      let ptr = emptyPointer /. "test"
      parsePointer (renderPointer ptr) `shouldBe` Right ptr
    
    it "roundtrips multiple segments" $ do
      let ptr = emptyPointer /. "foo" /. "bar" /. "baz"
      parsePointer (renderPointer ptr) `shouldBe` Right ptr
    
    it "roundtrips with special characters" $ do
      let ptr = emptyPointer /. "a~b" /. "c/d" /. "e%f"
      parsePointer (renderPointer ptr) `shouldBe` Right ptr

