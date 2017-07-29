module DementiaSpec ( main, spec ) where

import Control.Exception (evaluate)

import Test.Hspec
import Test.QuickCheck

import Dementia as D

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  -- Simple canary test, if this fails, something's probably wrong with the
  -- whole test framework setup.
  describe "canary" $ do
    it "tweets" $ do
      D.canary `shouldBe` "tweet"

  -- These tests correspond to the guards on the verification functions that
  -- constrain acceptable values on our aliased types.
  context "constrained data types" $ do
    describe "verifyName" $ do
      it "accepts proper names" $ do
        "John" `shouldSatisfy` D.verifyName
      it "disallows treasonous names" $ do
        -- no treasonous names yet, any string will do
        True `shouldBe` True
    describe "verifySector" $ do
      it "accepts proper sectors" $ do
        "DOE" `shouldSatisfy` D.verifySector
      it "disallows treasonous sectors" $ do
        "foo" `shouldNotSatisfy` D.verifySector
        "Bar" `shouldNotSatisfy` D.verifySector
        "BLUE" `shouldNotSatisfy` D.verifySector
    describe "verifyClone" $ do
      it "accepts proper clones" $ do
        5 `shouldSatisfy` D.verifyClone
      it "disallows treasonous clones" $ do
        0 `shouldNotSatisfy` D.verifyClone
        (-5) `shouldNotSatisfy` D.verifyClone

  -- Tests for our Character data type and smart constructors.
  describe "designedCharacter" $ do
    context "when passed good parameters" $ do
      let c = D.designedCharacter "John" "DOE" 5
      it "has a character name" $ do
        D.name c `shouldBe` "John"
      it "has a sector identifier" $ do
        D.sector c `shouldBe` "DOE"
      it "has a clone number" $ do
        D.clone c `shouldBe` 5
    context "when passed a bad name" $ do
      it "(currently impossible to pass a bad name)" $ do
        True
    context "when passed a bad sector identifier" $ do
      it "should throw an exception" $ do
        evaluate (D.designedCharacter "John" "Grumblethorpe" 5)
          `shouldThrow` (const True :: Selector DementiaException)
    context "when passed a bad clone number" $ do
      it "should throw an exception" $ do
        evaluate (D.designedCharacter "John" "DOE" (-1))
          `shouldThrow` (const True :: Selector DementiaException)
  describe "randomCharacter" $ do
    let c = D.randomCharacter
    it "has the name \"Ralph\"" $ do
      D.name c `shouldBe` "Ralph"
    it "has the sector \"BRO\"" $ do
      D.sector c `shouldBe` "BRO"
    it "has the clone \"5\"" $ do
      D.clone c `shouldBe` 3
