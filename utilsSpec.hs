module UtilSpec where

import Test.Hspec
import Utils

main = hspec $ do
  describe "Utils" $ do
      describe "round-Robin" $ do
        it "returns series form lists taking one by one" $ do
          roundRobin ["ab", "12"] `shouldBe` "a1b2"

        it "returns by skipping list elements when exhauste" $ do
          roundRobin ["ab", "c", "def", "g"] `shouldBe` "acdgbef"

      describe "split" $ do
        it "returns list of string by separating by specified char(s)" $ do
          split ';' "a;bcd;eff;;h" `shouldBe` ["a", "bcd", "eff", "h"]

      describe "join" $ do
        it "returns string by concatenating list of strings" $ do
            join "-" ["A", "bc", "13"] `shouldBe` "A-bc-13"

      describe "combinations" $ do
        it "returns all combinations of sublists by taking n elements" $ do
            combinations 3 "abcd" `shouldBe` ["abc","abd","acd","bcd"]
