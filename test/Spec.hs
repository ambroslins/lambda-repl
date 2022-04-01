module Main where

import Data.List (singleton)
import Expr
import Parser
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Arbitrary (arbitrary), elements, oneof, resize, sized, (===))

main :: IO ()
main = hspec $ do
  describe "Parser.expr" $ do
    it "parses a variable" $
      parse expr "x" `shouldBe` pure (Variable "x")

    it "parses the identity function" $
      parse expr "\\x. x" `shouldBe` pure (Lambda "x" (Variable "x"))

    it "parses the const function" $
      parse expr "\\x. \\y. x" `shouldBe` pure (Lambda "x" (Lambda "y" (Variable "x")))

    it "parses function application" $
      parse expr "f x" `shouldBe` pure (Application (Variable "f") (Variable "x"))

    it "parses church numerals plus" $
      parse expr "\\n. \\m. \\f. \\x. n f (m f x)"
        `shouldBe` pure
          ( Lambda
              "n"
              ( Lambda
                  "m"
                  ( Lambda
                      "f"
                      ( Lambda
                          "x"
                          ( Application
                              (Application (Variable "n") (Variable "f"))
                              (Application (Application (Variable "m") (Variable "f")) (Variable "x"))
                          )
                      )
                  )
              )
          )
  describe "Expr.eval" $ do
    it "evaluates identity" $
      eval [] (Application (Lambda "x" (Variable "x")) (Variable "y")) `shouldBe` Variable "y"

    it "evaluates const" $
      eval [] (Application (Lambda "x" (Lambda "y" (Variable "x"))) (Variable "z")) `shouldBe` Lambda "y" (Variable "z")

    it "evaluates church numeral one plus two" $
      maybe (expectationFailure "parse error") (uncurry shouldBe) $ do
        one <- parse expr "\\f. \\x. f x"
        two <- parse expr "\\f. \\x. f (f x)"
        three <- parse expr "\\f. \\x. f (f (f x))"
        plus <- parse expr "\\n. \\m. \\f. \\x. n f (m f x)"
        onePlusTwo <- parse expr "plus one two"
        pure $ (eval [("one", one), ("two", two), ("plus", plus)] onePlusTwo, three)
  describe "Expr.pretty" $ do
    prop "roundtrips with parse" $ \e ->
      parse expr (pretty e) === Just e

instance Arbitrary Expr where
  arbitrary = sized $ \size ->
    let smaller = resize (size - 1) arbitrary
     in if size <= 0
          then Variable <$> varName
          else
            oneof
              [ Variable <$> varName,
                Lambda <$> varName <*> smaller,
                Application <$> smaller <*> smaller
              ]
    where
      varName = singleton <$> elements ['a' .. 'z']
