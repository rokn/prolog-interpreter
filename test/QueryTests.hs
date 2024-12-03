module QueryTests where

import Test.Hspec

import TestUtils

import Interpreter.PrologInterpreter
import Interpreter.PrologInterpreterInternal
import Parser.PrologParserInternal
import Parser.Parser

getAllAnswers :: Query -> [Answer]
getAllAnswers q = case answerQuery q of
                      Just (Result a newQ) -> a : getAllAnswers newQ
                      Nothing -> []


queryTests = do
  let program = unsafeParseFile "prolog/test1.pl"
      makeQ   = createQuery program . unsafeParseQuestion
      termP s = fromRight (eval term s)
      s1 =:= s2 = termP s1 := termP s2

  describe "Basic fact checking" $ do
      it "Giving true results" $ do
        getAllAnswers (makeQ "?- male(james1).") `shouldBe` [[]]
        getAllAnswers (makeQ "?- male(X).") `shouldBeWithLength` 5
        getAllAnswers (makeQ "?- parent(charles1, X).") `shouldBe` [["X" =:= "james1"]]
        getAllAnswers (makeQ "?- parent(charles1, X).") `shouldBe` [["X" =:= "james1"]]

      it "Giving false results" $ do
        getAllAnswers (makeQ "?- male(isThisRealMan).") `shouldBe` []
        getAllAnswers (makeQ "?- female(james2).") `shouldBe` []
        getAllAnswers (makeQ "?- parent(james1, charles1).") `shouldBe` []
        getAllAnswers (makeQ "?- parent(noone, X).") `shouldBe` []


  describe "Basic resolution no recursion" $ do
      it "Giving true results" $ do
        getAllAnswers (makeQ "?- sister(james2, X).") `shouldBe` [["X" =:= "catherine"]]
        getAllAnswers (makeQ "?- brother(catherine, X).") `shouldBeWithLength` 2
        getAllAnswers (makeQ "?- sister(X, elizabeth).") `shouldBeWithLength` 3
        getAllAnswers (makeQ "?- brother(X, george1).") `shouldBe` [["X" =:= "george1"]]

      it "Giving false results" $ do
        getAllAnswers (makeQ "?- sister(george1, X).") `shouldBe` []
        getAllAnswers (makeQ "?- sister(george1, X).") `shouldBe` []
        getAllAnswers (makeQ "?- sister(X, george1).") `shouldBe` []


  describe "Basic list operations" $ do
      it "Giving true results" $ do
        getAllAnswers (makeQ "?- head([1,2,3], X).") `shouldBe` [["X" =:= "1"]]
        getAllAnswers (makeQ "?- head([a], X).") `shouldBe` [["X" =:= "a"]]
        getAllAnswers (makeQ "?- head([1], 1).") `shouldBe` [[]]
        getAllAnswers (makeQ "?- length([a,b,c], X).") `shouldBe` [["X" =:= "3"]]
        getAllAnswers (makeQ "?- last([a,b,c], X).") `shouldBe` [["X" =:= "c"]]

      it "Giving false results" $ do
        getAllAnswers (makeQ "?- head([], X).") `shouldBe` []
        getAllAnswers (makeQ "?- head([1], 2).") `shouldBe` []
        getAllAnswers (makeQ "?- length(notAList, X).") `shouldBe` []
        getAllAnswers (makeQ "?- last([], X).") `shouldBe` []

  describe "Basic 'not' usage" $ do
    it "Giving true results" $ do
      getAllAnswers (makeQ "?- not(brother(charles1, charles2)).") `shouldBe` [[]]
      getAllAnswers (makeQ "?- not(last([1,2,3], 2)).") `shouldBe` [[]]

    it "Giving false results" $ do
      getAllAnswers (makeQ "?- not(sister(charles1, elizabeth)).") `shouldBe` []
      getAllAnswers (makeQ "?- not(last([1,2,3], 3)).") `shouldBe` []


  describe "Basic 'is' usage" $ do
    it "Unifiying with constant terms" $ do
      getAllAnswers (makeQ "?- X is 3.") `shouldBe` [["X" =:= "3"]]
      getAllAnswers (makeQ "?- X is a.") `shouldBe` [["X" =:= "a"]]
      getAllAnswers (makeQ "?- X is f(c(a)).") `shouldBe` [["X" =:= "f(c(a))"]]

    it "Unifiying with other variables" $ do
      getAllAnswers (makeQ "?- Y is a, X is Y.") `shouldBe` [["Y" =:= "a", "X" =:= "a"]]
      getAllAnswers (makeQ "?- female(Y), X is Y.") `shouldBeWithLength` 3
