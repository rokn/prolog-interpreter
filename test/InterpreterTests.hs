module InterpreterTests where

import Test.Hspec
import Data.Maybe (fromJust)

import Interpreter.PrologInterpreterInternal
import Parser.PrologParser
import Parser.PrologParserInternal
import Parser.Parser


interpreterTests = do
  let termP s = fromRight (eval term s)
      ruleP s = fromRight (eval rule s)
      factP s = fromRight (eval fact s)
      s1 =:= s2 = termP s1 := termP s2

  describe "unify" $ do
    it "unifying unifiable equalities" $ do
      unify ["a" =:= "a"] `shouldBe` Just []
      unify ["a" =:= "X"] `shouldBe` Just ["X" =:= "a"]
      unify ["X" =:= "X"] `shouldBe` Just []
      unify ["X" =:= "Y"] `shouldBe` Just ["X" =:= "Y"]
      fromJust (unify ["X" =:= "a", "Z" =:= "g(X)"]) `shouldMatchList` ["X" =:= "a", "Z" =:= "g(a)"]
      unify ["f(a, X)" =:= "f(a, b)"] `shouldBe` Just ["X" =:= "b"]
      unify ["f(X)" =:= "f(Y)"] `shouldBe` Just ["X" =:= "Y"]
      unify ["f(g(X))" =:= "f(Y)"] `shouldBe` Just ["Y" =:= "g(X)"]
      fromJust (unify ["f(g(X), X)" =:= "f(Y, a)"]) `shouldMatchList` ["Y" =:= "g(a)", "X" =:= "a"]

    it "failing on wrong equalities" $ do
      unify ["a" =:= "b"] `shouldBe` Nothing
      unify ["f(a)" =:= "g(a)"] `shouldBe` Nothing
      unify ["f(X)" =:= "g(Y)"] `shouldBe` Nothing
      unify ["f(X)" =:= "f(Y, Z)"] `shouldBe` Nothing
      unify ["f(X, Z)" =:= "f(Y)"] `shouldBe` Nothing
      unify ["X" =:= "f(X)"] `shouldBe` Nothing

  describe "substitution" $ do
    it "substituting in term" $ do
      substituteInTerm ["X" =:= "g(a)"] (termP "a") `shouldBe` termP "a"
      substituteInTerm ["X" =:= "g(a)"] (termP "X") `shouldBe` termP "g(a)"
      substituteInTerm ["X" =:= "g(a)"] (termP "X") `shouldBe` termP "g(a)"
      substituteInTerm ["X" =:= "g(a)"] (termP "Z") `shouldBe` termP "Z"
      substituteInTerm ["X" =:= "g(a)"] (termP "f(a, X)") `shouldBe` termP "f(a, g(a))"
      substituteInTerm ["X" =:= "g(a)"] (termP "f(Y, X)") `shouldBe` termP "f(Y, g(a))"
      substituteInTerm ["X" =:= "g(a)"] (termP "f(X)") `shouldBe` termP "f(g(a))"
      substituteInTerm ["X" =:= "g(a)"] (termP "f(g(X))") `shouldBe` termP "f(g(g(a)))"

    it "substituting in rule" $ do
      substituteInRule ["X" =:= "f(g(a))"] (ruleP "f(a, X) :- g(b).") `shouldBe` ruleP "f(a, f(g(a))) :- g(b)."
      substituteInRule ["X" =:= "f(g(a))"] (ruleP "f(Y, X) :- g(X), y(x).") `shouldBe` ruleP "f(Y, f(g(a))) :- g(f(g(a))), y(x)."

  describe "variable searching" $ do
    it "variables in term" $ do
      varsTerm (termP "a") `shouldMatchList` []
      varsTerm (termP "X") `shouldMatchList` ["X"]
      varsTerm (termP "f(a)") `shouldMatchList` []
      varsTerm (termP "f(X)") `shouldMatchList` ["X"]
      varsTerm (termP "f(a, X)") `shouldMatchList` ["X"]
      varsTerm (termP "f(a, X, Y)") `shouldMatchList` ["X", "Y"]
      varsTerm (termP "f(X, X)") `shouldMatchList` ["X"]
      varsTerm (termP "f(a, X, Y, X)") `shouldMatchList` ["X", "Y"]
      varsTerm (termP "f(g(a))") `shouldMatchList` []
      varsTerm (termP "f(g(X))") `shouldMatchList` ["X"]
      varsTerm (termP "f(g(X), a)") `shouldMatchList` ["X"]
      varsTerm (termP "f(g(X), Y)") `shouldMatchList` ["X", "Y"]
      varsTerm (termP "f(g(X), z(Y))") `shouldMatchList` ["X", "Y"]
      varsTerm (termP "f(g(a(X)), z(Y))") `shouldMatchList` ["X", "Y"]

    it "variables in rule" $ do
      varsRule (factP "f(a).") `shouldMatchList` []
      varsRule (factP "f(X).") `shouldMatchList` ["X"]
      varsRule (ruleP "f(a, X) :- good(X).") `shouldMatchList` ["X"]
      varsRule (ruleP "f(a, X, Y) :- g(Z).") `shouldMatchList` ["X", "Y", "Z"]
      varsRule (ruleP "f(g(a)) :- append(g(a)).") `shouldMatchList` []

    it "variables in substitution" $ do
      varsSubstitution ["a" =:= "a"] `shouldMatchList` []
      varsSubstitution ["X" =:= "a"] `shouldMatchList` ["X"]
      varsSubstitution ["X" =:= "Y"] `shouldMatchList` ["X", "Y"]
      varsSubstitution ["X" =:= "Y", "Z" =:= "f(X)"] `shouldMatchList` ["X", "Y", "Z"]
      varsSubstitution ["X" =:= "a", "Z" =:= "f(X)"] `shouldMatchList` ["X", "Z"]


