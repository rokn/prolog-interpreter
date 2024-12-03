module ParserTests where

import Parser.PrologParser
import Parser.PrologParserInternal
import Parser.Parser
import TestUtils
import Test.Hspec
import Test.QuickCheck

parserTests = do
--identificator :: Parser Identificator
  describe "identificatorParsing" $ do
      let identParse = eval $ parse identificator
      let identStartParse = eval identificator
      it "Passing on correct identifiers" $ do
        identParse "identificator" `shouldSucceedWith` UserDefined "identificator"
        identParse "vErYSTRangeIdentificator" `shouldSucceedWith` UserDefined "vErYSTRangeIdentificator"
        identParse "identW1thnumber5" `shouldSucceedWith` UserDefined "identW1thnumber5"
        identStartParse "someId(lala)" `shouldSucceedWith` UserDefined "someId"
        identStartParse "thisIsReal, thisIsNot" `shouldSucceedWith` UserDefined "thisIsReal"

      it "failing on wrong identifiers" $ do
        shouldFail $ identParse "37shouldntpass"
        shouldFail $ identParse "other$symbols"
        shouldFail $ identParse "evenA(Brace)"
        shouldFail $ identParse "shouldntpass(atom)"

--variable :: Parser Variable
  describe "variableParsing" $ do
      let varParse = eval $ parse variable
      let varStartParse = eval variable
      it "Passing on correct variables" $ do
        varParse "AllYourVarsAreBelongToUs" `shouldSucceedWith` "AllYourVarsAreBelongToUs"
        varParse "This1Var1Has1Numbers1" `shouldSucceedWith` "This1Var1Has1Numbers1"
        varStartParse "ThisIsSparta, messenger" `shouldSucceedWith` "ThisIsSparta"

      it "failing on wrong variables" $ do
        shouldFail $ varParse "37NoNoNo"
        shouldFail $ varParse "VatI$Thi$"
        shouldFail $ varParse "CanI,DoThis"

--constant :: Parser Constant
  describe "constantParsing" $ do
  -- This is the same as identifiers but for completeness they are added
      let constParse = eval $ parse constant
          constStartParse = eval constant
          makeConst = IdentC . UserDefined

      it "Passing on correct constants" $ do
        constParse "identificator" `shouldSucceedWith` makeConst "identificator"
        constParse "vErYSTRangeIdentificator" `shouldSucceedWith` makeConst "vErYSTRangeIdentificator"
        constParse "identW1thnumber5" `shouldSucceedWith` makeConst "identW1thnumber5"
        constStartParse "someId(lala)" `shouldSucceedWith` makeConst "someId"
        constStartParse "thisIsReal, thisIsNot" `shouldSucceedWith` makeConst "thisIsReal"

      it "failing on wrong constants" $ do
        shouldFail $ constParse "37shouldntpass"
        shouldFail $ constParse "other$symbols"
        shouldFail $ constParse "evenA(Brace)"
        shouldFail $ constParse "shouldntpass(atom)"

--atom :: Parser Atom
  describe "atomParsing" $ do
      let atomParse = eval $ parse atom
          makeConst = IdentC . UserDefined
      it "Passing on correct atoms" $ do
        atomParse "identificator()" `shouldSucceedWith` Atom (UserDefined "identificator") []
        atomParse "atomata(  Nice)" `shouldSucceedWith` Atom (UserDefined "atomata") [TermV "Nice"]
        atomParse "atomata(constant  )" `shouldSucceedWith` Atom (UserDefined "atomata") [TermC (makeConst "constant")]
        atomParse "atomata(inner( ))" `shouldSucceedWith` Atom (UserDefined "atomata") [TermA $ Atom (UserDefined "inner") []]
        atomParse "multiples(X, y, Z)" `shouldSucceedWith` Atom (UserDefined "multiples") [TermV "X", TermC (makeConst "y"), TermV "Z"]
        atomParse "multiplesInner(X  ,nice()  )" `shouldSucceedWith` Atom (UserDefined "multiplesInner") [TermV "X", TermA (Atom (UserDefined "nice") [])]

      it "failing on wrong atoms" $ do
        shouldFail $ atomParse "this(is, legal, not"
        shouldFail $ atomParse "isIt   X, Y"
        shouldFail $ atomParse "evenA$BraceMiss)"
        shouldFail $ atomParse "shouldntpass)("
        shouldFail $ atomParse "almostThere(X, good($))"

--term :: Parser Term
  describe "termParsing" $ do
  -- Every possible part of terms is now tested so just one quick test for every one
      let termParse = eval $ parse term
          makeConst = IdentC . UserDefined
      it "Passing on correct terms" $ do
        termParse "identificator(  )" `shouldSucceedWith` TermA (Atom (UserDefined "identificator") [])
        termParse "GoodVar" `shouldSucceedWith` TermV "GoodVar"
        termParse "pi" `shouldSucceedWith` TermC (makeConst "pi")

      it "failing on wrong terms" $ do
        shouldFail $ termParse "WhatKindOfTerm, IsThis"
        shouldFail $ termParse "ThisIsIllegal$()"

--fact :: Parser Fact
  describe "factParsing" $ do
  -- facts are just atoms with a dot -> small tests
      let factParse = eval $ parse fact
      it "Passing on correct facts" $ do
        factParse "identificator()." `shouldSucceedWith` Rule (Atom (UserDefined "identificator") []) []
        shouldSucceed $ factParse "multi(X ,   y)."
        shouldSucceed $ factParse "good(nice(X))."

      it "failing on wrong facts" $ do
        shouldFail $ factParse "noDotBaby(X)"
        shouldFail $ factParse "youCanTDoThis."

--goals :: Parser Goals
  describe "goalsParsing" $ do
  -- goals are just atoms with a commas -> small tests
      let goalsParse = eval $ parse goals
      it "Passing on correct goals" $ do
        shouldSucceed $ goalsParse "identificator(), second(Y, z)"
        shouldSucceed $ goalsParse "multi(X, y),    goal1(ZZZ)"
        shouldSucceed $ goalsParse "evenOneShouldWork(hooli)"

      it "failing on wrong goals" $ do
        shouldFail $ goalsParse "noSecond(X), "
        shouldFail $ goalsParse "nice(), butNotSo(), bolls)"

--rule :: Parser Rule
  describe "ruleParsing" $ do
      let ruleParse = eval $ parse rule
      it "Passing on correct rule" $ do
        ruleParse "identificator(X) :- goal(X)." `shouldSucceedWith` Rule (Atom (UserDefined "identificator") [TermV "X"]) [Atom (UserDefined "goal") [TermV "X"]]
        shouldSucceed $ ruleParse "multi(X, y):-   goal1(ZZZ), goal2(X), goal3(ZZZ, y)."
        shouldSucceed $ ruleParse "evenOneShouldWork(X):-g(X)."

      it "failing on wrong rule" $ do
        shouldFail $ ruleParse "nodot(X) :- here(X)"
        shouldFail $ ruleParse "no() - colon(), mark()."
        shouldFail $ ruleParse "no() : dash()."
        shouldFail $ ruleParse "spacey() :   -  dash()."

--question :: Parser Goals
  describe "questionParsing" $ do
      let questionParse = eval $ parse question
      it "Passing on correct question" $ do
        shouldSucceed $ questionParse "?- identificator(), second(Y, z)."
        shouldSucceed $ questionParse "?- multi(X, y), goal1(ZZZ)."
        shouldSucceed $ questionParse "?- evenOneShouldWork(hooli)."

      it "failing on wrong question" $ do
        shouldFail $ questionParse "?- nodot(X)"
        shouldFail $ questionParse "no(), question(), mark()."
        shouldFail $ questionParse "? no(), dash()."
        shouldFail $ questionParse "? - spacey(), dash()."

--program :: Parser Program
  describe "programParsing" $ do
      let programParse = eval program
      it "Passing on correct program" $ do
        shouldSucceed $ programParse "identificator().   \n   second(Y, z)."
        shouldSucceed $ programParse "multi(X, y).rule(Z):-         this(Z), is(good)."
        shouldSucceed $ programParse "multiline(X, y).\nrule(Z):- this(Z), is(good)."
        shouldSucceed $ programParse "evenOneShouldWork(hooli)."

      it "failing on wrong program" $ do
        shouldFail $ programParse ""
        shouldFail $ programParse "no(). questions(). ?- asked()."
        shouldFail $ programParse "no(), commas()."
        shouldFail $ programParse "spacey(), dash()."
