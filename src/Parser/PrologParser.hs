module Parser.PrologParser
  (
    Identificator(..),
    Variable,
    Constant(..),
    Atom(..),
    Term(..),
    Rule(..),
    Fact,
    Program,
    Goals,
    parseProgram,
    parseQuestion,
    InternalId(..),
  ) where

import Parser.PrologParserInternal

