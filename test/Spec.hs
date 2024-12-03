import Test.Hspec


import ParserTests
import InterpreterTests
import QueryTests


main :: IO ()
main =
  hspec $ do
    parserTests
    interpreterTests
    queryTests

