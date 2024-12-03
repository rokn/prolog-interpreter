{-# LANGUAGE MonadComprehensions #-}
module Parser.PrologParserInternal where

import Parser.Parser
import Control.Applicative ((<|>))
import Data.List (intercalate)
import System.IO.Unsafe (unsafePerformIO)

data InternalId = EmptyList | List | Not | Is | Plus | Minus
  deriving (Eq)

data Identificator = UserDefined String | Internal InternalId
  deriving (Eq)
type Variable = String
data Constant = IdentC Identificator | NumbC Int
  deriving (Eq)

data Atom = Atom {
  aIdentificator :: Identificator,
  aTerms :: [Term]
} deriving (Eq)

data Term = TermC Constant | TermV Variable | TermA Atom
  deriving (Eq)

data Rule = Rule {
  rAtom  :: Atom,
  rGoals :: Goals
} deriving (Eq)

type Fact = Rule

type Program = [Rule]

type Goals = [Atom]

instance Show InternalId where
  show EmptyList = "[]"
  show List = "_list"
  show Not = "_not"
  show Is = "_is"
  show Plus = "+"
  show Minus = "-"

instance Show Identificator where
  show (UserDefined s) = s
  show (Internal i) = show i

instance Show Constant where
  show (IdentC i) = show i
  show (NumbC n) = show n

instance Show Term where
    show (TermC c) = show c
    show (TermV v) = v

    show (TermA (Atom (Internal id) terms))
      | id == List = show (items terms)
        where items [x, TermC _] = [show x]
              items [x, TermV v] = [show x, v]
              items [x, TermA (Atom _ rest)] = show x : items rest
    show (TermA a) = show a

instance Show Atom where
    show (Atom i ts) = show i ++ "(" ++ intercalate ", " (map show ts) ++ ")"

instance Show Rule where
    show (Rule a ts)
      | null ts = show a ++ "."
      | otherwise = show a ++ " :- " ++ intercalate ", " (map show ts) ++ "."


printProgram p = putStrLn ("\n" `intercalate` map show p)
printQuestion q = putStrLn ("?- " ++ ", " `intercalate` map show q)

oPar = symbol "("
cPar = symbol ")"

oBracket = symbol "["
cBracket = symbol "]"

identificator :: Parser Identificator
identificator = token [UserDefined (x : xs) | x <- lower, xs <- many (alphaNumeric <|> char '_')]
                 <|> pFail "Expected a valid identificator"

variable :: Parser Variable
variable = token [x : xs | x <- upper, xs <- many alphaNumeric]

constant :: Parser Constant
constant = IdentC <$> identificator  <|> NumbC <$> integer

atom :: Parser Atom
atom = [Atom ident terms | ident <- identificator,
                           terms <- bracket oPar (sepBy term (symbol ",")) cPar]

term :: Parser Term
term = TermV <$> variable  <|> TermA <$> atom  <|> TermC <$> constant  <|> specialTerm
      <|> pFail "Expected a valid term(constant | variable | atom)."

fact :: Parser Fact
fact = [Rule a [] | a <- atom, _ <- symbol "."]

goal :: Parser Atom
goal = specialGoal <|> atom

goals :: Parser Goals
goals = sepBy goal (symbol ",")

goals1 :: Parser Goals
goals1 = sepBy1 goal (symbol ",")

rule :: Parser Rule
rule = [Rule a gs | a <- atom, _ <- symbol ":-", gs <- goals1, _ <- symbol "."]

question :: Parser Goals
question = [gs | _ <- symbol "?-", gs <- goals, _ <- symbol "."]

comments :: Parser ()
comments = [() | _ <- many comment] <|> return ()
  where comment = symbol "%" >> (skipTo (symbol "\n") <|> skipTo eof)

program :: Parser Program
program = parse $ [prog | prog <- many1 (comments >> (fact <|> rule)), _ <- comments]
          <|> pFail "Expected a valid fact or rule definition."

parseProgram :: String -> ErrorMonad Program
parseProgram = eval program

parseQuestion :: String -> ErrorMonad Goals
parseQuestion = eval $ parse question

expression :: Parser Term
expression = toAtom <$> op operator side side <|> term <|> pFail "Expected a valid expression"
  where operator = (symbol "+" >> return Plus) <|> (symbol "-" >> return Minus)
        side     = var <|> numb
        toAtom (t1, id, t2) = TermA $ Atom (Internal id) [t1, t2]
        numb = TermC . NumbC <$> integer
        var  = TermV <$> variable

specialGoal :: Parser Atom
specialGoal = [Atom (Internal Not) (map TermA notGoals) | _ <- symbol "not", notGoals <- bracket oPar goals cPar] <|>
              [Atom (Internal Is)  [TermV var, expr]    | var <- variable, _ <- symbol "is", expr <- expression]

specialTerm :: Parser Term
specialTerm = [makeList terms | terms <- bracket oBracket (sepBy term (symbol ",")) cBracket] <|>
              [TermA $ Atom (Internal List) terms | terms <- bracket oBracket listParser cBracket]
  where makeList []     = TermC $ IdentC (Internal EmptyList)
        makeList (x:xs) = TermA $ Atom (Internal List) [x, makeList xs]
        listParser = [[head, TermV tail] | head <- term, _ <- symbol "|", tail <- variable]


-- Unsafe stuff should be private

fromRight :: (Show a) => Either a b -> b
fromRight (Right b) = b
fromRight (Left a)  = error $ "This shouldn't be left: " ++ show a

unsafeParse :: Parser a -> String -> a
unsafeParse parser = fromRight . eval parser

unsafeParseFile :: FilePath -> Program
unsafeParseFile file = unsafePerformIO $ do
  contents <- readFile file
  return (fromRight (parseProgram contents))

unsafeParseQuestion :: String -> Goals
unsafeParseQuestion q = fromRight (parseQuestion q)
