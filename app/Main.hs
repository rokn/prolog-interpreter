{-# LANGUAGE MonadComprehensions #-}
module Main where

import Parser.PrologParser
import Interpreter.PrologInterpreterInternal
import System.Environment
import System.IO (stderr, hPutStrLn, stdout, hFlush)
import Data.List (intercalate)
import Parser.Parser (ErrorMonad)

startRepl :: Program -> IO ()
startRepl program = repl program Nothing

showAnswer :: Maybe Result -> IO ()
showAnswer Nothing = putStrLn "false."
showAnswer (Just (Result answer state)) = do
  putStr "true"
  putStrLn ";"
  print answer

handleAnswer :: Goals -> Program -> Maybe Result -> IO ()
handleAnswer goals program Nothing = do
  showAnswer Nothing
  repl program Nothing
handleAnswer goals program res@(Just (Result _ state)) = showAnswer res >> repl program (Just state)

handleGoals :: Goals -> Program -> Maybe Query -> IO ()
handleGoals [] program Nothing = repl program Nothing
handleGoals [] program (Just state) = handleAnswer (qInitialGoals state) program result
        where result = answerQuery state
handleGoals goals program _ = handleAnswer goals program result
        where result = answerQuery (createQuery program goals)


getRealQuery :: String -> String
getRealQuery query = "?-" ++ query ++ "."

repl :: Program -> Maybe Query -> IO ()
repl program state = do
  putStr "?- "
  hFlush stdout
  query <- getLine
  case parseQuestion (getRealQuery query) of
        Left errs -> do
          errPutStrLn "Wrong query!"
          errPutStrLn ("\n" `intercalate` errs)
          repl program Nothing
        Right goals -> handleGoals goals program state

errPutStrLn = hPutStrLn stderr

main :: IO ()
main = do
  args <- getArgs
  case safeHead args of
    Nothing -> errPutStrLn "Expected a file to be passed!"
    Just file -> do
      contents <- readFile file
      case parseProgram contents of
        Left errs -> do
          errPutStrLn "Failed interpreting!"
          errPutStrLn ("\n" `intercalate` errs)
        Right program -> startRepl program

