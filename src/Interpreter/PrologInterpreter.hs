module Interpreter.PrologInterpreter(
  Answer,
  Result(Result),
  Query,
  safeHead,
  answerQuery,
  createQuery,
  qInitialGoals,
) where

import Interpreter.PrologInterpreterInternal

