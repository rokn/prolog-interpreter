module Interpreter.PrologInterpreterInternal where

import Control.Monad.Extra
import Parser.PrologParser(Program, Term(..), Variable, Goals,
                    Identificator(..), Constant(..), Atom(..), Rule(..), InternalId(..))
import Control.Applicative ((<|>))

import Debug.Trace (trace)
import Data.List (union, foldl', intersect, intercalate, nub)
import Data.Maybe (fromMaybe)

data Substitution = Term := Term
  deriving (Show, Eq)

type Substitutions = [Substitution]

data BranchPoint = BranchPoint
  {
    bpSubstitutions :: Substitutions,
    bpRulesLeft    :: [Rule],
    bpGoals        :: Goals
  } deriving (Show, Eq)

data Query = Query
  {
    qInitialGoals :: Goals,
    qProgram      :: Program,
    qBranches     :: [BranchPoint]
  } deriving (Eq)

instance Show Query where
    show query = initialGoals ++ "\n" ++ branches
      where initialGoals = "Query\nInitial goals:" ++ show (qInitialGoals query)
            branches = "\n" `intercalate` [ "Branch:\n\tNext rule: " ++ show (safeHead $ bpRulesLeft branch)
                                             ++ "\n\tGoals: " ++ show (bpGoals branch)
                                             ++ "\n\tSubstitution: " ++ show (bpSubstitutions branch)
                                             | branch <- qBranches query]

type Answer = Substitutions

data Result = Result Answer Query
                        deriving (Show, Eq)


-- Public
createQuery :: Program -> Goals -> Query
createQuery program goals = Query { qInitialGoals = goals, qProgram = program, qBranches = [initialBranch] }
  where initialBranch = BranchPoint { bpSubstitutions = [], bpRulesLeft = program, bpGoals = goals }

answerQuery :: Query -> Maybe Result
answerQuery (Query _ _ []) = Nothing
answerQuery query
  | hasNoGoalsLeft query = Just (Result (unifyGoalVars query) (removeCurrentBranch query))

answerQuery query = tryWithCurrentRule <|>
                    if hasNoRulesLeft query
                    then answerQuery (removeCurrentBranch query)
                    else answerQuery (removeCurrentRule query)

  where tryWithCurrentRule = do
          let branch = currentBranch query

          goal <- getCurrentGoal branch

          if isInternalGoal goal then handleInternalGoal query goal
          else do

            rule <- getCurrentRule branch

            let usedVariables = varsSubstitution (getQuerySubstitutions query) `union` varsAtom goal
                fixedRule = fixVariables usedVariables rule
            unifier <- unify [TermA goal := TermA (rAtom fixedRule)]

            let newGoals = map (substituteInAtom unifier) (rGoals fixedRule)
            let oldGoals = map (substituteInAtom unifier) (tail $ bpGoals branch)
            let newBranch = BranchPoint unifier (qProgram query) (newGoals ++ oldGoals)

            let newQuery = (addNewBranch newBranch . removeCurrentRule) query

            answerQuery newQuery


safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x


-- Private
currentBranch :: Query -> BranchPoint
currentBranch = head . qBranches

removeCurrentBranch :: Query -> Query
removeCurrentBranch (Query igs prog [])     = Query igs prog []
removeCurrentBranch (Query igs prog (b:bs)) = Query igs prog bs

addNewBranch :: BranchPoint -> Query -> Query
addNewBranch bp (Query igs prog bps) = Query igs prog (bp:bps)

addNewSubstitution :: Substitution -> Query -> Query
addNewSubstitution _ (Query igs prog []) = Query igs prog []
addNewSubstitution s (Query igs prog (b:bps)) = Query igs prog (addSubstitution s b:bps)

addSubstitution :: Substitution -> BranchPoint -> BranchPoint
addSubstitution s (BranchPoint subs rules goals) = BranchPoint (s : subs) rules goals

removeCurrentGoal :: Query -> Query
removeCurrentGoal (Query igs prog (b:bs)) = Query igs prog (removeCurrentGoalFromBranch b : bs)

removeCurrentRule :: Query -> Query
removeCurrentRule (Query igs prog (b:bs)) = Query igs prog (removeCurrentRuleFromBranch b : bs)

removeCurrentRuleFromBranch :: BranchPoint -> BranchPoint
removeCurrentRuleFromBranch (BranchPoint subs [] goals) = BranchPoint subs [] goals
removeCurrentRuleFromBranch (BranchPoint subs (_:rules) goals) = BranchPoint subs rules goals

removeCurrentGoalFromBranch :: BranchPoint -> BranchPoint
removeCurrentGoalFromBranch (BranchPoint subs rules []) = BranchPoint subs rules []
removeCurrentGoalFromBranch (BranchPoint subs rules (_:goals)) = BranchPoint subs rules goals

getQuerySubstitutions :: Query -> Substitutions
getQuerySubstitutions query = concatMap bpSubstitutions (qBranches query)

mapCurrentGoals :: (Atom -> Atom) -> Query -> Query
mapCurrentGoals f q = do
  let (BranchPoint subs rules goals) = currentBranch q
  let newBranch = BranchPoint subs rules (map f goals)
  (addNewBranch newBranch . removeCurrentBranch) q


hasNoGoalsLeft :: Query -> Bool
hasNoGoalsLeft (Query _ _ (b:_)) = null (bpGoals b)

hasNoRulesLeft :: Query -> Bool
hasNoRulesLeft (Query _ _ (b:_)) = null (bpRulesLeft b)

getCurrentRule :: BranchPoint -> Maybe Rule
getCurrentRule branch = safeHead $ bpRulesLeft branch

getCurrentGoal :: BranchPoint -> Maybe Atom
getCurrentGoal branch = safeHead $ bpGoals branch

left :: Substitution -> Term
left (t := _) = t

right :: Substitution -> Term
right (_ := t) = t


unify :: Substitutions -> Maybe Substitutions
unify eqs = unifyAll eqs []


-- head(good, X) fails!!
unifyAll :: Substitutions -> Substitutions -> Maybe Substitutions
unifyAll [] state = Just state
unifyAll ((t1 := t2) : eqs) state
  | t1 == t2 = unifyAll eqs state

unifyAll ((TermC c1 := TermC c2) : eqs) state
  | c1 == c2 = unifyAll eqs state
  | c1 /= c2 = Nothing

unifyAll ((TermC _ := TermA _) : _) _ = Nothing
unifyAll ((TermA _ := TermC _) : _) _ = Nothing

unifyAll ((TermA (Atom i1 ts1) := TermA (Atom i2 ts2)):eqs) state
  | i1 /= i2 = Nothing
  | length ts1 /= length ts2 = Nothing
  | i1 == i2 = unifyAll (zipWith (:=) ts1 ts2 ++ eqs) state

unifyAll ((TermC c := TermV v) : eqs) state = unifyAll ((TermV v := TermC c):eqs) state
unifyAll ((TermA a := TermV v) : eqs) state = unifyAll ((TermV v := TermA a):eqs) state

unifyAll (eq@(TermV v := term):eqs) state
  | inTermVars v term = Nothing
  | otherwise = unifyAll (subst eqs) (eq : subst state)
  where subst = map (substituteInEquality [eq])


substituteInTerm :: Substitutions -> Term -> Term
substituteInTerm [] t = t
substituteInTerm ((TermV v := t) : eqs) (TermV v2)
  | v == v2 = t

substituteInTerm subs (TermA (Atom i ts)) = TermA (Atom i substituted)
  where substituted = map (substituteInTerm subs) ts

substituteInTerm (e:eqs) t = substituteInTerm eqs t


substituteInAtom :: Substitutions -> Atom -> Atom
substituteInAtom subs (Atom i ts) = Atom i (map (substituteInTerm subs) ts)

substituteInEquality :: Substitutions -> Substitution -> Substitution
substituteInEquality subs (t1 := t2) = substitute t1 := substitute t2
  where substitute = substituteInTerm subs

substituteInRule :: Substitutions -> Rule -> Rule
substituteInRule subs (Rule a gs) = Rule (substitute a) (map substitute gs)
  where substitute = substituteInAtom subs


inTermVars :: Variable -> Term -> Bool
inTermVars x t = x `elem` varsTerm t

varsTerm :: Term -> [Variable]
varsTerm (TermC _) = []
varsTerm (TermV x) = [x]
varsTerm (TermA (Atom _ ts)) = nub $ concatMap varsTerm ts

varsAtom :: Atom -> [Variable]
varsAtom = varsTerm . TermA

varsRule :: Rule -> [Variable]
varsRule (Rule a as) = varsAtom a `union` concatMap varsAtom as

varsSubstitution :: Substitutions -> [Variable]
varsSubstitution [] = []
varsSubstitution ((t := t2):ss) = foldl' union [] [varsTerm t, varsTerm t2, varsSubstitution ss]

fixVariables :: [Variable] -> Rule -> Rule
fixVariables forbidden r = substituteInRule subst r
  where freeVars   = map TermV (filter (not . (`elem` forbidden)) (map (('_' :) . show) [1 ..]))
        commonVars = map TermV $ forbidden `intersect` varsRule r
        subst      = zipWith (:=) commonVars freeVars

unifyGoalVars :: Query -> Answer
unifyGoalVars query  = filter hasVarInGoals (fromMaybe [] unifiedState)
  where unifiedState = unify $ getQuerySubstitutions query
        hasVarInGoals (t := _) = any (`elem` goalVars) (varsTerm t)
        goalVars = concatMap varsAtom (qInitialGoals query)

isInternalGoal :: Atom -> Bool
isInternalGoal (Atom (Internal _) _) = True
isInternalGoal _                     = False

handleInternalGoal :: Query -> Atom -> Maybe Result
handleInternalGoal query g@(Atom (Internal Not) _) = handleNegationGoal query g
handleInternalGoal query g@(Atom (Internal Is) _) = handleUnifyGoal query g

calculateExpression :: Term -> Term
calculateExpression t@(TermC (NumbC n)) = t
calculateExpression t@(TermA (Atom (Internal id) terms)) = calculateOp id terms
  where calculateOp Plus  [TermC (NumbC x), TermC (NumbC y)] = TermC (NumbC (x+y))
        calculateOp Minus [TermC (NumbC x), TermC (NumbC y)] = TermC (NumbC (x-y))
        calculateOp _ _ = t
calculateExpression t = t

handleUnifyGoal :: Query -> Atom -> Maybe Result
handleUnifyGoal query (Atom _ [x, expr])
  | null (varsTerm realExpr) = answerQuery newQuery
  | otherwise = Nothing
    where realExpr = substituteInTerm subst expr
          subst = getQuerySubstitutions query
          newSubstitution = x := calculateExpression realExpr
          currentGoalChanger = mapCurrentGoals (substituteInAtom [newSubstitution])
          newQuery = (currentGoalChanger . addNewSubstitution newSubstitution . removeCurrentGoal) query

handleUnifyGoal _ _ = error "Unexpected `is` terms!"

handleNegationGoal :: Query -> Atom -> Maybe Result
handleNegationGoal query (Atom _ terms) =
  case answerQuery (createQuery (qProgram query) goals) of
    Just _ -> Nothing
    Nothing -> answerQuery (removeCurrentGoal query)
  where
    goals = fromTerms terms
    fromTerms [] = []
    fromTerms (TermA atom:ts) = atom : fromTerms ts


tracePrint :: Show a => a -> Maybe ()
tracePrint info = trace (show info) Just ()
