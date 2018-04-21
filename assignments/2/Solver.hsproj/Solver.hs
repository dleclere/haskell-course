{-# LANGUAGE GADTs #-}

module Solver where

import Formula

-- Evaluating terms
-- ----------------

eval :: Term t -> t
eval (Con a)   = a
eval (And a b) = (eval a) && (eval b)
eval (Or a b) = (eval a) || (eval b)
eval (Smaller a b) = (eval a) < (eval b)
eval (Plus a b) = (eval a) + (eval b)
eval (Name _) = error "eval: Name"    -- this constructor is not relevant for evaluation

-- Checking formulas
-- -----------------

satisfiable :: Formula ts -> Bool
satisfiable (Body a) = eval a
satisfiable (Forall [] p) = False
satisfiable (Forall (v:vs) p) = satisfiable (p (Con v)) || satisfiable (Forall (vs) p)

solutions :: Formula ts -> [ts]
solutions (Body a)
  | result == True = [()]
  | otherwise = []
  where result = eval a
solutions (Forall [] p) = []
solutions (Forall (v:vs) p) = 
  case (solutions (p (Con v))) of
    results@(r:rs) -> (map (\x -> (v, x)) results) ++ (solutions (Forall (vs) p))
    [] -> (solutions (Forall (vs) p))