{-# LANGUAGE GADTs #-}

module Solver where

import Formula


-- Evaluating terms
-- ----------------

eval :: Term t -> t
eval _ = error "FIXME: implement eval"
eval (Name _) = error "eval: Name"    -- this constructor is not relevant for evaluation


-- Checking formulas
-- -----------------

satisfiable :: Formula ts -> Bool
satisfiable _ = error "FIXME: implement satisfiable"

solutions :: Formula ts -> [ts]
solutions _ = error "FIXME: implement solutions"