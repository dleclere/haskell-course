{-# LANGUAGE GADTs #-}

module Formula where

-- Datatype of formulas
-- --------------------

data Formula ts where
  Body   :: Term Bool                     -> Formula ()
  Forall :: Show a 
         => [a] -> (Term a -> Formula as) -> Formula (a, as)

data Val = IntV  Int
         | BoolV Bool
         deriving Show

-- UNTYPED VERSION
-- data Term = Con Val
--           | And Term Term
--           | Or Term Term
--           | Smaller Term Term
--           | Plus Term Term

-- data Val = IntV  Int
--          | BoolV Bool
--          deriving Show

data Term t where
  Con     :: Bool -> Term t
  
  Name    :: String -> Term t    -- to facilitate pretty printing


-- Pretty printing formulas
-- ------------------------

instance Show t => Show (Term t) where
  show (Con v)       = show v
--  show (And p q)     = "(" ++ show p ++ " && " ++ show q ++ ")"
--  show (Or p q)      = "(" ++ show p ++ " || " ++ show q ++ ")"
--  show (Smaller n m) = "(" ++ show n ++ " < "  ++ show m ++ ")"
--  show (Plus n m)    = "(" ++ show n ++ " + "  ++ show m ++ ")"
  show (Name name)   = name

instance Show (Formula ts) where
  show = show' ['x' : show i | i <- [0..]]
    where
      show' :: [String] -> Formula ts' -> String
      show' ns     (Body body)   = show body
      show' (n:ns) (Forall vs p) = "forall " ++ n ++ "::" ++ show vs ++ ". " ++ show' ns (p (Name n))

--
---- Example formulas
---- ----------------
--
--ex1 :: Formula ()
--ex1 = Body (Con True)
--
--ex2 :: Formula (Int, ())
--ex2 = Forall [1..10] $ \n ->
--        Body $ n `Smaller` (n `Plus` Con 1)
--
--ex3 :: Formula (Bool, (Int, ()))
--ex3 = Forall [False, True] $ \p -> 
--      Forall [0..2] $ \n -> 
--        Body $ p `Or` (Con 0 `Smaller` n)
--        
