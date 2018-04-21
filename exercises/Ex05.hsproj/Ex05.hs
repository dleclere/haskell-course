{-# LANGUAGE GADTs, EmptyDataDecls, TypeFamilies, TypeOperators, DataKinds, FlexibleInstances #-}

module Ex05 where
  
data Tag = Empty | NonEmpty

-- inaryTree to be indexed by one of these two data types
-- BinaryTree Empty Integer should refer to an empty tree of integers, whereas BinaryTree NonEmpty Integer should refer to a tree containing one or more integers.


data BinaryTree i a where
  Leaf   :: BinaryTree Empty a
  Branch :: a -> BinaryTree i a -> BinaryTree i a -> BinaryTree NonEmpty a
  
top :: BinaryTree NonEmpty a -> a
top (Branch v l r) = v

----

data BoolProp (a :: Bool) where
  PTrue  :: BoolProp True
  PFalse :: BoolProp False
  PAnd   :: BoolProp a -> BoolProp b -> BoolProp (a && b)
  POr    :: BoolProp a -> BoolProp b -> BoolProp (a || b)
  PNot   :: BoolProp a -> BoolProp (Not a) 

type family Not (a :: Bool) :: Bool
type instance Not True  = False
type instance Not False = True

type family (&&) (a :: Bool) (b :: Bool) :: Bool
type instance (&&) True True = True
type instance (&&) False True = False
type instance (&&) True False = False
type instance (&&) False False = False

type family (||) (a :: Bool) (b :: Bool) :: Bool
type instance (||) True True = True
type instance (||) False True = True
type instance (||) True False = True
type instance (||) False False = False

class ToRuntimeBool a where
  eval :: a -> Bool

instance ToRuntimeBool (BoolProp True) where
  eval _ = True

instance ToRuntimeBool (BoolProp False) where
  eval _ = False