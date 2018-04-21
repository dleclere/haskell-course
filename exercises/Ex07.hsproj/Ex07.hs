{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Ex07 where

data Nat = Z | S Nat

type family (+) (n :: Nat) (m :: Nat) :: Nat
type instance Z     + m = m
type instance (S n) + m = S (n + m)

type family (*) (n :: Nat) (m :: Nat) :: Nat
type instance Z * m = Z
type instance (S n) * m = m + (n * m)

data Vec (n :: Nat) a where
  VNil  :: Vec Z a
  VCons :: a -> Vec n a -> Vec (S n) a

deriving instance Show a => Show (Vec n a)
deriving instance Eq a   => Eq   (Vec n a)

-- IMPLEMENT headV
headV :: Vec (S n) a -> a
headV (VCons x xs) = x

appendV :: Vec n a -> Vec m a -> Vec (n + m) a
appendV VNil         ys = ys
appendV (VCons x xs) ys = VCons x (appendV xs ys)


-- IMPLEMENT concatV
concatV :: Vec n (Vec n2 b) -> Vec (n * n2) b
concatV VNil = VNil
concatV (VCons x xs) = appendV x (concatV xs)