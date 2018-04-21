{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

data Nat = Z | S Nat
  
data SNat (n :: Nat) where  -- natural numbers as singleton type
  Zero :: SNat Z
  Succ :: SNat n -> SNat (S n)

data Vec (n :: Nat) a where
  VNil  :: Vec Z a
  VCons :: a -> Vec n a -> Vec (S n) a
  
type family (*) (n :: Nat) (m :: Nat) :: Nat
type instance Z * m = Z
type instance (S n) * m = m + (n * m)

type family (+) (n :: Nat) (m :: Nat) :: Nat
type instance Z     + m = m
type instance (S n) + m = S (n + m)

deriving instance Show a => Show (Vec n a)
deriving instance Eq a   => Eq   (Vec n a)

append3 :: (Vec n1 a) -> (Vec n2 a) -> (Vec (n1 + n2) a)
append3 (VCons x VNil) ys = VCons x ys
append3 (VCons x xs) ys = VCons x $ append3 xs ys

concat3 :: Vec n1 (Vec n2 a) -> Vec (n1 * n2) a
concat3 VNil = VNil
concat3 (VCons x xs) = append3 x (concat3 xs)

replicateV :: SNat n -> a -> Vec n a
replicateV Zero a = VNil
replicateV (Succ n) a = VCons a $ replicateV n a
