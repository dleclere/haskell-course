{-# LANGUAGE GADTs, EmptyDataDecls, TypeFamilies, TypeOperators, DataKinds, FlexibleInstances #-}

module Ex06 where

data List a = Nil | a ::: List a

data Format (fmt :: List *) where
  X :: Format Nil 
  S :: Format a -> Format (String ::: a)
  I :: Format a -> Format (Int ::: a)  
  L :: String -> Format a -> Format a
  
type family FormatArgsThen (fmt :: List *) (ty :: *) 
type instance FormatArgsThen Nil ty = ty
type instance FormatArgsThen (t ::: fmt) ty = t -> FormatArgsThen fmt ty

printf :: Format fmt -> FormatArgsThen fmt String
printf f = printf' f ""
  where
    printf' :: Format fmt -> String -> FormatArgsThen fmt String
    printf' X acc = acc
    printf' (S nx) acc = \x -> (printf' nx (acc ++ x))
    printf' (I nx) acc = \x -> (printf' nx (acc ++ (show x)))
    printf' (L str nx) acc = (printf' nx (acc ++ str))
