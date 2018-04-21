
module MergeSort where
  
import Data.List
import Test.QuickCheck
  
merge :: Ord a => [a] -> [a] -> [a]
merge []      ys        = ys
merge xs      []        = xs
merge (x:xs)  (y:ys)
  | x <= y              = x:merge xs (y:ys)
  | otherwise           = y:merge (x:xs) ys


split :: [a] -> ([a], [a])
split [] = ([], [])
split [x] = ([x], [])
split (x:(y:xys)) = (x:s1, y:s2)
  where (s1, s2) = split xys

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort s1) (mergesort s2)
  where 
    (s1, s2) = split xs

-- tests

prop_preservesLength :: [Int] -> Bool
prop_preservesLength xs =
  length xs == length (mergesort xs)

prop_idempotent :: [Int] -> Bool
prop_idempotent xs =
  mergesort xs == mergesort (mergesort xs)
  
prop_ordered :: [Int] -> Bool
prop_ordered xs =
  isSorted (mergesort xs)
  
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xys) = (x <= y) && isSorted (y:xys)

prop_permutation :: [Int] -> Bool
prop_permutation xs =
  isPermutation (mergesort xs) xs
  where
     isPermutation xs ys = null ((xs \\ ys) ++ (ys \\ xs))

-- a \\ b -> subtract one list from another
-- null a -> check if list is empty 

-- helper

quickCheck' prop
  = output <$>
    quickCheckWithResult stdArgs{chatty=False} prop