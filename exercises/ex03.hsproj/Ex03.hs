module Ex03 (
  BinaryTree(..), isBST, insertBST, searchTrees,
  prop_insert_on_empty_tree, prop_insert_preserves_bst, prop_insert_adds_element, prop_insert_does_not_change_other_elements,
  prop_insert_duplicate_check,
  prop_delete_detect_error, prop_delete_preserves_bst, prop_delete_removes_element, prop_delete_does_not_change_other_elements,
  height, size
) where

import Test.QuickCheck
import Data.List(sort, nub, intersect, delete)

data BinaryTree = Branch Integer BinaryTree BinaryTree
                | Leaf
                deriving (Show, Ord, Eq)

isBST :: BinaryTree -> Bool
isBST Leaf = True
isBST (Branch v l r) = allTree (< v) l && allTree (>= v) r && isBST l && isBST r
  where allTree :: (Integer -> Bool) -> BinaryTree -> Bool
        allTree f (Branch v l r) = f v && allTree f l && allTree f r
        allTree f (Leaf) = True       

insertBST :: Integer -> BinaryTree -> BinaryTree
insertBST i Leaf = Branch i Leaf Leaf
insertBST i (Branch v l r) 
  | i < v = Branch v (insertBST i l) r
  | otherwise = Branch v l (insertBST i r)   

searchTrees :: Gen BinaryTree
searchTrees = sized searchTrees'
  where 
   searchTrees' 0 = return Leaf
   searchTrees' n = do 
      v <- (arbitrary :: Gen Integer)
      fmap (insertBST v) (searchTrees' $ n - 1)

--------------
flatten :: BinaryTree -> [Integer]
flatten Leaf = []
flatten (Branch i t1 t2) = flatten(t1) ++ [i] ++ flatten(t2)

dSort :: [Integer] -> [Integer]
dSort list = nub $ sort list

prop_insert_on_empty_tree insertFunction integer = (insertFunction integer Leaf) == Branch integer Leaf Leaf

prop_insert_preserves_bst insertFunction integer = 
  forAll searchTrees $ \x -> isBST(insertFunction integer x)

hasElement :: Integer -> BinaryTree -> Bool
hasElement i Leaf = False
hasElement i (Branch v l r)
  | i == v = True
  | otherwise = hasElement i l || hasElement i r

prop_insert_adds_element insertFunction integer = 
  forAll searchTrees $ \x -> (not(hasElement integer x)) ==> (hasElement integer $ insertFunction integer x)
  
forAllEls :: (Integer -> Bool) -> BinaryTree -> Bool
forAllEls fn Leaf = False
forAllEls fn (Branch v l r) = (fn v) && (forAllEls fn l) && (forAllEls fn r)

a :: (Integer -> BinaryTree -> BinaryTree) -> Integer -> Integer -> BinaryTree -> Bool
a insertFunction integer newInteger x = (((sort (flatten (insertFunction integer x)))) `intersect` ((sort (flatten (insertFunction newInteger x)))) == sort (flatten x))

prop_insert_does_not_change_other_elements insertFunction integer newInteger = 
  integer /= newInteger ==>
    forAll searchTrees $ \x -> a insertFunction integer newInteger x

hasDuplicates :: BinaryTree -> Bool
hasDuplicates Leaf = False
hasDuplicates (Branch i l r) = (hasElement i l) || (hasElement i r) || (hasDuplicates l) || (hasDuplicates r)

countElement :: Integer -> BinaryTree -> Integer
countElement i Leaf = 0
countElement i (Branch v l r)
  | i == v = 1 + childCount
  | otherwise = childCount
  where childCount = (countElement i l) + (countElement i r)

hasntIncreasedDuplicates :: Integer -> (Integer -> BinaryTree -> BinaryTree) -> BinaryTree -> Bool
hasntIncreasedDuplicates i insertFn tree
  | iCount == 0 = pCount == 1
  | otherwise = pCount == iCount
  where iCount = countElement i tree
        pCount = (countElement i (insertFn i tree))

prop_insert_duplicate_check insertFunction integer =
  forAll searchTrees $ \x -> hasntIncreasedDuplicates integer (insertFunction) x
  
ainsert1 :: Integer -> BinaryTree -> BinaryTree
ainsert1 i Leaf = Leaf
ainsert1 i (Branch v l r) 
  | i < v = Branch v (ainsert1 i l) r
  | otherwise =  Branch v l (ainsert1 i r)

ainsert2 :: Integer -> BinaryTree -> BinaryTree
ainsert2 i Leaf = Branch i Leaf Leaf
ainsert2 i (Branch v l r) 
  | i < v = Branch v (ainsert2 i l) r
  | i > v = Branch v l (ainsert2 i r)

ainsert3 :: Integer -> BinaryTree -> BinaryTree
ainsert3 i Leaf = Branch i Leaf Leaf
ainsert3 i (Branch v l r) 
  | i < v = Branch v (ainsert3 i l) r
  | i == v = Branch v l r
  | i > v = Branch v l (ainsert3 i r)

ainsert4 :: Integer -> BinaryTree -> BinaryTree
ainsert4 i Leaf = Branch i Leaf Leaf
ainsert4 i (Branch v l r) 
  | i < v = Branch v l (ainsert4 i r)
  | otherwise = Branch v (ainsert4 i l) r

------------

prop_delete_detect_error deleteFunction i1 i2 = 
  i1 /= i2 ==>
    deleteFunction i1 (Branch i2 Leaf Leaf) == Branch i2 Leaf Leaf 

prop_delete_preserves_bst deleteFunction integer = 
  forAll searchTrees $ \x -> isBST(deleteFunction integer x)

deleteElementTest :: (Integer -> BinaryTree -> BinaryTree) -> Integer -> BinaryTree -> Bool
deleteElementTest del i tree
  | preDelete == 0 = postDelete == 0
  | otherwise = postDelete == preDelete - 1
  where preDelete = countElement i tree
        postDelete = countElement i $ del i tree
  
prop_delete_removes_element deleteFunction integer = 
  forAll searchTrees $ deleteElementTest deleteFunction integer
    
doe :: (Integer -> BinaryTree -> BinaryTree) -> Integer -> Integer -> BinaryTree -> Bool
doe fn i1 i2 x = (l1 `intersect` l2) == delete i1 (delete i2 (sort (flatten x)))
  where l1 = sort (flatten (fn i1 x))
        l2 = sort (flatten (fn i2 x))

prop_delete_does_not_change_other_elements deleteFunction integer newInteger =
  integer /= newInteger ==>
    forAll searchTrees $ \x -> doe deleteFunction integer newInteger x
    
incorrectDelete :: Integer -> BinaryTree -> BinaryTree
incorrectDelete i (Leaf) = Leaf
incorrectDelete i (Branch v l r) = Leaf
    
----------------

height :: BinaryTree -> Integer
height Leaf = 0
height (Branch v l r) = 1 + max (height l) (height r)
      
size :: BinaryTree -> Integer
size Leaf = 0
size (Branch v l r) = 1 + max (size l) (size r)

quickCheck' prop
  = output <$>
    quickCheckWithResult stdArgs{chatty=False} prop