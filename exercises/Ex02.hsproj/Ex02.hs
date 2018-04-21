module Ex02 (
  BinaryTree(..), isBST, insert, deleteAll, searchTrees, isSorted, sortedListsWithoutDuplicates, isBalanced,
  mysteryPred, mysterious, astonishing
) where

import Test.QuickCheck
import Data.List(sort, nub)

data BinaryTree = Branch Integer BinaryTree BinaryTree
                | Leaf
                deriving (Show, Ord, Eq)
                
data TreeState = Balanced | LeftHeavy | RightHeavy deriving (Eq, Show)

isBST :: BinaryTree -> Bool
isBST Leaf = True
isBST (Branch v l r) = allTree (< v) l && allTree (>= v) r && isBST l && isBST r
  where allTree :: (Integer -> Bool) -> BinaryTree -> Bool
        allTree f (Branch v l r) = f v && allTree f l && allTree f r
        allTree f (Leaf) = True
        
--Add an integer to a BinaryTree, preserving BST property.
insert :: Integer -> BinaryTree -> BinaryTree
insert i Leaf = Branch i Leaf Leaf
insert i (Branch v l r) 
  | i < v = Branch v (insert i l) r
  | otherwise = Branch v l (insert i r)

--Remove all instances of an integer in a binary tree, preserving BST property
deleteAll :: Integer -> BinaryTree -> BinaryTree
deleteAll i Leaf = Leaf
deleteAll i (Branch j Leaf r) | i == j = deleteAll i r
deleteAll i (Branch j l Leaf) | i == j = deleteAll i l
deleteAll i (Branch j l r) | i == j = let (x, l') = deleteRightmost l
                                       in Branch x l' (deleteAll i r)
                           | i <  j = Branch j (deleteAll i l) r
                           | i >  j = Branch j l (deleteAll i r)
  where deleteRightmost :: BinaryTree -> (Integer, BinaryTree)
        deleteRightmost (Branch i l Leaf) = (i, l)
        deleteRightmost (Branch i l r)    = let (x, r') = deleteRightmost r
                                             in (x, Branch i l r')

searchTrees :: Gen BinaryTree
searchTrees = sized searchTrees'
  where 
   searchTrees' 0 = return Leaf
   searchTrees' n = do 
      v <- (arbitrary :: Gen Integer)
      fmap (insert v) (searchTrees' $ n - 1)

----------------------

exists :: Integer -> BinaryTree -> Bool
exists i Leaf = False
exists i (Branch j t1 t2) | i == j = True
exists i (Branch j t1 t2) = (exists i t1) || (exists i t2)

mysteryPred :: Integer -> BinaryTree -> Bool
mysteryPred = exists

prop_mysteryPred_1 integer = forAll searchTrees $ \tree -> mysteryPred integer (insert integer tree)

prop_mysteryPred_2 integer = forAll searchTrees $ \tree -> not (mysteryPred integer (deleteAll integer tree))

----------------------

flatten :: BinaryTree -> [Integer]
flatten Leaf = []
flatten (Branch i t1 t2) = flatten(t1) ++ [i] ++ flatten(t2)

mysterious :: BinaryTree -> [Integer]
mysterious tree = flatten tree

prop_mysterious_1 integer = forAll searchTrees $ \tree -> mysteryPred integer tree == (integer `elem` mysterious tree)

prop_mysterious_2 = forAll searchTrees $ isSorted . mysterious

isSorted :: [Integer] -> Bool
isSorted (x:y:rest) = x <= y && isSorted (y:rest)
isSorted _ = True

----------------------

-- Note `nub` is a function that removes duplicates from a sorted list
sortedListsWithoutDuplicates :: Gen [Integer]
sortedListsWithoutDuplicates = fmap (nub . sort) arbitrary

--insertBalanced:: BinaryTree -> Integer -> BinaryTree
--insertBalanced Leaf x = Branch x Leaf Leaf
--insertBalanced tree@(Branch i Leaf right) x = Branch i ()
--
--buildTree :: BinaryTree -> [Integer] -> BinaryTree
--buildTree tree [] = tree
--buildTree Leaf (x1:xs) = buildTree(Branch x1 Leaf Leaf) xs
--buildTree tree@(Branch i Leaf right) (x1:xs) = buildTree(Branch i (Branch x1 Leaf Leaf) right) xs
--buildTree tree (x1:xs) = buildTree (insert x1 tree) xs

treeHeight :: BinaryTree -> Integer
treeHeight Leaf = 0
treeHeight (Branch v l r) = 1 + max (treeHeight l) (treeHeight r)

rotateLeft :: BinaryTree -> BinaryTree
rotateLeft Leaf = Leaf
rotateLeft tree@(Branch _ Leaf _) = tree
rotateLeft (Branch qi (Branch pi a b) c) = Branch pi a (Branch qi b c)

rotateRight :: BinaryTree -> BinaryTree
rotateRight Leaf = Leaf
rotateRight tree@(Branch _ _ Leaf) = tree
rotateRight (Branch pi a (Branch qi b c)) = Branch qi (Branch pi a b) c

binsert :: Integer -> BinaryTree -> BinaryTree
binsert i Leaf = Branch i Leaf Leaf
binsert i (Branch v l r) 
  | i < v = balanceBranch $ Branch v (binsert i l) r
  | otherwise = balanceBranch $ Branch v l (binsert i r)

buildTree2 :: [Integer] -> BinaryTree
buildTree2 = foldr binsert Leaf

treeState :: BinaryTree -> TreeState
treeState Leaf = Balanced
treeState (Branch _ l r)
  | abs balanceFactor <= 1 = Balanced
  | leftHeight < rightHeight = RightHeavy
  | otherwise = LeftHeavy
  where leftHeight = treeHeight l
        rightHeight = treeHeight r
        balanceFactor = leftHeight - rightHeight

balanceBranch :: BinaryTree -> BinaryTree
balanceBranch Leaf = Leaf
balanceBranch tree@(Branch _ l r) = balancedBranch
  where 
    state = treeState tree
    leftState = treeState l
    rightState = treeState r
    balancedBranch
      | state == Balanced = tree
      | state == LeftHeavy && rightState == LeftHeavy = rotateLeft(rotateLeft(tree))
      | state == LeftHeavy = rotateLeft tree
      | state == RightHeavy && leftState == RightHeavy = rotateRight(rotateRight(tree))
      | state == RightHeavy = rotateRight tree

astonishing :: [Integer] -> BinaryTree
astonishing [] = Leaf
astonishing xs = (buildTree2 xs)

isBalanced :: BinaryTree -> Bool
isBalanced Leaf = True
isBalanced (Branch v l r) = abs (height l - height r) <= 1 && isBalanced l && isBalanced r
  where height Leaf = 0
        height (Branch v l r) = 1 + max (height l) (height r)
      

prop_astonishing_1 = forAll sortedListsWithoutDuplicates $ isBST . astonishing

prop_astonishing_2 = forAll sortedListsWithoutDuplicates $ isBalanced . astonishing

prop_astonishing_3 = forAll sortedListsWithoutDuplicates $ \ integers -> mysterious (astonishing integers) == integers
  
prop_sortedListsWithoutDuplicates_1 = forAll sortedListsWithoutDuplicates isSorted 
prop_sortedListsWithoutDuplicates_2 = forAll sortedListsWithoutDuplicates $ \x -> x == nub x


quickCheck' prop
  = output <$>
    quickCheckWithResult stdArgs{chatty=False} prop