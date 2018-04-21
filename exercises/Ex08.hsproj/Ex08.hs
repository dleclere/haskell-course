module Ex08 (BinaryTree(..), treeMap, parTreeMap, parTreeMapWithCutoff) where

import System.Random 
import Control.Applicative
import Control.Parallel
import System.Environment

data BinaryTree a = Leaf | Branch a (BinaryTree a) (BinaryTree a) deriving (Read, Show, Eq)

treeMap :: (a -> b) -> BinaryTree a -> BinaryTree btreeMap f Leaf = Leaf
treeMap f (Branch v l r) = let v' = f v
                               l' = treeMap f l 
                               r' = treeMap f r
                           in
                           v' `pseq` l' `pseq` r' `pseq` Branch v' l' r'

parTreeMap :: (a -> b) -> BinaryTree a -> BinaryTree b
parTreeMap f Leaf = Leaf
parTreeMap f (Branch v l r) = let v' = f v
                                  l' = parTreeMap f l 
                                  r' = parTreeMap f r
                           in
                           v' `par` (l' `par` (r' `pseq` Branch v' l' r'))
                           

parTreeMapWithCutoff :: Int -> (a -> b) -> BinaryTree a -> BinaryTree b
parTreeMapWithCutoff n f Leaf = Leaf
parTreeMapWithCutoff n f (Branch v l r) = let v' = f v
                                              l' = parTreeMapWithCutoff (n - 1) f l 
                                              r' = parTreeMapWithCutoff (n - 1) f r
                           in
                             if (n > 0)
                                then v' `par` (l' `par` (r' `pseq` Branch v' l' r'))
                                else   
                                  v' `pseq` l' `pseq` r' `pseq` Branch v' l' r'