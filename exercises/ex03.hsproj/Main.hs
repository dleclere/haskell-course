import Ex03(
  prop_insert_on_empty_tree, 
  prop_insert_preserves_bst,
  prop_insert_adds_element,
  prop_insert_does_not_change_other_elements,
  prop_insert_duplicate_check,
  BinaryTree,
  BinaryTree(Leaf, Branch)
  )
import Test.QuickCheck

main :: IO ()
main = quickCheck $ prop_insert_on_empty_tree insert1
                .||. prop_insert_preserves_bst insert1
                .||. prop_insert_adds_element insert1
                .||. prop_insert_does_not_change_other_elements insert1
                .||. prop_insert_duplicate_check insert1
                .||. prop_insert_on_empty_tree insert2
                .||. prop_insert_preserves_bst insert2
                .||. prop_insert_adds_element insert2
                .||. prop_insert_does_not_change_other_elements insert2
                .||. prop_insert_duplicate_check insert2
                .||. prop_insert_on_empty_tree insert3
                .||. prop_insert_preserves_bst insert3
                .||. prop_insert_adds_element insert3
                .||. prop_insert_does_not_change_other_elements insert3
                .||. prop_insert_duplicate_check insert4
                .||. prop_insert_on_empty_tree insert4
                .||. prop_insert_preserves_bst insert4
                .||. prop_insert_adds_element insert4
                .||. prop_insert_does_not_change_other_elements insert4
                .||. prop_insert_duplicate_check insert4
                
                


insert1 :: Integer -> BinaryTree -> BinaryTree
insert1 i Leaf = Leaf
insert1 i (Branch v l r) 
  | i < v = Branch v (insert1 i l) r
  | otherwise =  Branch v l (insert1 i r)

insert2 :: Integer -> BinaryTree -> BinaryTree
insert2 i Leaf = Branch i Leaf Leaf
insert2 i (Branch v l r) 
  | i < v = Branch v (insert2 i l) r
  | i > v = Branch v l (insert2 i r)

insert3 :: Integer -> BinaryTree -> BinaryTree
insert3 i Leaf = Branch i Leaf Leaf
insert3 i (Branch v l r) 
  | i < v = Branch v (insert3 i l) r
  | i == v = Branch v l r
  | i > v = Branch v l (insert3 i r)

insert4 :: Integer -> BinaryTree -> BinaryTree
insert4 i Leaf = Branch i Leaf Leaf
insert4 i (Branch v l r) 
  | i < v = Branch v l (insert4 i r)
  | otherwise = Branch v (insert4 i l) r