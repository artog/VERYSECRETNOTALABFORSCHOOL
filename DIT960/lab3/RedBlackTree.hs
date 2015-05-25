{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------

module RedBlackTree (
  RBTree,        -- type of red-black search trees
  tree1,
  emptyTree,     -- RBTree a
  isEmpty,       -- RBTree a -> Bool
  leftSub,       -- RBTree a -> RBTree a
  rightSub,      -- RBTree a -> RBTree a
  rootVal,       -- RBTree a -> a
  isBlack,       -- RBTree a -> Bool
  get,           -- Ord a => a -> RBTree a -> Maybe a
  insert,        -- Ord a => a -> RBTree a -> RBTree a
  inorder,       -- RBTree a -> [a]
  remove,        -- Ord a => a -> RBTree a -> RBTree a
  size,          -- RBTree a -> Int
  maxheight,     -- RBTree a -> Int
  checkTree      -- Ord a => RBTree a -> Bool
 ) where

--------------------------------------------------------------------------------

import Control.Monad (guard)
import Data.Maybe (isJust)

-- red-black search trees
data Color = R | B deriving (Show, Eq, Read)
data RBTree a = Empty | Tree Color a (RBTree a) (RBTree a)
  deriving (Eq, Show, Read)

emptyTree :: RBTree a
emptyTree = undefined

isEmpty :: RBTree a -> Bool
isEmpty = undefined

leftSub :: RBTree a -> RBTree a
leftSub = undefined

rightSub :: RBTree a -> RBTree a
rightSub Empty = Empty
rightSub (Tree _ _ _ t2) = t2

rootVal :: RBTree a -> a
rootVal = undefined

isBlack :: RBTree a -> Bool
isBlack = undefined

get :: Ord a => a -> RBTree a -> Maybe a
get = undefined

insert :: Ord a => a -> RBTree a -> RBTree a
insert = undefined

inorder :: RBTree a -> [a]
inorder Empty = []
inorder (Tree _ a t1 t2) = (inorder t1) ++ [a] ++ (inorder t2) 

size :: RBTree a -> Int
size Empty = 0
size (Tree _ _ t1 t2) = 1 + size t1 + size t2

maxheight :: RBTree a -> Int
maxheight Empty = 0
maxheight (Tree _ _ t1 t2) = 1 + max (maxheight t1) (maxheight t2)

--------------------------------------------------------------------------------
-- Optional function

remove :: Ord a => a -> RBTree a -> RBTree a
remove = undefined

--------------------------------------------------------------------------------
-- Check that a red-black tree is ordered and obeys the red-black invariants

checkTree :: Ord a => RBTree a -> Bool
checkTree root =
  (isSorted (inorder root) &&
  checkRedParents root &&
  isJust (checkBlackHeight root) &&
  isBlack root)

-- True if the given list is ordered
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted (_:[]) = True
isSorted (a:b:l) = (a <= b) && (isSorted (b:l))

-- True if every red node only has black children
checkRedParents :: RBTree a -> Bool
checkRedParents Empty = True
checkRedParents (Tree c _ t1 t2) | c == B    = and [checkRedParents t1, checkRedParents t2]
checkRedParents (Tree _ _ t1 t2) | otherwise = and [valid, checkRedParents t1, checkRedParents t2]
  where 
    left  = isBlack t1
    right = isBlack t2
    valid = left && right

-- If the number of black nodes from each leaf to the root
-- are the same, then that number is returned,
-- otherwise returns Nothing
checkBlackHeight :: RBTree a -> Maybe Int
checkBlackHeight tree
  | isEmpty tree = do
      guard (isBlack tree)

      return 1
  | otherwise = do
      let left = leftSub tree
      let right = rightSub tree
      lh <- checkBlackHeight left
      rh <- checkBlackHeight right
      guard (lh == rh)
      if isBlack tree
      then
        return (lh + 1)
      else do
        guard (isBlack left)
        guard (isBlack right)
        return lh

--------------------------------------------------------------------------------

tree1 :: RBTree Int
tree1 = (Tree B 5 (Tree R 2 (Tree B 1 Empty Empty) Empty) (Tree R 6 Empty Empty))
tree2 :: RBTree Int
tree2 = (Tree R 5 (Tree B 2 (Tree R 3 Empty Empty) Empty) (Tree B 6 Empty Empty))
tree3 :: RBTree Int
tree3 = (Tree R 5 (Tree R 2 (Tree R 1 Empty Empty) Empty) (Tree B 6 Empty Empty))
