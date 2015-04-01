{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------

module RedBlackTree (
  RBTree,        -- type of red-black search trees
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
data RBTree a = ...
  deriving (Eq, Show, Read)

emptyTree :: RBTree a
emptyTree = undefined

isEmpty :: RBTree a -> Bool
isEmpty = undefined

leftSub :: RBTree a -> RBTree a
leftSub = undefined

rightSub :: RBTree a -> RBTree a
rightSub = undefined

rootVal :: RBTree a -> a
rootVal = undefined

isBlack :: RBTree a -> Bool
isBlack = undefined

get :: Ord a => a -> RBTree a -> Maybe a
get = undefined

insert :: Ord a => a -> RBTree a -> RBTree a
insert = undefined

inorder :: RBTree a -> [a]
inorder = undefined

size :: RBTree a -> Int
size = undefined

maxheight :: RBTree a -> Int
maxheight = undefined

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
isSorted = undefined

-- True if every red node only has black children
checkRedParents :: RBTree a -> Bool
checkRedParents = undefined

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
