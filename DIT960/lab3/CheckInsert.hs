{-# OPTIONS -Wall #-}

{-
  to compile and run:
  $ ghc --make -O3 CheckInsert.hs -main-is CheckInsert.main && ./CheckInsert
-}

--------------------------------------------------------------------------------

module CheckInsert where

import Data.List ( permutations, foldl' )
import Control.Monad ( when )
import RedBlackTree

--------------------------------------------------------------------------------
-- check insertion

main :: IO ()
main = do
  when (not $ checkTree (emptyTree::RBTree Int)) $
    fail "Invariant not true for empty tree"
  mapM_ checkTreesOfSize [1..10]

checkTreesOfSize :: Int -> IO ()
checkTreesOfSize n = do
  putStrLn ("Testing trees of size " ++ show n)
  mapM_ checkTreeFromList (permutations [1..n])

checkTreeFromList :: [Int] -> IO ()
checkTreeFromList list
  | checkTree tree' = return ()
  | otherwise = do
    putStrLn ("\nInvariant not true after inserting\n  " ++ show (head list) ++
              "\ninto\n  " ++ show tree ++
              "\nresulting tree was\n  " ++ show tree')
    fail "bad tree"
  where tree = foldl' (flip insert) emptyTree (tail list)
        tree' = insert (head list) tree

--------------------------------------------------------------------------------
