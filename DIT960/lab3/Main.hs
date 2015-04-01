{-# OPTIONS -Wall #-}

{-
  to run:
  $ ghc -e main Main.hs < swahili-small.txt

  to compile and run:
  $ ghc -O3 --make Main.hs && ./Main < swahili-small.txt
-}

import RedBlackTree

--------------------------------------------------------------------------------

main :: IO ()
main = do
  contents <- getContents

  -- split the data into words and build a red-black tree
  -- use foldl
  undefined

  -- calculate and print statistics
  -- use fromIntegral/ceiling/logBase
  undefined

--------------------------------------------------------------------------------

