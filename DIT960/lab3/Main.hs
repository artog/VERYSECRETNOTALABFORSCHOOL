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
    let tree = foldl (\t x -> insert x t) emptyTree $ words contents
    --let tree    = tree1
        n       = size tree
        h       = fromIntegral $ maxheight tree
        h'      = (logBase 2.0 ((fromIntegral n)+1)) - 1.0 :: Double
        ratio   = h / h'
        valid   = checkTree tree
        first20 = unwords $ take 20 $ inorder tree
    -- calculate and print statistics
    -- use fromIntegral/ceiling/logBase
    putStr $ "Size: " ++ (show n) ++ "\n"
    putStr $ "Height: " ++ (show h) ++ "\n"
    putStr $ "Optimal height: " ++ (show h') ++ "\n"
    putStr $ "Height / Optimal height: " ++ (show ratio) ++ "\n"
    putStr $ "checkTree: " ++ (show valid) ++ "\n"
    putStr $ "First 20 words: " ++ first20 ++ "\n"

--------------------------------------------------------------------------------
