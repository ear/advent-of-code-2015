{-# LANGUAGE TypeApplications #-}

import Data.Ord
import Data.List
import Data.Function

main :: IO ()
main = do
  xs <- map (read @Int) . lines <$> readFile "input.txt"
  let cs = filter ((150 ==) . sum) . subsets $ xs
  print . length $ cs
  print . length
        . head
        . groupBy ((==) `on` length)
        . sortBy (comparing length)
        $ cs

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = let xss = subsets xs in xss ++ map (x:) xss
