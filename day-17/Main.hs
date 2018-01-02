{-# LANGUAGE TypeApplications #-}

import Data.List

main = do
  xs <- map (read @Int) . lines <$> readFile "input.txt"
  print . length . filter ((150 ==) . sum) . subsets $ xs

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = let xss = subsets xs in xss ++ map (x:) xss

select :: [a] -> [(a,[a])]
select xs = let
  n = length xs
  in map (\i -> let (ls,(x:rs)) = splitAt i xs in (x,ls++rs)) [0..n-1]
