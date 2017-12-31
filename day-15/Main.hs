{-# LANGUAGE TypeApplications #-}

import Data.List
import Control.Arrow

main :: IO ()
main = do
  -- m <- parse <$> readFile "test.txt"
  (m,m') <- parse <$> readFile "input.txt"
  mapM_ print m
  print m'
  -- print . score m $ [44,56]
  print . maximum . map (score m) $ domain
  print . maximum . map (score m) . filter (cc m') $ domain

parse = first transpose
      . unzip
      . map (init &&& last)
      . map (map (read @Int) . words)
      . lines
      . filter (`elem` " \n-0123456789")

-- domain = [ [x,100-x] | x <- [0..100] ]

domain = [ [x,y,z,w]
         | x <- [0..100      ]
         , y <- [0..100-x    ]
         , z <- [0..100-(x+y)]
         , let w = 100-(x+y+z) ]

score m = product
        . map (\x -> if x < 0 then 0 else x)
        . map sum
        . zipWith (zipWith (*)) m
        . replicate (length m)

cc m' = (500==) . sum . zipWith (*) m'
