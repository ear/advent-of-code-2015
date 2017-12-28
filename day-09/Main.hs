{-# LANGUAGE TypeApplications #-}
import Text.Parsec
import Text.Parsec.String

import Data.List
import Data.Maybe
import qualified Data.Map as M

main :: IO ()
main = do
  Right r <- parseFromFile p "input.txt"
  let cs    = cities r
      trips = permutations cs
      m     = M.fromList $ r ++ [ ((c2,c1),d) | ((c1,c2),d) <- r ]
      costs = map sum
            . catMaybes
            . map (sequence . map (`pairToDist` m) . tripToPairs)
            $ trips
  print . minimum $ costs

p = do { c1 <- many1 letter
       ; string " to "
       ; c2 <- many1 letter
       ; string " = "
       ; d <- read @Int <$> many1 digit
       ; return ((c1,c2),d)
       }
    `sepBy1` newline

cities xs = nub $ sort $ [ c1 | ((c1,_),_) <- xs ] ++ [ c2 | ((_,c2),_) <- xs ]

tripToPairs =
  map (\[x,y] -> (x,y)) . filter ((2==) . length) . (inits =<<) . tails

pairToDist pair m = M.lookup pair m
