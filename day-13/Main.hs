{-# OPTIONS_GHC -Wno-type-defaults -Wno-missing-signatures
                -Wno-incomplete-patterns -Wno-unused-do-bind #-}

import Text.Parsec
import Text.Parsec.String

import Data.Ord
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M

import Control.Arrow

main :: IO ()
main = do
  Right r <- parseFromFile p "input.txt"
  let m = M.fromList r
      xs = nub . sort . map (fst . fst) $ r
      xss = permutations xs
  print . (happiness m &&& id) . maximumBy (comparing (happiness m)) $ xss

neighbors [a,b,c] = [(b,a),(b,c)]

happiness m xs = sum
               . map (fromJust . (`M.lookup` m))
               . concatMap neighbors
               . take (length xs) . map (take 3) . tails . cycle $ xs

p = do { n1 <- (:) <$> upper <*> many1 lower
       ; string " would "
       ; gl <- try (string "gain") <|> string "lose"
       ; space
       ; n <- read <$> many1 digit
       ; string " happiness units by sitting next to "
       ; n2 <- (:) <$> upper <*> many1 lower
       ; char '.'
       ; let n' = case gl of "gain" -> n; "lose" -> (-n)
       ; return ((head n1,head n2),n') }
    `sepBy`
    newline
