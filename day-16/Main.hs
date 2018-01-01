{-# LANGUAGE TypeApplications #-}

import Text.Parsec
import Text.Parsec.String

import Data.List
import Data.Maybe
import Data.Function
import Control.Monad

main :: IO ()
main = do
  Right r <- parseFromFile p "input.txt"
  Right t <- parseFromFile t "tape.txt"
  -- print $ nub . sort . concatMap (map fst . snd) <$> r
  print . filter (((==) `on` hash) t . snd) $ r
  print . filter ((match `on` hash) t . snd) $ r

p = do { string "Sue "; n <- read @Int <$> many1 digit; string ": "
       ; xs <- prop `sepBy` (string ", ")
       ; return (n,xs) }
    `sepBy` newline

prop = do { name <- many1 letter
          ; string ": "
          ; value <- read @Int <$> many1 digit
          ; return (name,value) }

t = prop `sepBy` newline

mfcsam = ["akitas","cars","cats","children","goldfish"
         ,"perfumes","pomeranians","samoyeds","trees","vizslas"]

newtype Hash = H [Maybe Int] deriving Show

instance Eq Hash where
  (H xs) == (H ys) = null . filter not . catMaybes $ zipWith (liftM2 (==)) xs ys

hash :: [(String,Int)] -> Hash
hash = H . (`map` mfcsam) . assign
  where
    assign ps l = snd <$> find ((l==) . fst) ps

match (H xs) (H ys) = null . filter not . catMaybes $
  zipWith3 (liftM2 . comparison) mfcsam ys xs

comparison "cats"        = (>)
comparison "trees"       = (>)
comparison "pomeranians" = (<)
comparison "goldfish"    = (<)
comparison _             = (==)
