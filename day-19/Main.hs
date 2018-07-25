{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-do-bind
    -Wno-unused-imports -Wno-type-defaults #-}
import Text.Parsec
import Text.Parsec.String

import Data.List
import qualified Data.Set as S

import Data.Semigroup
import Data.Monoid

import Data.Ord

import Debug.Trace

main = do
  Right (rs,m) <- parseFromFile p "input.txt"
  -- print . S.size . molecules m $ rs
  let swap (a,b) = (b,a)
      rs' = reverse $ sortBy (comparing (length . fst)) $ swap <$> rs
  print $ steps m rs'

p = do { rs <- (do { r <- many1 letter ; string " => " ; l <- many1 letter
                   ; return (r,l)
                   } `endBy1` newline)
       ; newline ; m <- many1 letter ; eof
       ; return (rs, m)
       }

-- Part 1

molecules m = foldl' (\s -> S.union s . S.fromList . replace m) S.empty

replace m (from,to) = let
  is = filter (completeMatch m from) $ findIndices (head from ==) m
  n = length from
  replaceAt i = let (ls,rs) = splitAt i m in ls ++ to ++ drop n rs
  in replaceAt <$> is

completeMatch m from i = and $ zipWith (==) from (drop i m)

-- Part 2

-- 1. make a Set of all the possible substitutions of the current molecules (ms)
-- 2. [heuristic] keep a only number (40) of the shortest molecules (minLen')
-- 3. repeat until "e" appears in the Set
-- Funnily… it worked! Tried 10, 20, and at 40 it got there.
steps :: String -> [(String,String)] -> Int
steps m = go 0 (S.singleton m)
  where
    go n ms rs
      | S.null ms = -1
      | "e" `S.member` ms = n
      | otherwise = traceShow (n,length ms,length <$> S.toList ms) $ go (n+1) cs rs
        where cs' = S.fromList $! rs >>= \r -> S.toList ms >>= \m -> replace m r
              minLen' = getMin $ foldMap (Min . length) cs'
              cs = S.take 40 $ S.filter ((<= minLen') . length) cs'
