{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-do-bind
    -Wno-unused-imports -Wno-type-defaults #-}
import Text.Parsec
import Text.Parsec.String

import Data.List
import qualified Data.Set as S

import Data.Ord
import Data.Maybe
import Data.Function

import Control.Arrow
import Control.Monad

import Debug.Trace

main = do
  Right (rs,m) <- parseFromFile p "input.txt"
  let swap (a,b) = (b,a)
      (from,to) = join (***) (nub . sort . concat) . unzip $ rs
      us = to \\ from -- unique letters to the productions
      (evil,good) = partition (any (`elem` us) . snd) rs
  putStrLn $ concat ["evil letters: ", us]
  putStrLn "Good:"
  -- mapM_ print good
  putStrLn "Evil:"
  -- let isEvil = any (`elem` us)
  let sevil = map swap . sortBy (comparing (negate . length . snd)) $ evil
  mapM_ print sevil

-- apply m (from,to) = case filter (completeMatch m from) $ findIndices (head from ==) m of
--  [] -> Nothing

p = do { rs <- (do { r <- many1 letter ; string " => " ; l <- many1 letter
                   ; return (r,l)
                   } `endBy1` newline)
       ; newline ; m <- many1 letter ; eof
       ; return (rs, m)
       }

replace m (from,to) = let
  is = filter (completeMatch m from) $ findIndices (head from ==) m
  n = length from
  replaceAt i = let (ls,rs) = splitAt i m in ls ++ to ++ drop n rs
  in replaceAt <$> is

completeMatch m from i
  | length m - i < length from = False
  | otherwise = and $ zipWith (==) from (drop i m)

search needle rs n m
  | needle == m = traceShowId $! [n]
  | otherwise = search needle rs (n+1) =<< (replace m =<< rs)

swap (a,b) = (b,a)
contract rs mol = replace mol =<< map swap rs

t = do
  Right (rs,m) <- parseFromFile p "test.txt"
  print . findIndex ("e" `elem`)
        . iterate (nub . sort . concatMap (contract rs)) $ [m]

classify rs = let
  (froms,tos) = join (***) (nub . sort . concat) . unzip $ rs
  uniques = tos \\ froms
  in (uniques,partition (any (`elem` uniques) . snd) rs)

expunge evils = unfoldr go
  where
    shortest = take 1 . sortBy (comparing length)
    contractions mol = filter (/= mol) . concat . zipWith (flip replace) evils . repeat $ mol
    go = fmap (id &&& id) . listToMaybe . shortest . contractions

t2 = do
  Right (rs,m) <- parseFromFile p "input.txt"
  let (_,(evil,good)) = classify rs
  print 1

