{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-do-bind
    -Wno-unused-imports -Wno-type-defaults #-}
import Text.Parsec
import Text.Parsec.String

import Data.List
import qualified Data.Set as S

import Debug.Trace
import Control.Arrow
import Data.Maybe

main = do
  Right (rs,m) <- parseFromFile p "input.txt"
  print . S.size . molecules m $ rs
  let swap (a,b) = (b,a)
      rs' = swap <$> rs
  print . f 0 [] 0 rs' $ m


p = do { rs <- (do { r <- many1 letter ; string " => " ; l <- many1 letter
                   ; return (r,l)
                   } `endBy1` newline)
       ; newline ; m <- many1 letter ; eof
       ; return (rs, m)
       }

molecules m = foldl' (\s -> S.union s . S.fromList . replace m) S.empty

replace m (from,to) = let
  is = filter (completeMatch m from) $ findIndices (head from ==) m
  n = length from
  replaceAt i = let (ls,rs) = splitAt i m in ls ++ to ++ drop n rs
  in replaceAt <$> is

completeMatch m from i = and $ zipWith (==) from (drop i m)

f n ns _ _ "e"     = n:ns
f n ns i rs m
  | i == length rs = ns
  | otherwise      = let
    r@(from,to) = rs !! i
    !cases = replace m r
    !ns' = f (n+1) ns 0 rs <$> cases
    in concat ns' ++ f n ns (i+1) rs m

t _ [] = []
t n xs = traceShow (n,xs) xs
