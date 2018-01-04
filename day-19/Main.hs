{-# LANGUAGE BangPatterns #-}
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
  print . f 0 0 rs' $ m


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

f n _ _ "e"    = Just [n]
f n i rs m
  | i == length rs = Just []
  | otherwise      = let
    r@(from,to) = rs !! i
    is = filter (completeMatch m from) $! findIndices (head from ==) m
    -- !_ = traceShowId r
    !_ = let foo = replace m r
             bar = length <$> foo
         in case length foo of
              0 -> []
              n -> case n < 21 of
                True -> []
                False -> traceShowId [(n,i,r,foo,length <$> foo)]
              -- case null $ filter (<60) bar of
              --        True -> []
              --        False -> traceShowId [(bar, foo)]
    in case null is of
      True -> f n (i+1) rs m
      False -> let cases = filter ((`elem` "eO") . head) $ replace m r
               in Just $ concat $ catMaybes $ f (n+1) 0 rs <$> cases
