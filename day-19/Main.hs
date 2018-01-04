{-# LANGUAGE BangPatterns #-}
import Text.Parsec
import Text.Parsec.String

import Data.List
import qualified Data.Set as S

import Debug.Trace
import Control.Arrow

main = do
  Right (rs,m) <- parseFromFile p "input.txt"
  print . S.size . molecules m $ rs
  -- print . findIndex (S.member m) . take 8 . iterate (generate (length m) rs) $ S.singleton "e"
  print . findIndex (S.member "e")
        . traceShowId
        . iterate (ungenerate $ map swap rs)
        $ S.singleton m

swap (a,b) = (b,a)

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

generate n rs s = foldl' (f n s) S.empty rs

f n s s' (from,to) = S.union s' (S.fromList $ filter ((<= n) . length) . (`replace` (from,to)) =<< S.toList s)

ungenerate rs !s = S.fromList $ (\m -> replace m =<< rs) =<< S.toList s
